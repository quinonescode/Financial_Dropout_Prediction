# Date/time: 10/23/2024 at 1:27 pm
# Ticket  - Assessment Council - Goal for Today: Develop a theory of what financial stability factors predict drop-out?

# Employment status, payment on time, federal subsidies (pell, welfare, free lunch)
# More recent analysis for the past 3-5 years 

# The right Statistical test: 
# Nature of the variables: They are categorical
# Research question: Are you interested in association, causation. or difference? 
# Assumptions about the data: is it normally distributed, have equal variables, and have outliers? 




###########################################################################################################

#Lets open the libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(utils) 
library(sqldf) #for SQL
library(lubridate) #for age and time 
library(formattable) #for fancy tables
library(readxl) #for reading Excel tables 
#install.packages("patchwork")
library(patchwork) # To display 2 charts together
#install.packages("shiny")
library(shiny) #for the multi level table 
library(reshape2) #need this too to make the multi level table 
library(scales) # percentage labels
###########################################################################################################



#######################Drop outs for the year 8/29/2022 - 8/28/2023

Fall24<-read_excel('H:/Research/Assessment Council/Financial Predict Dropout/data/raw/ReUp Files/LCCC Newly Eligible Fall 2024 v2.xlsx')
Summer24 <- read_excel("H:/Research/Assessment Council/Financial Predict Dropout/data/raw/ReUp Files/LCCC Newly Eligible Summer 2024 ReUp File v2.xlsx")
Spring24<- read_excel("H:/Research/Assessment Council/Financial Predict Dropout/data/raw/ReUp Files/LCCC ReUp Newly Eligible Spring 2024 Rev 3.xlsx")
Fall23 <- read_csv('H:/Research/Assessment Council/Financial Predict Dropout/data/raw/ReUp Files/LCCC_Newly_Eligible_10_31_2023.csv')


#Levels don't match. 
names(Fall24)

Fall24<- select (Fall24, c("Student ID" ,
                           "Current Balance",
                           "Original Drop Date",
                           "Original Start Date",
                           "Current Holds",
                           "Academic Standing"))


names(Summer24)
Summer24<- select(Summer24,  c("Student ID" ,
                               "Current Balance",
                               "Original Drop Date",
                               "Original Start Date",
                               "Current Holds",
                               "Academic Standing"))

names(Spring24)
Spring24 <- select(Spring24, c( "Student ID" ,
                                "Current Balance",
                                "Original Drop Date",
                                "Original Start Date",
                                "Current Holds",
                                "Academic Standing"))

names(Fall23)
Fall23<- select (Fall23,c ( "SPRIDEN_ID",
                            "Balance Owed",
                            "Enrollment End",
                            "Enrollment Begin",
                            "STVHLDD_DESC" ,
                            "ASTD_CODE_END_OF_TERM"))



Fall23<-Fall23 %>%
  rename( 
    "Student ID"= "SPRIDEN_ID",
    "Current Balance"="Balance Owed" ,
    "Original Drop Date"="Enrollment End" ,
    "Original Start Date"="Enrollment Begin",
    "Current Holds"="STVHLDD_DESC" ,
    "Academic Standing"="ASTD_CODE_END_OF_TERM"
  )


dfReUp <- rbind(Fall24,
                Summer24,
                Spring24,
                Fall23)

names(dfReUp)

dfReUp<-dfReUp %>%
  rename( 
    "Student_ID"= "Student ID",
    "Current_Balance"="Current Balance" ,
    "Original_Drop_Date"="Original Drop Date" ,
    "Original_Start_Date"="Original Start Date",
    "Current_Holds"="Current Holds",
    "Academic_Standing"="Academic Standing"
  )

dfReUp$Status <- c("Dropout")

#############################Matching student population   8/29/2022 - 8/28/2023

#Borrowed these argos files from a different ticket. Because Argos was not working today
all202340 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202340.csv')
all202240 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202240.csv')
all202250 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202250.csv')
all202210 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202210.csv')
all202220 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202220.csv')

df <- rbind(all202340,
            all202240,
            all202250 ,
            all202210,
            all202220)

names(df)




######################### Clean Df

#I want to remove the students who are in the dfReup from the df population
#dropout =1, not dropout =0
dfall <- df %>% 
  mutate(dropout = ifelse(
    Student_ID %in% dfReUp$Student_ID, 1, 0 ))

#unduplicate 
#dfall <- dfall %>% distinct(Student_ID, .keep_all = TRUE)
#dfall$dropout <- as.factor(dfall$dropout)

#Remove High School Students and Guest


dfall <- dfall[dfall$Student_Type_Code != "P" & 
                 dfall$Student_Type_Code != "G"  , ]
dfall <- dfall %>% 
  mutate_if(sapply(dfall, is.character), as.factor)


summary(dfall$Student_Type_Code)

#Im pretty sure this is the correct number of students that have dropped out, because the original has dates starting in the 1970s
summary(dfall$dropout)

#Full-time or Part-Time
summary(dfall$Registered_Credits)


dfall <- dfall %>%
  group_by(Student_ID, Reg_Term_Code) %>%
  mutate(total_credits = sum(Registered_Credits, na.rm = TRUE))%>%
  mutate(FPT= ifelse(Registered_Credits >= 12, "Full-Time", "Part-Time"))



#Summarize again so that if the student was full-time at any time, then they are marked Full time 
dfall <- dfall  %>%
  group_by(Student_ID) %>%
  mutate(OverallFPT=
           ifelse(any(FPT== "Full-Time")==TRUE, "Full-Time",
                  "Part-Time"))%>% 
  mutate(OverallFPT=factor(OverallFPT))

summary(dfall$OverallFPT)
#We only need a few columns 
dflim<- select (dfall,c ( "PIDM", "Student_ID", "Registered_Credits",
                          "dropout", "OverallFPT"))

#download variable 
hold<-read_excel('H:/Research/Assessment Council/Financial Predict Dropout/data/raw/BBHold.xlsx')

names(hold)

#Create a new variable for students that are mixed dependent and independent 

hold <- hold %>% 
  group_by(PIDM) %>%
  mutate(Hold= "Hold")


dfhold <-data.frame ( 
  sqldf("
        SELECT
        a.PIDM, 
        a.dropout,
        a.OverallFPT,
        a.Registered_Credits,
        b.Hold
        FROM dflim a
        LEFT JOIN hold b
        ON a.PIDM = b.PIDM
        "))
dfhold <- dfhold %>% 
  mutate(Hold= replace_na(Hold, "Hold"))

partdflim <- dfhold %>%
  filter( Hold == "No Hold") %>%
  mutate(Hold = factor(Hold))%>%
  group_by(PIDM)%>%
  slice_max(Registered_Credits, with_ties = FALSE)


##################### ##################### ##################### ##################### ##################### 

#Logistic Regression on Course Load
#So the idea is that with more course load (higher number of credits) it becomes more likely that the student will dropout
summary(partdflim)


#Visualize the data 
ggplot(partdflim, aes(x= Registered_Credits,
                  y=dropout))+
  geom_jitter(height=.05,
              alpha =.1)+ 
  geom_smooth(method= "glm",
              method.args = list(family = "binomial"),
              se = FALSE)




