# Date/time: 10/14/2024 at 9:18 am
# Ticket  - Assessment Council - Goal for Today: Develop a theory of what financial stability factors predict drop-out?

# Employment status, payment on time, federal subsidies (pell, welfare, free lunch)
# More recent analysis for the past 3-5 years 

# The right Statistical test: 
# Nature of the variables: They are categorical
# Research question: Are you interested in association, causation. or difference? 
# Assumptions about the data: is it normally distributed, have equal variables, and have outliers? 


#Independent vs dependent students 




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
dfall <- df %>% 
  mutate(dropout = ifelse(
    Student_ID %in% dfReUp$Student_ID, "Dropout", "Not Dropout" ))

#unduplicate 
dfall <- dfall %>% distinct(Student_ID, .keep_all = TRUE)
dfall$dropout <- as.factor(dfall$dropout)

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

summary(dfall$FPT)


#Summarize again so that if the student was full-time at any time, then they are marked Full time 
dfall <- dfall  %>%
  group_by(Student_ID) %>%
  mutate(OverallFPT=
           ifelse(any(FPT== "Full-Time")==TRUE, "Full-Time",
                  "Part-Time"))%>% 
  mutate(OverallFPT=factor(OverallFPT))

summary(dfall$OverallFPT)


#We only need a few columns 
dflim<- select (dfall,c ( "PIDM", "Student_ID",
                            "dropout", "OverallFPT"))



#################################################
########### Pell 
#download variable 
pell<-read_excel('H:/Research/Assessment Council/Financial Predict Dropout/data/raw/Pell.xlsx')

#unduplicate
pell <- pell %>% distinct(SPRIDEN_ID, .keep_all = TRUE)

#Add it to the df
dfPELL <- dflim %>% 
  mutate(PELL = ifelse(
    Student_ID %in% pell$SPRIDEN_ID, "PELL", "No PELL" ))

#make the variables factors 
dfPELL <- dfPELL %>% 
  mutate( PELL=factor(PELL))

#View the summary 
summary(dfPELL)

#chi squared
FullPell <- dfPELL %>% 
  filter(OverallFPT =="Full-Time")
PartPell <- dfPELL %>% 
  filter(OverallFPT == "Part-Time")

chisq.test(table(dfPELL$dropout, dfPELL$PELL))
chisq.test(table(FullPell$dropout, FullPell$PELL))
chisq.test(table(PartPell$dropout, PartPell$PELL))

#The p-value is less than 0.1,  NOT statistically significant
#We can conclude that student dropout rate is not correlated to PELL
table(dfPELL$dropout, dfPELL$PELL)


#View the table & renaming
Pellgraph <- as.data.frame(table(dfPELL$dropout, dfPELL$PELL)) %>%
  rename("Status" = "Var1", "Aid" = "Var2", "Count"= "Freq")

#Graph 
ggplot(Pellgraph, aes(Aid, Count, fill=Status))+ 
  geom_bar(stat="identity", position = 'dodge')

##################### FULL TIME --- 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perpell <- dfPELL %>% 
  filter(OverallFPT=="Full-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(PELL = factor(PELL))%>%
  group_by(dropout, PELL) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perpell, aes(fill= PELL, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perpell$PELL == "PELL", 1, 0 ),
            position = position_fill(vjust=0.5))

##################### PART TIME --- 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perpell <- dfPELL %>% 
  filter(OverallFPT=="Part-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(PELL = factor(PELL))%>%
  group_by(dropout, PELL) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perpell, aes(fill= PELL, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perpell$PELL == "No PELL", 1, 0 ),
            position = position_fill(vjust=0.5))



##################### everyone ----- 100% stacked bar chart 
#First calculate percentages and put into a dataframe
perpell <- dfPELL %>% 
  mutate(dropout = factor(dropout)) %>% mutate(PELL = factor(PELL))%>%
  group_by(dropout, PELL) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perpell, aes(fill= PELL, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perpell$PELL == "No PELL", 1, 0 ),
            position = position_fill(vjust=0.5))
#################################################
########### Dependency  
#download variable 
dependency<-read_excel('H:/Research/Assessment Council/Financial Predict Dropout/data/raw/Dependency.xlsx')

names(dependency)

#Create a new variable for students that are mixed dependent and independent 

dependency <- dependency %>% 
  group_by(RCRAPP1_PIDM) %>%
  mutate(Dependency=
           ifelse(all(RCRAPP2_MODEL_CDE == "I")==TRUE, "Independent", "Dependent"))


dependency<- dependency %>%
  distinct(RCRAPP1_PIDM, .keep_all = TRUE)

#sql for matching this one 
names(dependency)
names(dflim)

dfdependency <-data.frame ( 
  sqldf("
        SELECT
        a.PIDM, 
        a.dropout,
        a.OverallFPT,
        b.Dependency
        FROM dflim a
        LEFT JOIN dependency b
        ON a.PIDM = b.RCRAPP1_PIDM
        "))

      
#make the variables factors 
dfdependency <- dfdependency %>% 
  mutate_if(sapply(dfdependency, is.character), as.factor)

#View the summary 
summary(dfdependency)

#There are 2047 students for which we dont have the status
# This could be that they did not file for financial aid, so we did not record this answer

#chi squared
FullDep <- dfdependency %>% 
  filter(OverallFPT =="Full-Time")
PartDep <- dfdependency %>% 
  filter(OverallFPT == "Part-Time")

chisq.test(table(FullDep$dropout, FullDep$Dependency))
chisq.test(table(PartDep$dropout, PartDep$Dependency))
chisq.test(table(dfdependency$dropout, dfdependency$Dependency))
#The p-value is 0.8,  NOT statistically significant
#We can conclude that student dropout rate is not correlated to dependency status
table(dfdependency$dropout, dfdependency$Dependency)



#View the table & renaming
Depgraph <- as.data.frame(table(dfdependency$dropout, dfdependency$Dependency)) %>%
  rename("Status" = "Var1", "Dependency" = "Var2", "Count"= "Freq")

#Graph 
ggplot(Depgraph, aes(Dependency, Count, fill=Status))+ 
  geom_bar(stat="identity", position = 'dodge')


##################### Full-Time- 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perdep <- dfdependency %>% 
  filter(OverallFPT == "Full-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(Dependency = factor(Dependency))%>%
  drop_na()%>%
  group_by(dropout, Dependency) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perdep, aes(fill= Dependency, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha = ifelse(perdep$Dependency == "Dependent", 1, 0 ) ,
            position = position_fill(vjust=0.5))

##################### 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perdep <- dfdependency %>% 
  filter(OverallFPT == "Part-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(Dependency = factor(Dependency))%>%
  drop_na()%>%
  group_by(dropout, Dependency) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perdep, aes(fill= Dependency, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha = ifelse(perdep$Dependency == "Independent", 1, 0 ) ,
            position = position_fill(vjust=0.5))


##################### 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perdep <- dfdependency %>% 
  mutate(dropout = factor(dropout)) %>% mutate(Dependency = factor(Dependency))%>%
  drop_na()%>%
  group_by(dropout, Dependency) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perdep, aes(fill= Dependency, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha = ifelse(perdep$Dependency == "Dependent", 1, 0 ) ,
            position = position_fill(vjust=0.5))
#################################################
########### BB Hold  
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
        b.Hold
        FROM dflim a
        LEFT JOIN hold b
        ON a.PIDM = b.PIDM
        "))

dfhold <- dfhold %>% 
  mutate(Hold= replace_na(Hold, "No Hold"))


#make the variables factors 
dfhold <- dfhold %>% 
  mutate_if(sapply(dfhold, is.character), as.factor)

#View the summary 
summary(dfhold)

#There are 2047 students for which we dont have the status
# This could be that they did not file for financial aid, so we did not record this answer

#chi squared
Fullhold <- dfhold %>% 
  filter(OverallFPT =="Full-Time")
Parthold <- dfhold %>% 
  filter(OverallFPT == "Part-Time")

chisq.test(table(Fullhold$dropout, Fullhold$Hold))
chisq.test(table(Parthold$dropout, Parthold$Hold))
chisq.test(table(dfhold$dropout, dfhold$Hold))
#The p-value is < .00001  HIGHLY statistically significant
#We can conclude that student dropout rate is correlated to BB office holds
table(dfhold$dropout, dfhold$Hold)



#View the table & renaming
Holdgraph <- as.data.frame(table(dfhold$dropout, dfhold$Hold)) %>%
  rename("Status" = "Var1", "Hold" = "Var2", "Count"= "Freq")

#Graph 
ggplot(Holdgraph, aes(Hold, Count, fill=Status))+ 
  geom_bar(stat="identity", position = 'dodge')

##################### Full-Time -- 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perhold <- dfhold %>% 
  filter(OverallFPT == "Full-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(Hold = factor(Hold))%>%
  group_by(dropout, Hold) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perhold, aes(fill= Hold, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perhold$Hold == "Hold", 1, 0 ),
            position = position_fill(vjust=0.5))

##################### 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perhold <- dfhold %>% 
  filter(OverallFPT == "Part-Time")%>%
  mutate(dropout = factor(dropout)) %>% mutate(Hold = factor(Hold))%>%
  group_by(dropout, Hold) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perhold, aes(fill= Hold, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perhold$Hold == "Hold", 1, 0 ),
            position = position_fill(vjust=0.5))


##################### 100% stacked bar chart 

#First calculate percentages and put into a dataframe
perhold <- dfhold %>% 
  mutate(dropout = factor(dropout)) %>% mutate(Hold = factor(Hold))%>%
  group_by(dropout, Hold) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n /sum(n))%>% 
  mutate(per=label_percent()(freq))#from the scales package

ggplot(perhold, aes(fill= Hold, y=per, x=dropout  ))+
  geom_bar(position = "stack", stat='identity')+
  geom_text(aes(label = per),
            alpha =ifelse(perhold$Hold == "Hold", 1, 0 ),
            position = position_fill(vjust=0.5))


##################### ##################### ##################### ##################### ##################### 

#Logistic Regression on Course Load
#So the idea is that with more course load (higher number of credits) it becomes more likely that the student will dropout




#Visualize the data 





