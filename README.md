

[Financial Predict Dropout.pptx](https://github.com/user-attachments/files/18606994/Financial.Predict.Dropout.pptx)

# Financial Dropout Prediction: A Behavioral Risk Study
**Quantitative Analysis of Student Retention & Financial Stability Factors**

##  Project Overview
The objective of this study was to develop a theoretical and predictive model to identify which financial stability factors most significantly correlate with student attrition at a community college. By analyzing the intersection of funding sources, employment indicators, and payment behaviors, this project provides a data-driven framework for proactive student retention interventions.

---

##  The Problem
Community colleges face unique challenges in student retention, often tied to "life friction"—specifically financial volatility. This project sought to answer: *Which financial indicators serve as the most reliable "red flags" for a student's likelihood to drop out?*

**Time Frame:** Fall 2022 – Fall 2023  
**Population:** All enrolled students at Lehigh Carbon Community College (LCCC).

---

##  Methodology & Variables
This study utilized **SQL** for data extraction and cohort definition, with **R** for statistical analysis and correlation testing.

### **Independent Variables Analyzed:**
* **Funding Model:** PELL Grant eligibility and Scholarship status.
* **Employment/Social Status:** Dependency status as a proxy for financial support systems.
* **Payment Behavior:** "On-time" vs. "Delinquent" status based on **Business Office Holds** (a key behavioral indicator).
* **Enrollment Intensity:** Full-time vs. Part-time status.

---

## Key Findings
The analysis yielded critical insights into the "pathway to dropout," debunking some common assumptions while reinforcing others:

1. **The "Payment Behavior" Signal:** There is a statistically significant correlation between **Business Office Holds** (payment delinquency) and the dropout rate across the entire student population. 
2. **Part-Time Vulnerability:** The correlation between financial holds and attrition was particularly pronounced among **Part-Time students**, suggesting lower financial "buffers" for this demographic.
3. **Dependency Neutrality:** Contrary to initial theories, **Dependency Status** (Independent vs. Dependent) showed no significant correlation with dropout rates, suggesting that family support proxies are less predictive than direct financial behavior.

---

##  Tech Stack
* **SQL:** Data mining, joining disparate tables (Registrar vs. Business Office), and cleaning enrollment records.
* **R:** Correlation analysis, Chi-square testing, and data visualization.
* **PowerPoint:** Synthesis of findings for executive-level institutional stakeholders.

---

##  Repository Contents
* `Presentation/`: Final Executive PowerPoint summarizing findings and strategic recommendations.
* `Scripts/`: (In Progress) R scripts for statistical testing and SQL queries used for data extraction.

---
*This research serves as a blueprint for "Early Warning Systems" in institutional settings, shifting from reactive to proactive student support based on behavioral financial data.*
