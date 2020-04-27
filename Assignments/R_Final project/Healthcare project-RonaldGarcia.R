#Healthcare cost analysis
#< RONALD GARCIA >
#Analysis to be done: 
  
#1. To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and 
#   has the maximum expenditure.
#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
#   the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
#3. To make sure that there is no malpractice, the agency needs to analyze if the race of 
#   the patient is related to the hospitalization costs.
#4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs 
#   by age and gender for the proper allocation of resources.
#5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay 
#   can be predicted from age, gender, and race.
#6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.

setwd("C:/Ron/Architecture/AI/AI Engineer/Data Science with R/Project/Projects for Submission/Healthcare/Healthcare")
getwd()

hospital_cost <- read.csv("HospitalCosts.csv", header = TRUE)

#Data structure
View(hospital_cost)
str(hospital_cost)
summary(hospital_cost)

#Analysis
attach(hospital_cost)

#Age
hist(AGE)

#Age categories
age_factor <- as.factor(AGE)
summary(age_factor)
tapply(TOTCHG,AGE,sum)
#Maximum expenditure
which.max(tapply(TOTCHG,AGE,sum))

#Diagnosis-related group
diag_group <- as.factor(APRDRG)
summary(diag_group)
which.max(summary(diag_group))
tapply(TOTCHG,diag_group,sum)
which.max(tapply(TOTCHG,diag_group,sum))
#The category 640 has the maximum expenditure
max(tapply(TOTCHG,diag_group,sum))


#Anova is used for one-way analysis of variance
race_factor <- as.factor(RACE)
summary(race_factor)
#Omit NA values
hospital_cost_modified <- na.omit(hospital_cost)
anova_one_way <- aov(TOTCHG~RACE)
summary(anova_one_way)
#pvalue 68%  - There is no relation between the race of patient and the hospital cost

#Linear regression is used to find the relationship between factors
model_gender <- lm(TOTCHG~AGE+FEMALE)
summary(model_gender)
#Female negative coeficient - Females incur in lower costs 
model_gender_race <- lm(LOS~AGE+FEMALE+RACE)
summary(model_gender_race)
#p-value is high - There is no relationship between the given variables
model_complete_analysis <- lm(TOTCHG~ .,data=hospital_cost_modified) 
summary(model_complete_analysis)
#Age and length of stay affect the total hospital cost
