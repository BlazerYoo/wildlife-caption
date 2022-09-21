# DATA CLEANING

library(tidyverse)

# read in dataset: no_duplicate_dataset.csv
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/mh-data-analysis/data/no_duplicate_dataset.csv?token=GHSAT0AAAAAABYYWKV3LPOLJP7WX4HGMXHYYZLHJRA')

# define variables for columns 
age <- dataset$`What.is.your.age.`
gender <- dataset$`To.which.gender.identity.do.you.most.identify.`
ethnicity <- dataset$`Please.specify.your.ethnicity.`
education <- dataset$`What.is.the.highest.degree.or.level.of.school.you.have.completed..If.currently.enrolled..what.is.your.highest.degree.received.`
treatment <- dataset$`Treatment.shown`

## demographics for whole dataset
table(gender) # gender
hist(age) # age
hist(ethnicity) # ethnicity
table(education) # education

## split into 4 datasets based on image
image1 = dataset[which (treatment == 'Image1'), ]
image2 = dataset[which (treatment == 'Image2'), ]
image3 = dataset[which (treatment == 'Image3'), ]
image4 = dataset[which (treatment == 'Image4'), ]

## demographics based on image
# age
boxplot(age~treatment) 
# ethnicity
eth_counts <- table(ethnicity, treatment)
barplot(eth_counts, main = "Ethnicity vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(eth_counts), beside=TRUE) 
# gender
gender_counts <- table(gender, treatment)
barplot(gender_counts, main = "Gender vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(gender_counts), beside=TRUE)
# education
edu_counts <- table(education, treatment)
barplot(edu_counts, main = "Education vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(edu_counts), beside=TRUE)
