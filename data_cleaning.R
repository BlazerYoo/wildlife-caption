# DATA CLEANING

library(tidyverse)

# read in dataset: no_duplicate_dataset.csv
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB3SK6A7R4Q3PUVTBOATPMCKY4CO2WQ')

# define variables for columns 
age <- dataset$`What.is.your.age.`
gender <- dataset$`To.which.gender.identity.do.you.most.identify.`
ethnicity <- dataset$`Please.specify.your.ethnicity.`
education <- dataset$`What.is.the.highest.degree.or.level.of.school.you.have.completed..If.currently.enrolled..what.is.your.highest.degree.received.`
treatment <- dataset$`Treatment.shown`


## demographics for whole dataset
# gender
gender_all <- table(gender) 
barplot(gender_all, main = "Gender Distribution", xlab = "Gender", ylab ="number of respondents", 
        legend = rownames(gender_all), beside=TRUE)
# age
hist(age) 
# ethnicity
eth_all <- table(ethnicity)
barplot(eth_all, main = "Ethnicity Distribution", xlab = "Ethnicity", ylab ="number of respondents", 
                   legend = rownames(eth_all), beside=TRUE)
# education
edu_all <- table(education)
barplot(edu_all, main = "Education Distribution", xlab = "Education", ylab ="number of respondents", 
        legend = rownames(edu_all), beside=TRUE)


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
# encode ethnicity labels
rownames(eth_counts)[rownames(eth_counts) == "1"] = "Caucasian"
rownames(eth_counts)[rownames(eth_counts) == "2"] = "Hispanic or Latino"
rownames(eth_counts)[rownames(eth_counts) == "3"] = "Black or African American"
rownames(eth_counts)[rownames(eth_counts) == "4"] = "Native American or American Indian"
rownames(eth_counts)[rownames(eth_counts) == "5"] = "Asian / Pacific Islander"
rownames(eth_counts)[rownames(eth_counts) == "6"] = "Other / Mutliple"
rownames(eth_counts)[rownames(eth_counts) == "7"] = "Prefer not to say"
barplot(eth_counts, main = "Ethnicity vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(eth_counts), args.legend = list(x = "topright",
                                                          inset = c(-0.55, -0.3)),
        beside=TRUE) 
# gender
gender_counts <- table(gender, treatment)
barplot(gender_counts, main = "Gender vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(gender_counts), beside=TRUE)
# education
edu_counts <- table(education, treatment)
barplot(edu_counts, main = "Education vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(edu_counts), beside=TRUE)
