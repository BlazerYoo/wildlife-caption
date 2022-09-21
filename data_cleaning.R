# DATA CLEANING
## ggplot
## fix legends, color palette 
## increase y axis

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
barplot(eth_counts, main = "Ethnicity vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(eth_counts), beside=TRUE) 
# gender
gender_counts <- table(gender, treatment)
barplot(gender_counts, main = "Gender vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", beside=TRUE)
# education
edu_counts <- table(education, treatment)
barplot(edu_counts, main = "Education vs Treatment Shown", xlab = "Treatment Shown", ylab ="number of respondents", 
        legend = rownames(edu_counts), beside=TRUE)


## question analysis:
# image 1: mountain gorilla w/o captions
# image 2: mountain gorilla w/captions
# image 3: loris w/o captions
# image 4: loris w/captions

# pet: I would like to have this animal as a pet.
# captioning doesn't seem to matter
pet <- dataset$`X...I.would.like.to.have.this.animal.as.a.pet.`
mode <- function(pet){which.max(tabulate(pet))}
summary(pet)
mode(pet)

pet_ct <- table(pet, treatment)
barplot(pet_ct, legend = rownames(pet_ct))

# This animal would make a good pet
gpet <- dataset$`X...This.animal.would.make.a.good.pet.`
summary(gpet)
gpet_ct <- table(gpet, treatment)
barplot(gpet_ct)

# This animal is an endangered species.-> caption doesn't make a difference
species <- dataset$`X...This.animal.is.an.endangered.species.`
species_ct <- table(species, treatment)
barplot(species_ct)

time <- dataset$`Duration..in.seconds.`
boxplot(time~treatment, outline=FALSE)
summary(aov(time~treatment))
