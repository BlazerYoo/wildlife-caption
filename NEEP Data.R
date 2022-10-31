### DATA FOR NEEP

library(ggplot2)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)


# read in dataset: no_duplicate_dataset.csv
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/mh-data-analysis/data/no_duplicate_dataset.csv?token=GHSAT0AAAAAAB2JTYKHFJZXKGAONIH6CJ3WY2VYLCQ')

# treatment: image #
## image 1: mountain gorilla w/o captions
## image 2: mountain gorilla w/captions
## image 3: loris w/o captions
## image 4: loris w/captions
treatment <- dataset$`Treatment.shown`


## QUESTION 1: I would like to have this animal as a pet.
# pet conclusion: captioning doesn't seem to matter
pet <- dataset$`X...I.would.like.to.have.this.animal.as.a.pet.`
pet_df<- data.frame(pet, treatment)
pet_df$pet <- paste("Score", pet_df$pet, sep="_")
pet_df

# Normalized Barplot
# (for raw count, position="stack")
jpeg("pet.jpg", units="in", width=6, height=5.7, res=300)
ggplot(pet_df, aes(fill=pet, x=treatment)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  scale_x_discrete(labels=c("Gorilla Control", "Gorilla Test", "Loris Control", "Loris Test")) +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would like to have this animal as a pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


## QUESTION 2: This animal is an endangered species.
# endangered conclusion: caption doesn't make a difference
species <- dataset$`X...This.animal.is.an.endangered.species.`
endangered_df<- data.frame(species, treatment)
endangered_df$species <- paste("Score", endangered_df$species, sep="_")

# Normalized Barplot
jpeg("endangered.jpg", units="in", width=6, height=5.7, res=300)
ggplot(endangered_df, aes(fill=species, x=treatment)) + 
  geom_bar(stat="count", position="fill") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  scale_x_discrete(labels=c("Gorilla Control", "Gorilla Test", "Loris Control", "Loris Test")) +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"This animal is an endangered species.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
