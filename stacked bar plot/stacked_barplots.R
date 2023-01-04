### DATA FOR NEEP

library(ggplot2)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)


# read in dataset: no_duplicate_dataset.csv
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB4XGWTCLAGWUT7NMAWCUGBEY5V46FQ')

# treatment: image #
## image 1: mountain gorilla w/o captions
## image 2: mountain gorilla w/captions
## image 3: loris w/o captions
## image 4: loris w/captions
gorila_treatment_labels <- c("Image1", "Image2")
loris_treatment_labels <- c("Image3", "Image4")
gorilla_data <- dataset[dataset$Treatment.shown %in% gorila_treatment_labels,]
loris_data <- dataset[dataset$Treatment.shown %in% loris_treatment_labels,]
gorilla_treatments <- gorilla_data$Treatment.shown
loris_treatments <- loris_data$Treatment.shown


X...This.post.depicts.wildlife.research.
X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
X...This.animal.would.make.a.good.pet.
X...I.would.like.to.have.this.animal.as.a.pet.

### GORILLA
## QUESTION 1: I would like to have this animal as a pet.
# pet conclusion: captioning doesn't seem to matter
pet <- gorilla_data$X...I.would.like.to.have.this.animal.as.a.pet.
pet_df<- data.frame(pet, gorilla_treatments)
pet_df$pet <- paste("Score", pet_df$pet, sep="_")
pet_df

# Normalized Barplot
# (for raw count, position="stack")
jpeg("pet.jpg", units="in", width=6, height=5.7, res=300)
ggplot(pet_df, aes(fill=pet, x=gorilla_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  scale_x_discrete(labels=c("Gorilla Control", "Gorilla Test")) +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would like to have this animal as a pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
