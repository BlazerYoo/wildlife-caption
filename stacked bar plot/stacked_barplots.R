library(ggplot2)
library(ggpubr)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)


# read in dataset: no_duplicate_dataset.csv
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB4XGWTCRGDK7ISHDTXE2FEWY5WFHNA')

# treatment: image #
## image 1: mountain gorilla w/o captions
## image 2: mountain gorilla w/captions
## image 3: loris w/o captions
## image 4: loris w/captions
gorila_treatment_labels <- c("Image1", "Image2")
loris_treatment_labels <- c("Image3", "Image4")
gorilla_data <- dataset[dataset$Treatment.shown %in% gorila_treatment_labels,]
loris_data <- dataset[dataset$Treatment.shown %in% loris_treatment_labels,]

# rename column values
gorilla_data$Treatment.shown = ifelse(gorilla_data$Treatment.shown == "Image1", "Gorilla Control", "Gorilla Test")
loris_data$Treatment.shown = ifelse(loris_data$Treatment.shown == "Image3", "Loris Control", "Loris Test")
gorilla_treatments <- gorilla_data$Treatment.shown
loris_treatments <- loris_data$Treatment.shown
gorilla_data
loris_data


#X...This.post.depicts.wildlife.research.
#X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
#X...This.animal.would.make.a.good.pet.
#X...I.would.like.to.have.this.animal.as.a.pet.


#-------------------------------------------------------------------------------------------------------------------------------------------------


### GORILLA
## QUESTION 1: This post depicts wildlife research.
research <- gorilla_data$X...This.post.depicts.wildlife.research.
research_df<- data.frame(research, gorilla_treatments)
research_df$research <- paste("Score", research_df$research, sep="_")
research_df

# Normalized Barplot
# (for raw count, position="stack")
research_plot <- ggplot(research_df, aes(fill=research, x=gorilla_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"This post depicts wildlife research.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gorilla_research.jpg")
dev.off()



### GORILLA
## QUESTION 2: I would seek out an experience to personally interact with this animal.
interact <- gorilla_data$X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
interact_df<- data.frame(interact, gorilla_treatments)
interact_df$interact <- paste("Score", interact_df$interact, sep="_")
interact_df

# Normalized Barplot
# (for raw count, position="stack")
interact_plot <- ggplot(interact_df, aes(fill=interact, x=gorilla_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would seek out an experience to \n personally interact with this animal.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gorilla_interact.jpg")
dev.off()



### GORILLA
## QUESTION 3: This animal would make a good pet.
good_pet <- gorilla_data$X...This.animal.would.make.a.good.pet.
good_pet_df<- data.frame(good_pet, gorilla_treatments)
good_pet_df$good_pet <- paste("Score", good_pet_df$good_pet, sep="_")
good_pet_df

# Normalized Barplot
# (for raw count, position="stack")
good_pet_plot <- ggplot(good_pet_df, aes(fill=good_pet, x=gorilla_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"This animal would make a good pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gorilla_good_pet.jpg")
dev.off()



### GORILLA
## QUESTION 4: I would like to have this animals as a pet.
have_pet <- gorilla_data$X...I.would.like.to.have.this.animal.as.a.pet.
have_pet_df<- data.frame(have_pet, gorilla_treatments)
have_pet_df$have_pet <- paste("Score", have_pet_df$have_pet, sep="_")
have_pet_df

# Normalized Barplot
# (for raw count, position="stack")
have_pet_plot <- ggplot(have_pet_df, aes(fill=have_pet, x=gorilla_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would like to have this animal as a pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gorilla_have_pet.jpg")
dev.off()



figure <- ggarrange(research_plot, interact_plot, good_pet_plot, have_pet_plot,
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)

figure <- annotate_figure(
  figure,
  top = text_grob("Gorilla Posts", face = "bold", size = 14)
)

figure


#-------------------------------------------------------------------------------------------------------------------------------------------------


### LORIS
## QUESTION 1: This post depicts wildlife research.
research <- loris_data$X...This.post.depicts.wildlife.research.
research_df<- data.frame(research, loris_treatments)
research_df$research <- paste("Score", research_df$research, sep="_")
research_df

# Normalized Barplot
# (for raw count, position="stack")
research_plot <- ggplot(research_df, aes(fill=research, x=loris_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"This post depicts wildlife research.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("loris_research.jpg")
dev.off()



### LORIS
## QUESTION 2: I would seek out an experience to personally interact with this animal.
interact <- loris_data$X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
interact_df<- data.frame(interact, loris_treatments)
interact_df$interact <- paste("Score", interact_df$interact, sep="_")
interact_df

# Normalized Barplot
# (for raw count, position="stack")
interact_plot <- ggplot(interact_df, aes(fill=interact, x=loris_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would seek out an experience to \n personally interact with this animal.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("loris_interact.jpg")
dev.off()



### LORIS
## QUESTION 3: This animal would make a good pet.
good_pet <- loris_data$X...This.animal.would.make.a.good.pet.
good_pet_df<- data.frame(good_pet, loris_treatments)
good_pet_df$good_pet <- paste("Score", good_pet_df$good_pet, sep="_")
good_pet_df

# Normalized Barplot
# (for raw count, position="stack")
good_pet_plot <- ggplot(good_pet_df, aes(fill=good_pet, x=loris_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"This animal would make a good pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("loris_good_pet.jpg")
dev.off()



### LORIS
## QUESTION 4: I would like to have this animals as a pet.
have_pet <- loris_data$X...I.would.like.to.have.this.animal.as.a.pet.
have_pet_df<- data.frame(have_pet, loris_treatments)
have_pet_df$have_pet <- paste("Score", have_pet_df$have_pet, sep="_")
have_pet_df

# Normalized Barplot
# (for raw count, position="stack")
have_pet_plot <- ggplot(have_pet_df, aes(fill=have_pet, x=loris_treatments)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                    labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")) + 
  xlab("Image") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("\"I would like to have this animal as a pet.\" \n Likert Scale Survey Responses") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("loris_have_pet.jpg")
dev.off()



figure <- ggarrange(research_plot, interact_plot, good_pet_plot, have_pet_plot,
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)

figure <- annotate_figure(
  figure,
  top = text_grob("Loris Posts", face = "bold", size = 14)
)

figure

