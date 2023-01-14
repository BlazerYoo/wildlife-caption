library(ggplot2)
library(ggpubr)
library(nationalparkcolors)
library(tidyverse)
library(ggrepel)


# Read in dataset
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB4XGWTD2P7HSRYPQZSYTWEGY6B3HJQ')


# Treatment: image #
## image 1: mountain gorilla w/o captions
## image 2: mountain gorilla w/captions
## image 3: loris w/o captions
## image 4: loris w/captions
gorilla_treatment_labels <- c("Image1", "Image2")
gorilla_data <- dataset[dataset$Treatment.shown %in% gorilla_treatment_labels,]
loris_treatment_labels <- c("Image3", "Image4")
loris_data <- dataset[dataset$Treatment.shown %in% loris_treatment_labels,]


# Rename treatment names
gorilla_data$Treatment.shown = ifelse(gorilla_data$Treatment.shown == "Image1", "Gorilla Control", "Gorilla Test")
gorilla_treatments <- gorilla_data$Treatment.shown
loris_data$Treatment.shown = ifelse(loris_data$Treatment.shown == "Image3", "Loris Control", "Loris Test")
loris_treatments <- loris_data$Treatment.shown


# Generate plot
generate_plot <- function(q, animal, data, treatments, threshold){
  
  # Truncate extended decimal values
  trunc_number_n_decimals <- function(numberToTrunc, nDecimals){
    numberToTrunc <- numberToTrunc + (10^-(nDecimals+5))
    splitNumber <- strsplit(x=format(numberToTrunc, digits=20, format=f), split="\\.")[[1]]
    decimalPartTrunc <- substr(x=splitNumber[2], start=1, stop=nDecimals)
    truncatedNumber <- as.numeric(paste0(splitNumber[1], ".", decimalPartTrunc))
    return(truncatedNumber)
  }
  
  # Selectr 
  question <- data %>% select(q)
  question_df <- data.frame(question, treatments)
  
  # Reorganize df
  ctrl_score_freq <- question_df[question_df$treatments == paste(animal, "Control", sep=" "),]
  ctrl_freq <- table(ctrl_score_freq[, c(1)])
  
  test_score_freq <- question_df[question_df$treatments == paste(animal, "Test", sep=" "),]
  test_freq <- table(test_score_freq[, c(1)])
  
  ctrl_total = nrow(ctrl_score_freq)
  test_total = nrow(test_score_freq)
  
  df <- data.frame(c("Control", "Control", "Control", "Control", "Control",
                     "Test", "Test", "Test", "Test", "Test"),
                   c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5"),
                   c(trunc_number_n_decimals(ctrl_freq["1"]/ctrl_total, 4),
                     trunc_number_n_decimals(ctrl_freq["2"]/ctrl_total, 4),
                     trunc_number_n_decimals(ctrl_freq["3"]/ctrl_total, 4),
                     trunc_number_n_decimals(ctrl_freq["4"]/ctrl_total, 4),
                     trunc_number_n_decimals(ctrl_freq["5"]/ctrl_total, 4),
                     trunc_number_n_decimals(test_freq["1"]/test_total, 4),
                     trunc_number_n_decimals(test_freq["2"]/test_total, 4),
                     trunc_number_n_decimals(test_freq["3"]/test_total, 4),
                     trunc_number_n_decimals(test_freq["4"]/test_total, 4),
                     trunc_number_n_decimals(test_freq["5"]/test_total, 4)))
  
  colnames(df) <- c("Image", "Score", "Freq")
  
  
  # Titiel for barplot
  title <- gsub(".", " ", q, fixed=TRUE) %>% substring(5)
  
  # Normalized barplot
  ggplot(df, aes(x=Image, y=Freq, fill=Score)) + 
    geom_bar(position=position_stack(reverse = TRUE), stat="identity", color="black", width=0.9) +
    scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                      labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                      guide=guide_legend(reverse=TRUE)) +
    geom_label(aes(label = paste0(Freq*100,"%")), position=position_fill(vjust = 0.5, reverse = FALSE), size = 4,
               color = ifelse(df$Freq >= threshold, "black", rgb(0, 0, 0, alpha = 0)),
               fill = ifelse(df$Freq >= threshold, "white", rgb(0, 0, 0, alpha = 0)), show.legend = FALSE) +
    geom_label_repel(aes(label = paste0(Freq*100,"%")), position=position_fill(vjust = 0.2, reverse = FALSE), size = 4,
                     color = ifelse(df$Freq < threshold, "black", rgb(0, 0, 0, alpha = 0)),
                     fill = ifelse(df$Freq < threshold, "white", rgb(0, 0, 0, alpha = 0)), show.legend = FALSE) +
    xlab("Image") +
    ylab("Percentage of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    ggtitle(paste("\"", title, ".\"", sep="")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  #return(df)
}


#X...This.post.depicts.wildlife.research.
#X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
#X...This.animal.would.make.a.good.pet.
#X...I.would.like.to.have.this.animal.as.a.pet.

# Label staggering bar height threshold
# Stagger labels for bar sections that represent less than 15%
threshold = 0.15

# Generate plots for gorilla posts
generate_plot("X...This.post.depicts.wildlife.research.", "Gorilla", gorilla_data, gorilla_treatments, threshold)
generate_plot("X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.", "Gorilla", gorilla_data, gorilla_treatments, threshold)
generate_plot("X...This.animal.would.make.a.good.pet.", "Gorilla", gorilla_data, gorilla_treatments, threshold)
generate_plot("X...I.would.like.to.have.this.animal.as.a.pet.", "Gorilla", gorilla_data, gorilla_treatments, threshold)


# Generate plots for loris posts
generate_plot("X...This.post.depicts.wildlife.research.", "Loris", loris_data, loris_treatments, threshold)
generate_plot("X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.", "Loris", loris_data, loris_treatments, threshold)
generate_plot("X...This.animal.would.make.a.good.pet.", "Loris", loris_data, loris_treatments, threshold)
generate_plot("X...I.would.like.to.have.this.animal.as.a.pet.", "Loris", loris_data, loris_treatments, threshold)
