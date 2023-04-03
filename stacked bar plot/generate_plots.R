devtools::install_github("katiejolly/nationalparkcolors")

library(ggplot2)
library(ggpubr)
library(nationalparkcolors)
library(tidyverse)
#library(ggrepel)
#library(ggpmisc)
library(gginnards)


# Read in dataset
dataset <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAACAVXIZEO2EULA2QY7FKWAIMZBLEVNQ')


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
generate_plot <- function(q, animal, data, treatments, plot_args){
  
  threshold <- plot_args[1]
  size <- plot_args[2]
  padding <- plot_args[3]
  radius <- plot_args[4]
  border <- plot_args[5]
  hjust <- plot_args[6]
  vjust <- plot_args[7]
  lineWidth <- plot_args[8]
  lineLen <- plot_args[9]
  x0 <- plot_args[10]
  y0 <- plot_args[11]
  x1 <- plot_args[12]
  y1 <- plot_args[13]
  
  
  # Truncate extended decimal values
  trunc_number_n_decimals <- function(numberToTrunc, nDecimals){
    numberToTrunc <- numberToTrunc + (10^-(nDecimals+5))
    splitNumber <- strsplit(x=format(numberToTrunc, digits=20, format=f), split="\\.")[[1]]
    decimalPartTrunc <- substr(x=splitNumber[2], start=1, stop=nDecimals)
    truncatedNumber <- as.numeric(paste0(splitNumber[1], ".", decimalPartTrunc))
    return(truncatedNumber)
  }
  
  # Dataframe with just treatments and responses to given question
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
  p <- ggplot(df, aes(x=Image, y=Freq, fill=Score)) + 
    geom_bar(position=position_stack(reverse = TRUE), stat="identity", color="black", width=0.4) +
    scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                      labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                      guide=guide_legend(reverse=TRUE)) +
    
    geom_label(aes(label = paste0(Freq*100,"%")),
               position=position_fill(vjust = 0.5, reverse = TRUE),
               hjust = ifelse(df$Freq < threshold, abs(threshold / df$Freq) * -0.45, 0.3),
               label.padding = unit(padding, "lines"),
               label.r = unit(radius, "lines"),
               label.size = border,
               size = size,
               color = "black",
               fill= "white")
  
  # get the x and y coordinates of the label
  label_data <- layer_data(p, i=2L)
  print(label_data)
  x1 <- label_data[1,]$x
  y1 <- label_data[1,]$y

  x2 <- label_data[2,]$x
  y2 <- label_data[2,]$y

  x3 <- label_data[3,]$x
  y3 <- label_data[3,]$y
  
  x4 <- label_data[4,]$x
  y4 <- label_data[4,]$y
  
  x5 <- label_data[5,]$x
  y5 <- label_data[5,]$y
  
  x6 <- label_data[6,]$x
  y6 <- label_data[6,]$y
  
  x7 <- label_data[7,]$x
  y7 <- label_data[7,]$y
  
  x8 <- label_data[8,]$x
  y8 <- label_data[8,]$y
  
  x9 <- label_data[9,]$x
  y9 <- label_data[9,]$y
  
  x10 <- label_data[10,]$x
  y10 <- label_data[10,]$y

  plot <- ggplot(df, aes(x=Image, y=Freq, fill=Score)) + 
    geom_bar(position=position_stack(reverse = TRUE), stat="identity", color="black", width=0.4) +
    scale_fill_manual("Response", values=park_palette(n=5, name="Acadia"), 
                      labels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                      guide=guide_legend(reverse=TRUE)) +
    
    geom_segment(aes(x = x1, y = y1,
                     xend = x1+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y1), size = lineWidth, color = "red") +
    geom_segment(aes(x = x2, y = y2,
                     xend = x2+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y2), size = lineWidth, color = "red") +
    geom_segment(aes(x = x3, y = y3,
                     xend = x3+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y3), size = lineWidth, color = "red") +
    geom_segment(aes(x = x4, y = y4,
                     xend = x4+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y4), size = lineWidth, color = "red") +
    geom_segment(aes(x = x5, y = y5,
                     xend = x5+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y5), size = lineWidth, color = "red") +
    geom_segment(aes(x = x6, y = y6,
                     xend = x6+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y6), size = lineWidth, color = "red") +
    geom_segment(aes(x = x7, y = y7,
                     xend = x7+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y7), size = lineWidth, color = "red") +
    geom_segment(aes(x = x8, y = y8,
                     xend = x8+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y8), size = lineWidth, color = "red") +
    geom_segment(aes(x = x9, y = y9,
                     xend = x9+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y9), size = lineWidth, color = "red") +
    geom_segment(aes(x = x10, y = y10,
                     xend = x10+ifelse(Freq < threshold, lineLen, 0.000000001),
                     yend = y10), size = lineWidth, color = "red") +
    
    geom_label(aes(label = paste0(Freq*100,"%")),
               position=position_fill(vjust = 0.5, reverse = TRUE),
               hjust = ifelse(df$Freq < threshold, abs(threshold / df$Freq) * -0.2, -0.6),
               label.padding = unit(padding, "lines"),
               label.r = unit(radius, "lines"),
               label.size = border,
               size = size,
               color = "black",
               fill= "white") +
    
    xlab("Image") +
    ylab("Percentage of Respondents") +
    scale_y_continuous(labels=scales::percent) +
    ggtitle(paste("\"", title, ".\"", sep="")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  #move_layers(plot, "GeomLabel", position = "top")
  
  #ggsave(paste(title, ".jpg", sep=""))

  return(plot)
}


#X...This.post.depicts.wildlife.research.
#X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
#X...This.animal.would.make.a.good.pet.
#X...I.would.like.to.have.this.animal.as.a.pet.

# Label staggering bar height threshold
threshold = 0.1 # Stagger labels for bar sections that represent less than 10%
size = 5 # Label text size
padding = 0.2 # Label text padding
radius = 0.05 # Label border radius
border = 0.5 # Label border thickness
hjust = -0.6 # Change horizontal position of label
vjust = 0.5 # Change vertical position of label
lineWidth = 1 # Change labeling line width
lineLen = 0.3 # Change labeling line length
x0 = 0 # Labeling line start x
y0 = 0 # Labeling line start y
x1 = 1 # Labeling line end x
y1 = 1 # Labeling line end y


plot_args <- c(threshold, size, padding, radius, border, hjust, vjust, lineWidth, lineLen, x0, y0, x1, y1)

# Generate plots for gorilla posts
plot1 <- generate_plot("X...This.post.depicts.wildlife.research.", "Gorilla", gorilla_data, gorilla_treatments, plot_args)
#plot1
plot2 <- generate_plot("X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.", "Gorilla", gorilla_data, gorilla_treatments, plot_args)
#plot2
plot3 <- generate_plot("X...This.animal.would.make.a.good.pet.", "Gorilla", gorilla_data, gorilla_treatments, plot_args)
#plot3
plot4 <- generate_plot("X...I.would.like.to.have.this.animal.as.a.pet.", "Gorilla", gorilla_data, gorilla_treatments, plot_args)
#plot4

# General panel of plots for gorilla posts
figure <- ggarrange(plot1, plot2, plot3, plot4,
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)
figure <- annotate_figure(
  figure,
  top = text_grob("Gorilla Posts", face = "bold", size = 25)
)
figure



# Generate plots for loris posts
plot1 <- generate_plot("X...This.post.depicts.wildlife.research.", "Loris", loris_data, loris_treatments, plot_args)
#plot1
plot2 <- generate_plot("X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.", "Loris", loris_data, loris_treatments, plot_args)
#plot2
plot3 <- generate_plot("X...This.animal.would.make.a.good.pet.", "Loris", loris_data, loris_treatments, plot_args)
#plot3
plot4 <- generate_plot("X...I.would.like.to.have.this.animal.as.a.pet.", "Loris", loris_data, loris_treatments, plot_args)
#plot4


# General panel of plots for gorilla posts
figure <- ggarrange(plot1, plot2, plot3, plot4,
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)
figure <- annotate_figure(
  figure,
  top = text_grob("Loris Posts", face = "bold", size = 25)
)
figure
