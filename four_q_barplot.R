library(tidyverse)

data <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB3SK6A6JU2CUN3O455GNAHQY4CPQAA')

colnames(data)

#install.packages("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")

# I would like to have this animal as a pet
bar1 <- subset(data, select = c(6,14))

colnames(bar1)

#bar1 <- tail(bar1, -3)

#bar1

img1 <- bar1[bar1$Treatment.shown == "Image1",]
img2 <- bar1[bar1$Treatment.shown == "Image2",]
img3 <- bar1[bar1$Treatment.shown == "Image3",]
img4 <- bar1[bar1$Treatment.shown == "Image4",]

# Image 1
img1_one <- nrow(img1[img1$X...This.animal.would.make.a.good.pet. == "1",])
img1_two <- nrow(img1[img1$X...This.animal.would.make.a.good.pet. == "2",])
img1_three <- nrow(img1[img1$X...This.animal.would.make.a.good.pet. == "3",])
img1_four <- nrow(img1[img1$X...This.animal.would.make.a.good.pet. == "4",])
img1_five <- nrow(img1[img1$X...This.animal.would.make.a.good.pet. == "5",])

x1 <- c("1Strongly disagree", "2Disagree", "3Neutral", "4Agree", "5Strongly agree")
freq1 <- c(img1_one, img1_two, img1_three, img1_four, img1_five)

df1 <- data.frame(x1, freq1)

# Image 2
img2_one <- nrow(img2[img2$Treatment.shown == "1",])
img2_two <- nrow(img2[img2$Treatment.shown == "2",])
img2_three <- nrow(img2[img2$Treatment.shown == "3",])
img2_four <- nrow(img2[img2$Treatment.shown == "4",])
img2_five <- nrow(img2[img2$Treatment.shown == "5",])

x2 <- c("1Strongly disagree", "2Disagree", "3Neutral", "4Agree", "5Strongly agree")
freq2 <- c(img2_one, img2_two, img2_three, img2_four, img2_five)

df2 <- data.frame(x2, freq2)

# Image 3
img3_one <- nrow(img3[img3$Treatment.shown == "1",])
img3_two <- nrow(img3[img3$Treatment.shown == "2",])
img3_three <- nrow(img3[img3$Treatment.shown == "3",])
img3_four <- nrow(img3[img3$Treatment.shown == "4",])
img3_five <- nrow(img3[img3$Treatment.shown == "5",])

x3 <- c("1Strongly disagree", "2Disagree", "3Neutral", "4Agree", "5Strongly agree")
freq3 <- c(img3_one, img3_two, img3_three, img3_four, img3_five)

df3 <- data.frame(x3, freq3)

# Image 4
img4_one <- nrow(img4[img4$Treatment.shown == "1",])
img4_two <- nrow(img4[img4$Treatment.shown == "2",])
img4_three <- nrow(img4[img4$Treatment.shown == "3",])
img4_four <- nrow(img4[img4$Treatment.shown == "4",])
img4_five <- nrow(img4[img4$Treatment.shown == "5",])

x4 <- c("1Strongly disagree", "2Disagree", "3Neutral", "4Agree", "5Strongly agree")
freq4 <- c(img4_one, img4_two, img4_three, img4_four, img4_five)

df4 <- data.frame(x4, freq4)

df5 <- c(rep("Gorilla no cap" , 3) , rep("Gorilla cap" , 3) , rep("Loris no cap" , 3) , rep("Loris cap" , 3))
frequency <- rep(c("Strongly disagree" , "Disagree" , "Neutral", "Agree", "Strongly agree") , 4)

final_df <- data.frame(species, labels, frequency)

ggplot(final_df, aes(1,freq,fill=x)) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Loris caption I would like to have this animal as a pet") +
  xlab("Loris caption") +
  ylab("Frequency")
