library(tidyverse)

# Change working directory to encrypted drive
setwd("E:/drive-download-20220919T004420Z-001")

# Read csvs
qualtrics <- read.csv('E:/drive-download-20220919T004420Z-001/no_id_full_data.csv')
mturk <- read.csv('E:/drive-download-20220919T004420Z-001/mturk_workerids.csv')

# Rename Answer.surveycode column in mturk
names(mturk)[names(mturk) == "Answer.surveycode"] <- "Random.ID"

# Add WorkerId column to qualtrics
#   https://stackoverflow.com/questions/66854059/adding-a-new-element-in-a-column-based-on-a-value-of-another-column-in-r
merged_data <- merge(qualtrics, mturk, by = "Random.ID", all = TRUE)

# Remove rows with NA in columns
#   https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
merged_data <- na.omit(merged_data)

# Move WorkerId column to beginning
#   https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame
merged_data <- merged_data %>%
  select("WorkerId", everything())

# Sort by Start.Date
merged_data <- merged_data[order(merged_data$Start.Date),]

# Remove rows with duplicate WorkerIds
#   https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
merged_data = merged_data[!duplicated(merged_data$WorkerId),]

# Export dataframe as csv
write.csv(merged_data, 'E:/no_duplicate_dataset.csv', row.names=FALSE)