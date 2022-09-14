library(tidyverse)

# Change working directory to encrypted drive
setwd("E:/simplified_qualtrics")

# Merge all csvs
#   https://www.statology.org/r-merge-csv-files/
old_data <- read.csv('E:/simplified_qualtrics/Old1.csv')
new_data <- read.csv('E:/simplified_qualtrics/New1.csv')

merged_data <- list.files(path='E:/simplified_qualtrics/') %>%
  lapply(read_csv) %>%
  bind_rows

# Remove rows 2 (extra column metadata), 3591 (column titles), and 3592 (extra column metadata)
#   https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame
df <- slice(merged_data, -c(2, 3591, 3592))

# Remove last four columns
#   https://stackoverflow.com/questions/26483643/drop-last-5-columns-from-a-dataframe-without-knowing-specific-number
df <- df[1:(length(df)-4)]

# Rename column names with second row data (and delete first row)
#   https://datacornering.com/use-data-frame-row-as-a-column-names-in-r/
df <- df %>% purrr::set_names(as.character(slice(., 1))) %>% slice(-1)

# Remove first 171 characters (directions) from questions
#   https://stackoverflow.com/questions/34047552/remove-first-two-characters-for-all-column-names-in-a-data-frame-in-r
#   https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
#   https://www.geeksforgeeks.org/count-number-of-characters-in-string-in-r/
names(df)[nchar(names(df)) > 171] <- substring(names(df)[nchar(names(df)) > 171], 171)

# Remove last 26 charatcers (additional directions) from questions
#   https://reactgo.com/r-remove-last-n-characters-string/
names(df)[substring(names(df), nchar(names(df))) == "1"] <- substring(names(df)[substring(names(df), nchar(names(df))) == "1"], 1, nchar(names(df)[substring(names(df), nchar(names(df))) == "1"]) - 26)

# Rename last, signature, and date columns
#   https://statisticsglobe.com/rename-column-name-in-r-data-frame/
names(df)[names(df) == "FL_14 - Block Randomizer - Display Order"] <- "Treatment shown"
names(df)[names(df) == "Click to write the question text - Subject's Signature"] <- "Subject's Signature"
names(df)[names(df) == "Click to write the question text - Date"] <- "Date"

# Export dataframe as csv
write.csv(df, 'E:/duplicate_data.csv', row.names=FALSE)