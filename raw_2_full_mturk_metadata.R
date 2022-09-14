library(tidyverse)

# Change working directory to encrypted drive
setwd("E:/Mturk")

merged_data <- list.files(path='E:/Mturk/') %>%
  lapply(read_csv) %>%
  bind_rows