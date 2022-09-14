library(tidyverse)

# Change working directory to encrypted drive
setwd("E:/")

# Read full data
raw_full_data <- read.csv('E:/duplicate_data.csv')

# Remove unecessary columns
#   https://www.tutorialspoint.com/how-to-remove-a-column-from-an-r-data-frame
df <- subset(raw_full_data, select = -c(Response.Type, IP.Address, Response.ID,
                                        Recipient.Last.Name, Recipient.First.Name,
                                        Recipient.Email, External.Data.Reference,
                                        Location.Latitude, Location.Longitude,
                                        Distribution.Channel, Subject.s.Signature))

# Export dataframe as csv
write.csv(df, 'E:/condensed_duplicate_data.csv', row.names=FALSE)