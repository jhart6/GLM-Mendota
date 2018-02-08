library(readr) #to read in data - similar to read.csv, but inherently knows time stamps
library(dplyr)
library(tidyr)

setwd('~/Dropbox/LaMe GLM Calibration/Greedy/')
precip <- read.csv("NLDAS2_Mendota_2010_2016_cell_5.csv")

precipDaily <- precip %>%
  mutate(hour = hour(time)) %>% #adds an 'hour' column
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(rainDaily = sum(Rain, na.rm=TRUE)) %>%
  ungroup() %>%
  select(date,rainDaily)
View(precipDaily)
  
setwd("~/Dropbox/Mendota Summer 16/R/")
write.csv(precipDaily,file = "precipDaily.csv",row.names=FALSE)
