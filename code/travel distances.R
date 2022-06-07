library(opencage)
library(gmapsdistance)
library(zipcodeR)
library(here)
library(dplyr)
library(readr)
library(stringr)

data <- read_csv(here('Data/survey_monkey/sea_eagle_2022_02_22_date_location.csv'))

attempt <- data$location_attempt

# pad the zip codes with a 0 if they were dropped
for(i in 1:length(attempt)){
  this <- attempt[i]
  if(is.na(this)){
    next
  }else{
    if(nchar(this) == 4){
      this <- str_pad(this, width = 5, side = 'left', pad = '0')
      attempt[i] <- this
    } else{
      next
    }
  }
}

# add USA in locations
attempt <- gsub("Maine", "Maine, USA", attempt)
attempt <- gsub("Massachusetts", "Massachusetts, USA", attempt)
