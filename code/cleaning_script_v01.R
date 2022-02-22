# 22 february 2022
# data cleaning script
# first goal - organize columns, get multiple choice / select all columns into single columns

library(here)
library(tidyverse)
library(zoo)

setwd(here::here("Data/survey_monkey"))

d <- readxl::read_xlsx("survey_monkey_sea_eagle_2022_02_22.xlsx")

# first row which contains response categories
# creates a named charater vector
top <- d %>% slice(1) %>% unlist()

# just the names
names_top <- names(top)

# replace blank values in the name vector with NA- 
# this often happens for instances where there
# were response ccategories
names_top[grepl("\\.\\.\\.", names_top)] <- NA

# fill in the column names so ever response category has corresponding question
new_colnames <- zoo::na.locf(names_top)

# okay, I'm going to do this by hand and create short column names
# based on the values of the question / response categories
paste(new_colnames, unname(top), sep = "_")

new_names <- 
  c(
    "respond_id", #Respondent ID
    "collector_id", #Collector ID
    "start_date", # Start Date
    "end_date", # End Date
    "ip_address", # IP Adress
    "consent",  #I have read and understand the above and provide consent for the study
    "eagle_reference", #"Any mention or reference to \"eagle\" in the survey refers to the Steller's Sea Eagle_Response"
    "age18", #"Are you 18 (19 if in Alabama or Nebraska) years or older?
    "attempt_us", #"Did you attempt to see the Steller's Sea Eagle in the United States during December 2021 or January 2022?
    "attempt_ca",
    "find_out", #"How did you find out about the eagle?_Response"
    "find_out_other",
    "see", #Did you successfully see the eagle?
    "first_time", #Was this the first time you have seen the eagle?
    "date_first", #"If you successfully saw the eagle, what date did you first see it? (e.g., December 15, 2021; 15-Dec-2021; Dec-15-2021)
    "location", #Where did you attempt to see the eagle? Please enter the zip code, postal code, or city/state (city/province)
    "number_of_attempts", #"How many times did you travel to see the eagle?
    "leave_home_code", #Did seeing the bird require leaving your home zip/postal code?
    "home_code", #"What is your home zip/postal code?
    "observation_duration", #"How many minutes did you spend observing the eagle? If you visited the eagle more than once, please report the cumulative number of minutes spent observing (e.g., 15, 122, etc.).
    "private_vehicle", #"What mode of transportation did you use to see the bird?
    "rental_vehicle", #"What mode of transportation did you use to see the bird?
    "plane", #"What mode of transportation did you use to see the bird?
    "train",
    "other_transport", #"What mode of transportation did you use to see the bird?
    "carpool", #"If you used a vehicle, did you carpool or ride-share with others?
    "number_passengers", #"If you carpooled, how many individuals were in the vehicle?
    "primary_purpose", #"Was the eagle the primary purpose of the trip?
    "overnight", #"Did your trip involve overnight stay?
    "n_nights", #"If so, how many nights did you stay? Enter 0 if your trip did not involve overnight stay.
    "accomodation_friend",  #"What type of accommodation did you use?_Friend/relative's home"
    "accomodation_hotel", #
    "accomodation_campground",
    "accomodation_airbnb",
    "accomodation_vrbo",
    "accomodation_none",
    "accomodation_other",
    "accomodation_location", #Please enter the zip code, postal code, or city/state (city/province) of the accommodation in which you stayed. Enter NA if you did not use accommodations.
    "eat", #"While on the trip to see the eagle, how did you eat?
    "dine_fast_food", 
    "dine_fast_casual",
    "dine_sit_down",
    "dine_other",
    "number_dine_out", #"How many times did you eat out?
    "list_life", #"Which of the following \"bird lists\" do you keep?_Life list"
    "list_country", #"Which of the following \"bird lists\" do you keep?_Country list" 
    "list_state", #"Which of the following \"bird lists\" do you keep?_State list"
    "list_province", # province list
    "list_county", # county list
    "list_ebird", # only keep lists on ebird
    "list_idk", #"Which of the following \"bird lists\" do you keep?_I don't know what a bird list is"
    "list_none", #"Which of the following \"bird lists\" do you keep?_I don't keep bird lists"
    "list_other", #"Which of the following \"bird lists\" do you keep?_Other (please specify)" 
    "motivation_photography", #"What was your biggest motivation to see the bird?_Wildlife photography"
    "motivation_rarity",
    "motivation_vagrant",
    "motivation_life_list",
    "motivation_unlikely", #"What was your biggest motivation to see the bird?_Unlikely you would encounter the bird again" 
    "motivation_other", #"What was your biggest motivation to see the bird?_Other (please specify)" 
    "birding_experience",
    "birding_time", #"How long have you been birding? (approximate time in years)
    "bird_organization", #Do you belong to any bird-related organization?
    "gas_cost", #"Estimate how much you spent on gasoline. If you carpooled, please list the portion that you directly paid. If you carpooled but did not pay for gasoline, please enter
    "airfare_cost",  #"Estimate how much you spent on airfare. If you did not fly, please enter \"NA\
    "meal_cost", #"Estimate how much you spent on meals.
    "lodging_cost", #"Estimate how much you spent on lodging or accommodations.
    "donation5", #"If there was a required 'donation' of $5 to view the eagle, would you still have viewed the eagle?_
    "donation25", #"If there was a required 'donation' of $25 to view the eagle, would you still have viewed the eagle?
    "donation50", #"If there was a required 'donation' of $50 to view the eagle, would you still have viewed the eagle?
    "donation75",
    "donation100",
    "donation200",
    "age", #"What is your age?
    "gender", #What is your gender?
    "gender_other", #"What is your gender?_Other (please specify)" 
    "race", #Which of the following best describes you?
    "marital_status", 
    "marital_status_other",
    "hourly_wage", #"What is your approximate hourly wage?
    "highest_education", #What is your highest level of education?
    "highest_education_other",
    "employment_status", # "What best describes your employment status?
    "twitch_frequency", #"How often do you travel (leave your home zip/postal code) to view rare/vagrant birds in a year?
    "other_birding", #Did you do any other birding while you were in the area to view the eagle?
    "other_nature", #"Did you travel to other natural areas or pursue other nature-based activities while you were attempting to see the bird?
    "other_nature_01",#"If yes to the previous question, what activities did you participate in?_Activity 1
    "other_nature_02",
    "other_nature_03",
    "puffin_tour", #"If you viewed the eagle in Maine, did you book an Atlantic Puffin tour for the following season while in the area?
    "revisit", #"As a result of your trip to see the eagle, how likely are you to revisit that location in the future?
    "ebird", #"Did you submit your observation to eBird?
    "other_database", #"Did you submit your observation to any other database?
    "other_database_name", #"If you submitted to a database other than eBird, which was it?
    "twitter", #"Did you indicate you observed the bird on Twitter?,
    "facebook", #"Did you indicate you observed the bird on Facebook?
    "n_birders_visit1", #"Approximately how many other birders did you see while observing the eagle?
    "n_birders_visit2", 
    "n_birders_visit3",
    "comments")

d_new <- d %>% 
  setNames(., new_names) %>% 
  dplyr::filter(!row_number() == 1) # drop the first row

# COLUMNS THAT COULD USE SOME CLEANING (plus notes)
# date_first
# location
# observation_duration - some large values (> 1 day)
# carpool - change N/A to NA
# number_passengers - ugh this one's a mess
# n_nights - needs to have characters cleaned out
# birding_time - this one's a mess. have to clean out characters
# gas_cost - clean out characters / ranges
# airfare_cost - clean out characters / text
# meal_cost - clean out characters/ranges
# lodging cost - clean out characters / text 
# hourly_wage - mess
# n_biriders_visit1 / 2 /3 - would be nice to clean these up / categorize these
