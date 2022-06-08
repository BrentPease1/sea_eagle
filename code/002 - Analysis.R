library(tidyverse)
library(here)
library(Hmisc)
library(broom)


# bring in data with distances calculated
if(!(file.exists(here("Data/survey_monkey/sea_eagle_cleaning_2022_03_22_geocoded.csv")))){
  source(here("code/001 - Data Prep - bring together and get distances.R"))
  clean_data <- out
  rm(out)
} else{
  clean_data <- read_csv(here("Data/survey_monkey/sea_eagle_cleaning_2022_03_22_geocoded.csv"))
}

# need a few columns that I forgot to add in 001 - Data Prep - bring together and get distances
# script just has all "bring together" code and skips geocoding
source(here('code/001 - Data Prep - add ebird status to cleaned data.R'))

clean_data <- clean_data %>%
  left_join(x = clean_data, y = out %>% filter(!duplicated(respond_id)) %>% select(respond_id, ebird), by = 'respond_id')

rm(out)

# bring in education data
edu <- read_csv(here("Data/survey_monkey/sea_eagle_cleaned_2022_02_23_cost_corrections.csv")) %>%
  select(respond_id, highest_education)

clean_data <- clean_data %>%
  left_join(x = clean_data, y = edu %>% filter(!duplicated(respond_id)) %>% select(respond_id, highest_education), by = 'respond_id')
rm(edu)

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

## Apply values to calculate travel cost
## standard operating cost of automobile (cents/mi) 
## Bureau of Transportation Statistics
## [https://www.bts.gov/content/average-cost-owning-and-operating-automobilea-assuming-15000-vehicle-miles-year?msclkid=85d0714cb12611ec9c683208ed4b86e3]
standard_auto_cost <- 0.637 #averge cents per mile

## Hourly wage rate
## Federal average number of work hours a year, accounting for leap year = 2,087
## [https://www.opm.gov/policy-data-oversight/pay-leave/pay-administration/fact-sheets/computing-hourly-rates-of-pay-using-the-2087-hour-divisor?msclkid=ddd2f5f2b12611ec87569fbde77228ed]
## Median household income = $68,703
## [https://www.census.gov/library/publications/2020/demo/p60-270.html?msclkid=b33e0b9fb12611eca2382b17765f8fe5]
hourly_wage_rate <- 68703 / 2087


# # first bind full dataset with travel dataset calculated above
# clean_data <- clean_data %>%
#   select(respond_id, travel_dist_meter, travel_time_sec, meal_cost_new,
#          gas_cost_new, lodging_cost_new, airfare_cost_new,
#          age, gender, marital_status, employment_status, highest_education, race, home_zip,
#          date_text, overnight, overnight_count, carpool, carpool_count,
#          how_many_other_birders_1, how_many_other_birders_2, how_many_other_birders_3)

# calculate total dollars spent to operate vehicle.
# transform meters to miles, multiple by 2 for round trip, multiple by auto cost (cents/mi), divide by 100 to get dollars
clean_data <-  clean_data %>%
  mutate(automobile_operating_cost = ((((travel_dist_meter * 0.00062137)*2) * standard_auto_cost) / 100),
         opp_cost_time = (travel_time_sec / 3600) * (0.5 * hourly_wage_rate)) %>%
  replace_na(list(carpool = "No")) %>% #clean up carpool
  replace_na(list(carpool_count = 0)) %>% #clean up carpool
  replace_na(list(carpool_count = 0)) %>%  #clean up carpool
  replace_na(list(meal_cost_new = 0)) %>%
  replace_na(list(lodging_cost_new = 0)) %>%
  replace_na(list(airfare_cost_new = 0)) %>%
  replace_na(list(gas_cost_new = 0)) %>%
  # ## impute missing data
  # mutate(sex=impute(sex),
  #        marital_status=impute(marital_status, fun="random"),
  #        employment_status=impute(employment_status, fun="random"),
  #        education=impute(education, fun="random"),
  #        age_group=impute(age_group, fun="random"))
  
  # calculate total travel cost
    group_by(respond_id) %>%
    mutate(total_travel_cost = sum(gas_cost_new, airfare_cost_new, meal_cost_new, 
                                   lodging_cost_new, automobile_operating_cost, na.rm = T),
           total_travel_cost_time = sum(gas_cost_new, airfare_cost_new, meal_cost_new, 
                                        lodging_cost_new, automobile_operating_cost, opp_cost_time, na.rm = T)) %>%
    ungroup()

# clean gender column and throw out NA sociodemographics
clean_data <- clean_data %>%
  mutate(gender = ifelse(gender == "Non-binary / non-conforming" | gender == "Prefer not to say" |
                           gender == "Other (please specify)" | gender == "Transgender", "Other", gender)) %>%
  mutate(marital_status = ifelse(marital_status == 'Other (please specify)', "Other", marital_status)) %>%
  filter(!is.na(gender))

# group GED education with other
clean_data <- clean_data %>%
  mutate(highest_education = ifelse(highest_education == 'GED', "Other (please specify)", highest_education))

write.csv(clean_data, file = here("Data/cleaned_responses/sea_eagle_cleaning_2022_06_07_geocoded_travel_cost.csv"))



# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #


## Run a regression model
no_time <- glm(log1p(total_travel_cost) ~ overnight + gender + marital_status +
                 employment_status + highest_education + age + carpool, data=clean_data, family=gaussian)

no_time_summary <- tidy(no_time)

## Calculate the average predicted value and sum of predicted values
# The average cost per individual without opportunity cost of time included
mean(exp(no_time$fitted.values))

without_time_adjusted_value <- mean(exp(no_time$fitted.values))

# The overall estimate of the event without opportunity cost of time included - using only the respondents
sum(exp(no_time$fitted.values))

# Run a regression model
yes_time <- glm(log1p(total_travel_cost_time) ~ overnight + gender + marital_status +
                 employment_status + highest_education + age + carpool, data=clean_data, family=gaussian)

yes_time_summary <- tidy(yes_time)

# The average cost per individual with opportunity cost of time included
mean(exp(yes_time$fitted.values))

with_time_adjusted_value <- mean(exp(yes_time$fitted.values))

# The overall estimate of the event with opportunity cost of time included - using only the respondents
sum(exp(yes_time$fitted.values))



# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #



## Estimate of total number of birders/visitors ####
total_number_birders <- clean_data %>%
  select(date_text, ebird, how_many_other_birders_1, how_many_other_birders_2,
         how_many_other_birders_3)
# using eBird data first

# bring in eBird records
ebird <- read_table(file = here('Data/ebd_US_stseag_prv_relApr-2022/ebd_US_stseag_prv_relApr-2022.txt'))


# store some numbers
total_number_birders_in_dataset <- nrow(clean_data)
proportion_of_respondents_who_submitted_to_eBird <- as.data.frame(prop.table(table(clean_data$ebird))) %>% filter(Var1=="Yes") %>% .$Freq
total_eBird_records <- nrow(ebird)

#eBird estimate
(total_eBird_records*total_number_birders_in_dataset)/(total_number_birders_in_dataset*proportion_of_respondents_who_submitted_to_eBird)

eBird_total_estimate <- (total_eBird_records*total_number_birders_in_dataset)/(total_number_birders_in_dataset*proportion_of_respondents_who_submitted_to_eBird)

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #


## using birder number estimates
total_number_birders$date_first_seen <- as.Date(total_number_birders$date_text, format="%b-%d-%Y")

# clean columns
total_number_birders$how_many_other_birders_2 <- ifelse(total_number_birders$how_many_other_birders_2 == "Went to Georgetown, ME in 2022 and saw the eagle again: 250 birders", 250, total_number_birders$how_many_other_birders_2)
total_number_birders$how_many_other_birders_2 <- ifelse(total_number_birders$how_many_other_birders_2 == "40 (first time viewed)", 40, total_number_birders$how_many_other_birders_2)
total_number_birders$how_many_other_birders_2 <- as.numeric(as.character(total_number_birders$how_many_other_birders_2))

total_number_birders$how_many_other_birders_3 <- ifelse(total_number_birders$how_many_other_birders_3 == "400 (last time viewed)", 400, total_number_birders$how_many_other_birders_3)
total_number_birders$how_many_other_birders_3 <- ifelse(total_number_birders$how_many_other_birders_3 == "50.       Visit 4: 250", 50, total_number_birders$how_many_other_birders_3)
total_number_birders$how_many_other_birders_3 <- ifelse(total_number_birders$how_many_other_birders_3 == "Pending/tmrw", NA, total_number_birders$how_many_other_birders_3)
total_number_birders$how_many_other_birders_3 <- ifelse(total_number_birders$how_many_other_birders_3 == "Na", NA, total_number_birders$how_many_other_birders_3)
total_number_birders$how_many_other_birders_3 <- as.numeric(as.character(total_number_birders$how_many_other_birders_3))


total_estimate <- total_number_birders %>%
  group_by(date_first_seen) %>%
  summarise(mean=mean(c(how_many_other_birders_1,how_many_other_birders_2,
                      how_many_other_birders_3), na.rm=TRUE),
            max=max(c(how_many_other_birders_1,how_many_other_birders_2,
                    how_many_other_birders_3), na.rm=TRUE))

ggplot(total_estimate, aes(x=date_first_seen, y=max))+
  geom_point()+
  geom_line()+
  stat_smooth(method="loess", se=FALSE)

## get the smoothed values
## This looks at how different the results would be if 
## we used a smoothed curve, instead of
## the method above, but there was little difference
model <- loess(max ~ as.numeric(date_first_seen), data=total_estimate)
sum(predict(model))

sum(total_estimate$max)
sum(total_estimate$mean)

birders_total_estimate <- sum(total_estimate$max)





## Total economic estimates
without_time_adjusted_value*eBird_total_estimate
with_time_adjusted_value*eBird_total_estimate

without_time_adjusted_value*birders_total_estimate
with_time_adjusted_value*birders_total_estimate
