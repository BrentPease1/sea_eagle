library(tidyverse)
library(here)


# bring in data with distances calculated
if(!(file.exists(here("Data/survey_monkey/sea_eagle_cleaning_2022_03_22_geocoded.csv")))){
  source(here("code/001 - Data Prep - bring together and get distances.R"))
  clean_data <- out
  rm(out)
} else{
  clean_data <- read_csv(here("Data/survey_monkey/sea_eagle_cleaning_2022_03_22_geocoded.csv"))
}



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


# first bind full dataset with travel dataset calculated above
full_model_data <- clean_data %>%
  select(respond_id, travel_dist_meter, travel_time_sec, meal_cost_new,
         gas_cost_new, lodging_cost_new, airfare_cost_new,
         age, gender, marital_status, employment_status, race, home_zip,
         date_text, overnight, overnight_count, carpool, carpool_count)

# calculate total dollars spent to operate vehicle.
# transform meters to miles, multiple by auto cost (cents/mi), divide by 100 to get dollars
full_model_data <-  full_model_data %>%
  mutate(automobile_operating_cost = ((travel_dist_meter * 0.00062137) * standard_auto_cost) / 100)

# calculate total travel cost

full_model_data <-  full_model_data %>%
  group_by(respond_id) %>%
  mutate(total_travel_cost = sum(gas_cost_new, airfare_cost_new, meal_cost_new, 
                                 lodging_cost_new, automobile_operating_cost, na.rm = T)) %>%
  ungroup()

  # select any columns to be kept for analyses
  dplyr::select(number_of_trips, distance_km, time_hours,
                dining_cost, petrol_cost, accomodation_cost, airfare_cost,
                age, sex, marital_status, employment_status, education, postcode, 
                date_first_seen, `overnight?`, car_pool, car_pool_size) %>%
  # classify age into age groups
  mutate(age_group=cut(age, breaks=c(18, 34, 50, 65, Inf), labels=c("18-34", "35-50", "51-65", "66+"))) %>%
  # zero-fill accomodation
  replace_na(list(accomodation_cost=0)) %>%
  rename(overnight=`overnight?`) %>%
  mutate(number_of_trips = gsub("Once", 1, .$number_of_trips)) %>%
  mutate(number_of_trips = gsub("Twice", 2, .$number_of_trips)) %>%
  mutate(number_of_trips = gsub("Three times or more", 3, .$number_of_trips)) %>%
  mutate(number_of_trips = as.integer(as.character(.$number_of_trips))) %>%
  mutate(travel_cost_no_time = ((distance_km*2)*standard_auto_cost),
         opportunity_cost_half_hourly = (time_hours*(0.5*hourly_wage_rate))) %>%
  # if car-pooled, divide the travel cost by the number of car-pool size
  replace_na(list(car_pool_size=1)) %>%
  mutate(travel_cost_no_time = travel_cost_no_time/car_pool_size) %>%
  mutate(travel_cost_time = travel_cost_no_time+opportunity_cost_half_hourly) %>%
  mutate(travel_cost_and_accomodation_no_time=travel_cost_no_time+accomodation_cost,
         travel_cost_and_accomodation_time=travel_cost_time+accomodation_cost) %>%
  ## impute missing data
  mutate(sex=impute(sex),
         marital_status=impute(marital_status, fun="random"),
         employment_status=impute(employment_status, fun="random"),
         education=impute(education, fun="random"),
         age_group=impute(age_group, fun="random"))