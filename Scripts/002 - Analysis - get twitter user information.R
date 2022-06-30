library(academictwitteR)
library(here)
library(tidyverse)
#set_bearer()
#get_bearer()


# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #


# learning how this works

# tweets <-
#   get_all_tweets(
#     query = "#StellersSeaEagle",
#     start_tweets = "2021-12-01T00:00:00Z",
#     end_tweets = "2022-01-31T00:00:00Z",
#     n = Inf,
#     bind_tweets = FALSE,
#     file = "stse_hashtag_tweets",
#     data_path = here("Data/twitter/hashtag tweets")
#   )
# 
# tweets <- bind_tweets(data_path = here("Data/twitter/hashtag tweets"), user = TRUE, output_format = 'tidy')
# 
# two <-   get_all_tweets(
#   query = "Steller's Sea Eagle",
#   start_tweets = "2021-12-01T00:00:00Z",
#   end_tweets = "2022-01-31T00:00:00Z",
#   n = Inf,
#   bind_tweets = FALSE,
#   file = "stse_name_tweets",
#   data_path = here("Data/twitter/name tweets")
# )
# 
# two <- bind_tweets(data_path = here("Data/twitter/name tweets"), user = TRUE, output_format = 'tidy')

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

if(!(dir.exists(here("Data/twitter/test3")))){
  
  twitter <-   get_all_tweets(
    query = c("Steller's Sea Eagle", "stellers sea eagle", "#StellersSeaEagle"),
    start_tweets = "2021-12-01T00:00:00Z",
    end_tweets = "2022-01-31T00:00:00Z",
    n = 10000,
    bind_tweets = FALSE,
    file = here("Data/twitter/test"),
    data_path = here("Data/twitter/test3"),
    is_retweet = F,
    is_reply = F,
    is_quote = F
    # bbox = c(-72.476807,41.087632,-66.511230,45.537137)
  )
} else{
  twitter <- bind_tweets(data_path = here("Data/twitter/test3"), user = TRUE, output_format = 'tidy')
  
}




# 1. create a monk vector
these <- c("we", "chase", "chased", "saw", "seeing", "see", "finding", "find",
           'drive', 'travel*')

# 2. create a `|` separate pattern of your vector as Tim Biegleisen already did
pattern_search<- paste(these, collapse = "|")

# Now 

# grepl returns a logical vector TRUE/FALSE
twit_table <- table(grepl(pattern_search, twitter$text))




# # grep returns the indices of the matching elements
# grep(pattern_search, twitter$text)
# 
# 
# # to get the value names we use the argument value = TRUE of grep
# grep(pattern_search, twitter$text, value = TRUE)
# 
# # str_detect returns logical vector 
# str_detect(twitter$text, pattern_search)
# 
# # str_extract extracts the matching values and leaves NA in the not matching
# str_extract(twitter$text, pattern_search)
# 
# 
# 
# # To apply these vector operation in a dataframe column basically you do the same, 
# # unless you want to get the the value in the new column, then you add an `ifelse` statement
# # Here is an example:
# 
# mtcars %>% 
#   select(1) %>% 
#   rownames_to_column("cars") %>% 
#   mutate(new_grepl_TRUEFALSE = grepl(pattern_vehicles, cars)) %>% 
#   mutate(new_grepl_value = ifelse(grepl(pattern_vehicles, cars), cars, NA_character_)) %>% 
#   mutate(new_str_detect_TRUEFALSE = str_detect(cars, pattern_vehicles)) %>% 
#   mutate(new_str_detect_value = ifelse(str_detect(cars, pattern_vehicles), cars, NA_character_)) %>% 
#   mutate(new_str_extract = str_extract(cars, pattern_vehicles)) %>% 
#   head()
