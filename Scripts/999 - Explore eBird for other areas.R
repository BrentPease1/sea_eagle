library(tidyverse)
library(here)
library(Hmisc)
library(broom)
library(modelsummary)
library(auk)
library(scales)
library(colorspace)
library(sf)

auk::auk_set_ebd_path(here('Data/ebd_stseag_relJun-2022'), overwrite = T)




# bring in eBird records
#ebird <- read.table(file = here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt'), header = T)
if(!file.exists(here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_hawaii.txt"))){
  
  ebd <- auk_ebd(here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt'))
  
  hawaii <- here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_hawaii.txt")
  
  stseag_HI <-  auk_ebd(here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt')) %>% 
    auk_bbox(bbox = c(-178.334698,	18.910361,	-154.806773,	28.402123)) %>%
    auk_filter(file = hawaii)
  
} else{
  
  hawaii <- here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_hawaii.txt")
  stseag_HI <- read_ebd(hawaii)

}

# bring in eBird records
if(!file.exists(here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_alaska.txt"))){
  
  ebd <- auk_ebd(here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt'))
  
  alaska <- here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_alaska.txt")
  
  stseag_AK <-  auk_ebd(here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt')) %>% 
    auk_bbox(bbox = c(-179.148909,	51.214183,	179.77847,	71.365162)) %>%
    auk_filter(file = alaska)
  
} else{
  
  alaska <- here("Data/ebd_stseag_relJun-2022/ebd_filtered_stseag_August_8_2022_alaska.txt")
  stseag_AK <- read_ebd(alaska)
  
}

test <- here('Data/ebd_stseag_relJun-2022/ebd_stseag_relJun-2022.txt')
test <- read_ebd(test) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
