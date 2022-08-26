# plot effects of predictors

library(here)
library(tidyverse)

setwd(here::here("Results/"))

d <- read_csv("result_summary_v01.csv")

dont_want <- c(
  "Intercept", 
  "simplex1",
  "simplex2",
  "simplex3",
  "simplex4",
  "simplex5",
  "sigma1", 
  "sigma2", 
  "mean_cost1", 
  "mean_cost2", 
  "birders_mean1",
  "birders_mean2",
  "birders_max1",
  "birders_max2",
  "ebird1",
  "ebird2",
  "twitter1",
  "twitter2",
  "lp__")

library(MetBrewer)
pal <- MetPalettes$Demuth[[1]][c(3, 8)]

d %>% 
  filter(! name %in% dont_want) %>%
  arrange(mean) %>% 
  mutate(name = factor(name, levels = unique(name))) %>% 
  ggplot() +
  aes(y = name, x = mean, color = time) +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  geom_errorbar(aes(xmin = lower95, xmax = upper95), 
                width = 0,
                position = position_dodge(width = 0.4)) +
  geom_point(position = position_dodge(width = 0.4)) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  xlab("Effect") +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "white",
                                         color = NA))
