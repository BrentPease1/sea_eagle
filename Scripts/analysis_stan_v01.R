library(here)
library(tidyverse)
library(rstan)
library(MetBrewer)

setwd(here::here("Data/stan_data"))

load("eagle_data_for_stan_v01.RData")

clean_data <- clean_data %>% 
  filter(across(c(overnight, gender, marital_status, employment_status, highest_education,
                  age, carpool), ~!is.na(.x)))

mm <- model.matrix(log1p(total_travel_cost) ~ overnight + gender + marital_status +
               employment_status + highest_education + age + carpool, 
             data = clean_data)

stan_data <- list(
  N = nrow(clean_data),
  Y = log1p(clean_data$total_travel_cost), 
  Y_time = log1p(clean_data$total_travel_cost_time),
  K = ncol(mm), 
  X = mm,
  birders_estimate = birders_total_estimate,
  ebird_estimate = eBird_total_estimate
)

setwd(here::here("Scripts/stan_code"))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

out <- stan(
  file = "lm_v02.stan", 
  data = stan_data, 
  warmup = 1000, 
  iter = 2000, 
  chains = 4
)

pal <- MetPalettes$Demuth[[1]][c(4, 10)]

summary(out,
        probs = c(0.025, 0.975),
        pars = c("b", "Intercept", "sigma",
                 "b_time", "Intercept_time", "sigma_time",
                 
                 "mean_cost", "mean_cost_time",
                 "total_birders", "total_birders_time", 
                 "total_ebird", "total_ebird_time"))$summary %>% 
  as_tibble(rownames = "parameter") %>% 
  select(parameter, mean, lower95 = `2.5%`, upper95 = `97.5%`) %>% 
  filter(grepl("total", parameter)) %>% 
  mutate(time_cost = ifelse(grepl("time", parameter), "Time", "No time"),
         source = ifelse(grepl("birders", parameter), "Birders", "eBird")) %>% 
ggplot(aes(x = mean, y = time_cost, color = source)) + 
  geom_errorbar(aes(xmin = lower95, xmax = upper95), width = 0,
                position = position_dodge(width = 0.3), 
                size = 1) + 
  geom_point(position = position_dodge(width = 0.3), size = 3) + 
  scale_x_continuous(breaks = c(4e05, 5e05, 6e05, 7e05),
                     labels = c("400k", 
                                "500k", 
                                "600k", 
                                "700k")) + 
  scale_color_manual(values = pal) +
  theme_minimal() +
  labs(x = "Total expenditure (USD)",
       color = "Number of observers\n estimated from") + 
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "white",
                                         color = NA),
        legend.title.align = 0.5)
