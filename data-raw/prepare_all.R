library(drake)
library(here)

source(here("data-raw", "catalan_municipalities.R"))
source(here("data-raw", "./covid_tests_catalonia.R"))
source(here("data-raw", "./pollution_catalonia.R"))
source(here("data-raw", "./xema_stats_per_day.R"))

prepare_all_plan <- bind_plans(
  catalan_municipalities_plan,
  covid_tests_catalonia_plan,
  pollution_catalonia_plan,
  xema_stats_per_day_plan
)
