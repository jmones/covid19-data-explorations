# Used when building using a transient R session, by calling r_make()
# Uses drake. See: https://books.ropensci.org/drake/

source("R/packages.R")
source("R/functions.R")
source("R/plan.R")

source("data-raw/prepare_all.R")

all_plan <- bind_plans(
  prepare_all_plan,
  vignettes_plan
)

drake_config(all_plan)
