# Run this file to build all products of this package
# Uses drake. See: https://books.ropensci.org/drake/

source("R/packages.R")
source("R/functions.R")
source("R/plan.R")

source("data-raw/prepare_all.R")

all_plan <- bind_plans(
  prepare_all_plan,
  vignettes_plan
)

make(all_plan)
