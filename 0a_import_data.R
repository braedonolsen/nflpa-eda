# NFLPA Data Competition ----
## Load and import datasets ----

### load packages ----
library(tidyverse)
library(nflverse)

load_schedules() |> 
  view()

load_injuries() |> 
  view()
# test line