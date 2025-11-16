pacman::p_load(tidyverse, rio, lubridate, here, epikit, flextable) ### Load packages
###################################################################################################################################

### 1. Curated and cleaning database WWARNset
data <- read_csv("data/WWARNset.csv", locale = locale(encoding = "Latin1"))
