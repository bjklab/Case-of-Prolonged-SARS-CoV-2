#' ##############################
#' load libraries and set seed
#' ############################## 
library(tidyverse)
library(lubridate)
library(janitor)

set.seed(16)



#' ##############################
#' load raw epic med data
#' ############################## 
read_csv("../DR_MAR_2022-11-17_to_2022-12-07.csv") |> 
  clean_names() |> 
  mutate(date_time = mdy_hm(date_time)) |> 
  select(-action_by) |> 
  arrange(date_time, order) |> 
  identity() -> mar
mar

mar |> 
  write_csv("./data/dr_mar_2022-12-07.csv")


