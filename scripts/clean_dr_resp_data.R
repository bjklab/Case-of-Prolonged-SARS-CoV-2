#' ##############################
#' load libraries and set seed
#' ############################## 
library(tidyverse)
library(lubridate)

set.seed(16)



#' ##############################
#' load raw resp carelign data
#' ############################## 
read_csv("./data/DR O2 requirement 2022-12-04.csv", col_names = FALSE) |> 
  identity() |> 
  mutate(x2 = as.Date(X1),
         x3 = mdy_hm(X1),
         x4 = replace(X1, !is.na(x3), NA)
  ) |> 
  identity() -> dr
dr

# dr |> 
#   View()
# tibble(date_time = dr$x3[!is.na(dr$x3)], flow = dr$x4[!is.na(dr$x4)])

dr |> 
  select(x3, x4) |>
  fill(x4, .direction = "up") |> 
  filter(!is.na(x3)) |> 
  rename(date_time = x3,
         flow = x4) |> 
  identity() -> dr_clean
dr_clean

dr_clean |> 
  write_csv("./data/dr_o2_flow_formatted_2022-12-04.csv")



read_csv("./data/DR O2 requirement 2022-12-05.csv", col_names = FALSE) |> 
  identity() |> 
  mutate(x2 = as.Date(X1),
         x3 = mdy_hm(X1),
         x4 = replace(X1, !is.na(x3), NA)
         ) |> 
  identity() -> dr
dr

# dr |> 
#   View()
# tibble(date_time = dr$x3[!is.na(dr$x3)], flow = dr$x4[!is.na(dr$x4)])

dr |> 
  select(x3, x4) |>
  fill(x4, .direction = "up") |> 
  filter(!is.na(x3)) |> 
  rename(date_time = x3,
         flow = x4) |> 
  identity() -> dr_clean
dr_clean

dr_clean |> 
  write_csv("./data/dr_o2_flow_formatted_2022-12-05.csv")



#' ##############################
#' load raw resp epic data
#' ############################## 
read_csv("./data/DR_epic_oxygenation_2022-12-05.csv", col_names = FALSE) |> 
  pivot_longer(cols = c(-X1)) |> 
  select(-name) |> 
  pivot_wider(names_from = X1, values_from = value, values_fn = list) |> 
  unnest(cols = dplyr::everything()) |> 
  clean_names() |> 
  identity() -> dr_epic
dr_epic

dr_epic |> 
  gt()

dr_epic |> 
  write_csv("./data/dr_o2_flow_epic_2022-12-05.csv")
