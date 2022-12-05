#' ##############################
#' load libraries and set seed
#' ############################## 
library(tidyverse)
library(lubridate)
library(janitor)
library(gt)

set.seed(16)



#' ##############################
#' read O2 and med data
#' ############################## 
o2 <- read_csv("./data/dr_o2_flow_formatted_2022-12-05.csv")
o2

med <- read_csv("./data/dr_mar_2022-12-05.csv")
med


#' ##############################
#' format O2 data
#' ############################## 
o2 |> 
  separate(flow, into = c("o2sat","flow","units"), sep = " ", convert = TRUE) |> 
  drop_na() |> 
  arrange(date_time) |> 
  mutate(flow_smooth = map2_dbl(.x = flow, .y = lead(flow,1), .f = ~ min(.x, .y, na.rm = TRUE))) |> 
  identity() -> o2_clean
o2_clean


o2_clean |> 
  ggplot(data = _, aes(x = date_time, y = flow_smooth)) +
  geom_point() +
  geom_line() +
  scale_x_time()


#' ##############################
#' format med data
#' ############################## 

med |> 
  count(action)

med |> 
  filter(grepl("nirmatrelvir|prednisone|dexamethasone|methylprednisolone|remdesivir", tolower(order))) |> 
  filter(action == "Given" | action == "New Bag") |> 
  mutate(date = as.Date(date_time)) |> 
  select(date, order, action) |> 
  mutate(med = stringr::str_extract(string = tolower(order), pattern = "nirmatrelvir|prednisone|dexamethasone|methylprednisolone|remdesivir")) |> 
  distinct() |>
  #gt()
  select(date, med) |> 
  mutate(administered = TRUE) |> 
  pivot_wider(id_cols = c(date), names_from = c(med), values_from = administered, values_fill = FALSE) |> 
  #full_join(tibble(date = seq.Date(from = as.Date(min(med$date_time, na.rm = TRUE)), to = as.Date(max(med$date_time, na.rm = TRUE)), by = "day"))) |> 
  #replace_na(list("prednisone" = FALSE, "remdesivir" = FALSE, "nirmatrelvir" = FALSE, "methylprednisolone" = FALSE)) |> 
  mutate(steroid = prednisone | methylprednisolone) |> 
  arrange(date) |> 
  identity() -> med_on_off
med_on_off



med |> 
  filter(grepl("nirmatrelvir|prednisone|dexamethasone|methylprednisolone|remdesivir", tolower(order))) |> 
  filter(action == "Given" | action == "New Bag") |> 
  mutate(replace(order, grepl("methylprednisolone", tolower(order)) & grepl("62.5", tolower(order)), "predniSONE tablet 80 mg"),
         replace(dose, grepl("methylprednisolone", tolower(order)) & grepl("62.5", tolower(order)), "80 mg")) |> 
  filter(grepl("prednisone",tolower(order))) |> 
  mutate(date = as.Date(date_time), 
         dose_num = readr::parse_number(dose)) |> 
  select(date, dose_num) |> 
  group_by(date) |> 
  summarise(dose_num = max(dose_num, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(prednisone_equivalent = dose_num) |> 
  arrange(date) |> 
  identity() -> med_pred_dose
med_pred_dose


med_on_off |> 
  left_join(med_pred_dose, by = "date") |> 
  

