#' ##############################
#' load libraries and set seed
#' ############################## 
library(tidyverse)
library(lubridate)
library(janitor)

set.seed(16)



#' ##############################
#' read Ct data
#' ############################## 
ct <- read_tsv("../ct_values_2021-present_20221026.tsv.gz")



#' ##############################
#' clean Ct data
#' ############################## 
ct |>
  filter(fk_patient_id == as.numeric(read_lines("../dr_fk.txt"))) |>
  select(accession, order_date, result_item_description, result_value) |>
  filter(grepl("Ct", result_item_description)) |>
  group_by(accession) |>
  summarise(order_date = min(order_date), ct_value = min(as.numeric(result_value), na.rm = TRUE)) |> 
  mutate(date = as.Date(order_date)) |> 
  identity() -> ct_trend
ct_trend


#' ##############################
#' plot Ct data
#' ############################## 
ct_trend |> 
  ggplot(data = _, aes(x = date, y = ct_value)) +
  geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey") +
  geom_point(color = "darkgreen") +
  geom_line(color = "darkgreen") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")  +
  scale_y_continuous(limits = c(0,50)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 45, vjust = 0.5, hjust = 1),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.y = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "", y = "SARS-CoV-2 Cycle Threshold (Ct)") -> p_ct
p_ct


p_ct |> 
  ggsave(plot = _, filename = "./figs/p_ct.png", height = 6, width = 8, units = "in", dpi = 600)
p_ct |> 
  ggsave(plot = _, filename = "./figs/p_ct.pdf", height = 6, width = 8, units = "in")
