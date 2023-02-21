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
ct <- read_tsv("../ct_values_2021-present_20221208.tsv.gz")



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

# ct_trend  |> 
#   write_csv("./data/ct_trend.csv")


#' ##############################
#' plot Ct data
#' ############################## 
ct_trend |> 
  ggplot(data = _, aes(x = date, y = ct_value)) +
  #geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  #geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.6) +
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen", linewidth = 1) +
  annotate(geom = "point", x = as.Date("2022-05-15"), y = 33, color = "darkgreen", size = 3, shape = 22, fill = "darkgreen") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d<br>%Y", limits = c(as.Date("2022-05-01"), as.Date("2022-12-07"))) +
  scale_y_continuous(limits = c(0,50)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = ggtext::element_markdown(color = "black", angle = 45, vjust = 0.5, hjust = 1),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.y = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "", y = "SARS-CoV-2 Cycle Threshold (Ct)") -> p_ct_pre
p_ct_pre


p_ct_pre |> 
  ggsave(plot = _, filename = "./figs/p_ct_pre.png", height = 6, width = 8, units = "in", dpi = 600)
p_ct_pre |> 
  ggsave(plot = _, filename = "./figs/p_ct_pre.pdf", height = 6, width = 8, units = "in")


ct_trend |> 
  # add outside hospital (OSH) data
  bind_rows(tibble(accession = "osh_test", order_date = NA, ct_value = 33, date = as.Date("2022-05-15"))) |> 
  ggplot(data = _, aes(x = date, y = ct_value, shape = is.na(order_date))) +
  geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  #geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.6) +
  #geom_point(color = "darkgreen", size = 3) +
  #geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  #annotate(geom = "point", x = as.Date("2022-05-15"), y = 33, color = "darkgreen", size = 3, shape = 22, fill = "darkgreen") +
  #annotate(geom = "text", x = as.Date("2022-11-30"), y = 45, label = "*", color = "darkgreen", size = 8) +
  annotate(geom = "text", x = as.Date("2022-11-30"), y = 45, label = "*", size = 8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d<br>%Y", limits = c(as.Date("2022-05-01"), as.Date("2022-12-07"))) +
  scale_y_continuous(limits = c(0,50)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = ggtext::element_markdown(color = "black", angle = 45, vjust = 0.5, hjust = 1),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.y = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "", y = "SARS-CoV-2 Cycle Threshold (Ct)") -> p_ct_post
p_ct_post

p_ct_post |> 
  ggsave(plot = _, filename = "./figs/p_ct_post.png", height = 4, width = 6, units = "in", dpi = 600)
p_ct_post |> 
  ggsave(plot = _, filename = "./figs/p_ct_post.pdf", height = 5, width = 6, units = "in")

