#' ##############################
#' load libraries and set seed
#' ############################## 
library(tidyverse)
library(lubridate)
library(janitor)
library(gt)
library(patchwork)

set.seed(16)



#' ##############################
#' read O2 and med data
#' ############################## 
o2 <- read_csv("./data/dr_o2_flow_formatted_2022-12-05.csv")
o2

epic_o2 <- read_csv("./data/dr_o2_flow_epic_2022-12-07.csv")
epic_o2

med <- read_csv("./data/dr_mar_2022-12-07.csv")
med


#' ##############################
#' format O2 data
#' ############################## 
# o2 |> 
#   separate(flow, into = c("o2sat","flow","units"), sep = " ", convert = TRUE) |> 
#   drop_na() |> 
#   arrange(date_time) |> 
#   mutate(flow_smooth = map2_dbl(.x = flow, .y = lead(flow,1), .f = ~ min(.x, .y, na.rm = TRUE))) |> 
#   identity() -> o2_clean
# o2_clean
# 
# o2_clean |> 
#   ggplot(data = _, aes(x = date_time, y = flow_smooth)) +
#   geom_point() +
#   geom_line() +
#   scale_x_time()

epic_o2 |> 
  select(date, o2_flow_l_min, fi_o2_percent) |> 
  pivot_longer(cols = c(-date), names_to = "o2_measure", values_to = "o2_value") |> 
  mutate(o2_measure = case_when(o2_measure == "fi_o2_percent" ~ "FiO2 (%)",
                                o2_measure == "o2_flow_l_min" ~ "O2 Flow (L/min)")) |> 
  ggplot(data = _, aes(x = date, y = o2_value, color = o2_measure)) +
  geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.6) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d(option = "turbo") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  facet_wrap(facets = ~ o2_measure, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 45, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(color = "black"),
        legend.position = "none") +
  labs(x = "", y = "") -> p_o2
p_o2



#' ##############################
#' format med data
#' ############################## 
med |> 
  filter(grepl("nirmatrelvir|prednisone|dexamethasone|methylprednisolone|remdesivir", tolower(order))) |> 
  mutate(date = as.Date(date_time)) |> 
  select(date, order, action) |> 
  mutate(med = stringr::str_extract(string = tolower(order), pattern = "nirmatrelvir|prednisone|dexamethasone|methylprednisolone|remdesivir")) |> 
  distinct() |>
  #gt()
  select(date, med) |> 
  distinct() |> 
  mutate(administered = TRUE) |> 
  pivot_wider(id_cols = c(date), names_from = c(med), values_from = administered, values_fill = FALSE) |> 
  #full_join(tibble(date = seq.Date(from = as.Date(min(med$date_time, na.rm = TRUE)), to = as.Date(max(med$date_time, na.rm = TRUE)), by = "day"))) |> 
  #replace_na(list("prednisone" = FALSE, "remdesivir" = FALSE, "nirmatrelvir" = FALSE, "methylprednisolone" = FALSE)) |> 
  mutate(corticosteroid = prednisone | methylprednisolone) |> 
  arrange(date) |> 
  identity() -> med_on_off
med_on_off


med_on_off |> 
  select(date, corticosteroid, remdesivir, nirmatrelvir) |> 
  pivot_longer(cols = c(-date), names_to = "med", values_to = "administered") |> 
  filter(date %in% unique(epic_o2$date)) |> 
  mutate(med = replace(med, med == "nirmatrelvir", "nirmatrelvir<br>-ritonavir")) |> 
  mutate(med = factor(x = med, levels = c("corticosteroid", "nirmatrelvir<br>-ritonavir", "remdesivir"))) |>
  ggplot(data = _, aes(x = date, y = med, fill = administered)) +
  geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.6) +
  geom_point(color = "black", shape = 21, size = 5) +
  scale_fill_manual(values = list("TRUE" = "black", "FALSE" = "white")) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d")  +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 45, vjust = 0.5, hjust = 1),
        axis.text.y = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "", y = "") -> p_med
p_med



#' ##############################
#' combine plots
#' ############################## 

((p_o2 / p_med) +
  plot_layout(heights = c(2,1))) |> 
  identity() -> p_combined
p_combined

p_combined |> 
  ggsave(plot = _, filename = "./figs/p_combined.png", height = 6, width = 8, units = "in", dpi = 600)
p_combined |> 
  ggsave(plot = _, filename = "./figs/p_combined.pdf", height = 6, width = 8, units = "in")


  



#' ##############################
#' reformat plots to anonymize dates
#' ############################## 

epic_o2 |> 
  select(date, o2_flow_l_min, fi_o2_percent) |>
  mutate(fi_o2_percent = fi_o2_percent / 100,
         date_num = as.numeric(date - min(date)),
         date_str = paste0("Hospital Day ", stringr::str_pad(string = date_num, width = 2, side = 'left', pad = "0"))) |>  
  rename(`FiO2 (%)` = fi_o2_percent,
         `O2 Flow (L/min)` = o2_flow_l_min,
         `Hospital Day` = date_num) |> 
  identity() -> epic_o2_plus
epic_o2_plus

epic_o2_plus |> 
  ggplot(data = _, aes(x = `Hospital Day`, y = `FiO2 (%)`)) +
  geom_vline(xintercept = 6, linetype = 2, color = "black") +
  #geom_rect(xmin = 6, xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.2) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.5) +
  #scale_color_viridis_d(option = "turbo") +
  #scale_color_manual(values = ggsci::pal_jama()(3)[1]) +
  scale_x_continuous(breaks = seq(0,18,1)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.y = ggtext::element_markdown(color = "black"),
        axis.title.x = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "Hospital Day", y = "Fraction of Inspired O<sub>2</sub>") -> p_fio2
p_fio2

epic_o2_plus |> 
  ggplot(data = _, aes(x = `Hospital Day`, y = `O2 Flow (L/min)`)) +
  geom_vline(xintercept = 6, linetype = 2, color = "black") +
  #geom_rect(xmin = 6, xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.2) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.5) +
  #scale_color_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,18,1)) +
  #scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = ggtext::element_markdown(color = "black"),,
        axis.title.y = ggtext::element_markdown(color = "black"),
        axis.title.x = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "Hospital Day", y = "O<sub>2</sub> Flow (L/min)") -> p_flowo2
p_flowo2

med_on_off |> 
  select(date, corticosteroid, remdesivir, nirmatrelvir) |> 
  pivot_longer(cols = c(-date), names_to = "med", values_to = "administered") |> 
  filter(date %in% unique(epic_o2$date)) |> 
  mutate(date_num = as.numeric(date - min(date)),
         date_str = paste0("Hospital Day ", stringr::str_pad(string = date_num, width = 2, side = 'left', pad = "0"))) |> 
  mutate(med = replace(med, med == "nirmatrelvir", "nirmatrelvir<br>-ritonavir")) |> 
  mutate(med = factor(x = med, levels = c("corticosteroid", "nirmatrelvir<br>-ritonavir", "remdesivir"))) |>
  ggplot(data = _, aes(x = date_num, y = med, fill = administered)) +
  #geom_vline(xintercept = as.Date("2022-11-25"), linetype = 2, color = "black") +
  #geom_rect(xmin = as.Date("2022-11-25"), xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.6) +
  geom_vline(xintercept = 6, linetype = 2, color = "black") +
  #geom_rect(xmin = 6, xmax = Inf, ymin = -Inf, ymax = Inf, color = NA, fill = "grey", alpha = 0.2) +
  geom_point(color = "black", shape = 21, size = 5) +
  scale_fill_manual(values = list("TRUE" = "black", "FALSE" = "white")) +
  scale_x_continuous(breaks = seq(0,18,1))  +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color = "black", angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = ggtext::element_markdown(color = "black"),
        legend.position = "none") +
  labs(x = "Hospital Day", y = "") -> p_med_num
p_med_num

(((p_fio2 + theme(axis.title.x = element_blank(), axis.text.x = element_blank())) / (p_flowo2 + theme(axis.title.x = element_blank(), axis.text.x = element_blank())) / p_med_num) +
  plot_layout(heights = c(1,1,1))) |> 
  identity() -> p_combined_num
p_combined_num

p_combined_num |> 
  ggsave(plot = _, filename = "./figs/p_combined_num.png", height = 6, width = 8, units = "in", dpi = 600)
p_combined_num |> 
  ggsave(plot = _, filename = "./figs/p_combined_num.pdf", height = 6, width = 8, units = "in")

