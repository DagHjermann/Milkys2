
library(dplyr)
library(ggplot2)
library(forcats)

#
# Check PAH metabolites
#

dat_raw <- readRDS("Data/109_adjusted_data_2022-09-23.rds")  
dat_medians <- readRDS("Data/110_mediandata_updated_2022-09-23.rds")
lookup_stations <- read.csv("Input_data/Lookup_tables/Lookup_stationorder.csv")  


# Check parameters in each year
xtabs(~PARAM + MYEAR, dat_medians %>% filter(MYEAR >= 2007 & TISSUE_NAME == "Galle"))
xtabs(~PARAM + MYEAR, dat_raw %>% filter(MYEAR >= 2007 & TISSUE_NAME == "Galle"))

# Check stations in each year
xtabs(~STATION_CODE + MYEAR, dat_medians %>% filter(MYEAR >= 2009 & TISSUE_NAME == "Galle"))
xtabs(~STATION_CODE + MYEAR, dat_raw %>% filter(MYEAR >= 2009 & TISSUE_NAME == "Galle"))

# OH-pyren - normalized and non-normalized
dat_medians %>% 
  filter(MYEAR > 2000 & PARAM %in% c("PYR1O","PYR1OH") & STATION_CODE %in% c("53B","23B","30B") & Basis == "WW") %>%
  ggplot(aes(MYEAR, Value, color = PARAM)) +
  geom_point() +
  geom_line() +
  scale_y_log10() + 
  facet_wrap(vars(STATION_CODE))

#
# As above but "non-normalized" 2009 also removed  
#
params <- c("PYR1O","PYR1OH")
params_name <- "1-hydroxy pyrene in cod bile (PYR1OH)"
ref_value <- 21
data_plot <- dat_medians %>% 
  filter(MYEAR > 2000 & PARAM %in% params & STATION_CODE %in% c("15B", "53B","23B","30B") & Basis == "WW") %>%
  filter(!(MYEAR == 2009 & PARAM %in% c("PYR1OH"))) %>%
  left_join(lookup_stations %>% select(STATION_CODE, Station_name, Order)) %>%
  arrange(Order) %>%
  mutate(
    Station = paste(STATION_CODE, Station_name),
    Station = fct_inorder(Station))

ggplot(data_plot, aes(MYEAR, Value, color = PARAM)) +
  geom_pointrange(aes(ymin = Value_p25, ymax = Value_p75)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10() + 
  geom_hline(yintercept = ref_value, linetype = "dashed", color = "blue") +
  annotate("text", x = 1996, y = ref_value, label = "BAC", color = "blue", 
           hjust = 0, vjust = -0.2, size = 3) +
  facet_wrap(vars(Station)) +
  labs(title = params_name, y = "Concentration in bile (ng ml-1)", x = "") +
  theme_bw()

#
# Same for hydroxy-phenantrene; "non-normalized" 2009 also removed  
#
params <- c("PA1O","PA1OH")
params_name <- "1-hydroxy phenantrene in cod bile (PA1OH)"
ref_value <- 2.7

data_plot <- dat_medians %>% 
  filter(MYEAR > 2000 & PARAM %in% params & STATION_CODE %in% c("15B", "53B","23B","30B") & Basis == "WW") %>%
  filter(!(MYEAR == 2009 & PARAM %in% c("PYR1OH"))) %>%
  left_join(lookup_stations %>% select(STATION_CODE, Station_name, Order)) %>%
  arrange(Order) %>%
  mutate(
    Station = paste(STATION_CODE, Station_name),
    Station = fct_inorder(Station))

ggplot(data_plot, aes(MYEAR, Value, color = PARAM)) +
  geom_pointrange(aes(ymin = Value_p25, ymax = Value_p75)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_log10() + 
  scale_x_continuous(limits = c(1996, 2021)) +
  geom_hline(yintercept = ref_value, linetype = "dashed", color = "blue") +
  annotate("text", x = 1996, y = ref_value, label = "BAC", color = "blue", 
           hjust = 0, vjust = -0.2, size = 3) +
  facet_wrap(vars(Station)) +
  labs(title = params_name, y = "Concentration in bile (ng ml-1)", x = "") +
  theme_bw()




dat_medians %>% 
  filter(MYEAR > 2000 & PARAM %in% c("PA1O","PA1OH") & STATION_CODE == "53B" & Basis == "WW") %>%
  ggplot(aes(MYEAR, Value, color = PARAM)) +
  geom_point() +
  geom_line() +
  scale_y_log10()

dat_medians %>% 
  filter(MYEAR > 2000 & PARAM %in% c("ABS380","AY380") & STATION_CODE == "23B" & Basis == "WW") %>%
  ggplot(aes(MYEAR, Value, color = PARAM)) +
  geom_point() +
  geom_line()

View(tab[sel,])

test <- dat_trends_plot_list[[1]] %>% filter(Trend_type == "long")

levels(test$Station)

