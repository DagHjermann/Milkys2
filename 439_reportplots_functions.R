
library(dplyr)
library(forcats)
library(ggplot2)
library(ggiraph)

filename_110 <- "Data/110_mediandata_updated_ELU_2024-09-25.rds"      # update
# This trend file is created in milkys4 project, script 903 (hence the name)
filename_trends <- "Data/milkys4_903_trends_02.rds"                   # update

dat_medians_all <- readRDS(filename_110)
lookup_stations <- read.csv("Input_data/Lookup_tables/Lookup_stationorder.csv") # %>%
# lookup_proref <- read.csv("Input_data/Lookup_tables/Lookup_proref.csv")

proref_cols <- c(RColorBrewer::brewer.pal(6, "Blues")[5:2],
                 RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])

dat_medians <- dat_medians_all %>%
  filter(PARAM == "HG" & LATIN_NAME == "Gadus morhua" & MYEAR > 2003 & TISSUE_NAME == "Muskel" & Basis == "WW") %>%
  filter(STATION_CODE != "67B") %>%
  group_by(STATION_CODE) %>%
  mutate(n_years = length(unique(MYEAR))) %>%
  ungroup() %>%
  filter(n_years > 5) %>%
  left_join(lookup_stations %>% select(STATION_CODE, Order, Station_name), by = join_by(STATION_CODE)) %>%
  arrange(desc(Order)) %>%
  mutate(
    Station = paste(STATION_CODE, Station_name),
    Station = fct_inorder(Station),
    # hard-coded:
    EQS_ratio = Value/0.020,
    Above_EQS = EQS_ratio >= 1,
    Below_EQS = EQS_ratio < 1,
    # hard-coded:
    Proref_ratio = Value/0.019,
    Proref_ratio_binned = cut(Proref_ratio, breaks = c(0,0.5,0.75,0.9,1,2,5,10,20,100))) %>% 
  select(Station, MYEAR, Value, EQS_ratio, Above_EQS, Below_EQS, Proref_ratio, Proref_ratio_binned)

str(dat_medians)
levels(dat_medians$Station)

xtabs(~STATION_CODE + MYEAR, dat_medians)

# test plot
ggplot(dat_medians, aes(MYEAR, Station, fill = Proref_ratio_binned)) +
  geom_tile() +
  scale_fill_viridis_d()


make_tileplot <- function(data, x, y, fill, 
                          fill_colours = NULL, fill_legendtitle = fill, 
                          text = NULL,
                          tooltip = NULL,
                          frame = c(NA, NA),
                          frame_colours = c("red", "black")){
  gg <- ggplot(dat_medians, aes(.data[[x]], .data[[y]])) +
    geom_tile(aes(fill = .data[[fill]]))
  # Frame 1
  if (!is.na(frame[1])){
    gg <- gg + geom_tile(data = data[data[[frame[1]]],],
                         color = frame_colours[1], linewidth = 1, height = 0.9, width = 0.9, alpha = 0)
  }
  # Frame 2
  if (!is.na(frame[2])){
    gg <- gg + geom_tile(data = data[data[[frame[2]]],],
                         color = frame_colours[2], linewidth = 1, height = 0.9, width = 0.9, alpha = 0)
  }
  # Color scale for the fill
  if (is.null(fill_colours) & (is.factor(data[[fill]]) | is.character(data[[fill]]))){
    gg <- gg + scale_fill_viridis_d(name = fill_legendtitle)
  } else if (!is.null(fill_colours) & (is.factor(data[[fill]]) | is.character(data[[fill]]))){
    gg <- gg + scale_fill_manual(name = fill_legendtitle, values = fill_colours, na.value = "grey85")
  } else if (is.null(fill_colours) & is.numeric(data[[fill]])){
    gg <- gg + scale_fill_viridis_c(name = fill_legendtitle)
  } else {
    stop("only fillscale = viridis implemented")
  }
  # Text in plot
  if (!is.null(text) & !is.null(tooltip)){
    gg <- gg +
      geom_text_interactive(aes(label = .data[[text]], tooltip = .data[[tooltip]]), nudge_y = 0, size = 3)
  } else if (!is.null(text) & !is.null(tooltip)){
    gg <- gg +
      geom_text(aes(label = .data[[text]]), nudge_y = 0, size = 3)
  } 
  gg
}
#" debugonce(make_tileplot)
make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio")
make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio_binned")
make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio_binned", 
              fill_colours = proref_cols, fill_legendtitle = "Proref ratio")
make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio_binned", 
              fill_colours = proref_cols, fill_legendtitle = "Proref ratio", text = "Value")make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio_binned", 
              fill_colours = proref_cols, fill_legendtitle = "Proref ratio", text = "Value", 
              frame = c("Above_EQS", NA))
make_tileplot(dat_medians, "MYEAR", "Station", fill = "Proref_ratio_binned", 
              fill_colours = proref_cols, fill_legendtitle = "Proref ratio", text = "Value", 
              frame = c("Above_EQS", "Below_EQS"))




make_tileplot <- function(data){
  ggplot(dat_medians, aes(MYEAR, Station)) +
    geom_tile(aes(fill = Proref_ratio_binned)) + 
    geom_tile(data = subset(data, Above_EQS),
              color = "red", size = 1, height = 0.9, width = 0.9, alpha = 0) +
    geom_text_interactive(aes(label = Value, tooltip = Value), nudge_y = 0, size = 3) +
    # geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
    scale_fill_manual("Proref ratio", values = proref_cols, na.value = "grey85") +
    scale_color_manual(values = c("red", "white")) +
    scale_alpha_manual(values = c(1, 0)) +
    # scale_x_continuous(breaks = seq(startyr, 2020, 2), 
    #                    limits = c(startyr-0.5, current_year+0.5)) +
    theme_bw() +
    guides(colour = "none")
    # labs(
    #   title = plot_title,
    #   x = "Year", y = "")
}
make_tileplot(dat_medians)

dat_trends_all <- readRDS(filename_trends)
dat_trends <- dat_trends_all %>%
  filter(PARAM == "HG" & LATIN_NAME == "Gadus morhua" & TISSUE_NAME == "Muskel" & Basis == "WW",
         STATION_CODE %in% unique(exampledata$STATION_CODE))




  mutate(
    Value_txt = case_when(
      is.na(FLAG1) ~ signif2(Value, 2, maxdigits = 5),
      !is.na(FLAG1) & Value > 0 ~ paste0("<", signif2(Value, 2, maxdigits = 4)),
      !is.na(FLAG1) & Value == 0 ~ "0"                                # for 'SCCP eksl. LOQ' + 'MCCP eksl. LOQ'
    ),
    txt_conc = paste0("Median value: ", Value_txt, 
                      " (", signif(Value_min, 2), "-", signif(Value_max, 2), "; N =", N_median, ")"),
    txt_perc = paste0("25% and 75% percentiles: ", signif(Value_p25, 2), "-", signif(Value_p75, 2)),
    txt_loq = paste0("Measurements over LOQ: ", Over_LOQ, 
                     " (", signif(100*(Over_LOQ/N_median), 1), " %); median LOQ = ", Det_limit),
    txt_eqs = paste0("The median is ", signif(EQS_ratio, 2), " times the EQS (", EQS, ")"),
    txt_proref = paste0("The median is ", signif(Proref_ratio, 2), " times the PROREF (", Proref, ")"),
    Tooltip_txt = txt_conc,
    Tooltip_txt = ifelse(N_median > 5, paste(Tooltip_txt, "<br>", txt_perc), Tooltip_txt),
    Tooltip_txt = ifelse(Over_LOQ < N_median, paste(Tooltip_txt, "<br>", txt_loq), Tooltip_txt),
    Tooltip_txt = ifelse(!is.na(EQS), paste(Tooltip_txt, "<br>", txt_eqs), Tooltip_txt),
    Tooltip_txt = ifelse(!is.na(Proref), paste(Tooltip_txt, "<br>", txt_proref), Tooltip_txt)
  )
