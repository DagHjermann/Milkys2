
# Lese 'saved_plots' i mappe 'Figures_402/Til 2021-rapporten/'  

# Lage nye plott og lagre disse  


fig_folder <- 'Figures_402/Til 2021-rapporten/' 
dir(fig_folder)

# check
readLines(paste0(fig_folder, "/_saved_plots.csv"), n = 3)

# read
# NOTE: had smart quaotes. Had to download the file to PC, replace, smart quotes with straight quotes and upload again 
logfile <- read.csv(paste0(fig_folder, "/_saved_plots.csv"), quote = "'")

i <- 67
input <- logfile[i,] %>%
  rename(param = PARAM,
         basis = Basis) %>%
  mutate(eqs = as.logical(eqs))
input <- as.list(input)
# input
stationcode <- input$STATION_CODE

# input$proref <- as.numeric(strsplit(input$proref, split = ",")[[1]])
# input$proref <- input$proref[!is.na(input$proref)]
# input$proref

tsplot <- plot_timeseries(param = input$param, stationcode = stationcode, basis = input$basis, 
                          y_scale = input$y_scale,
                          ymax_perc = input$ymax_perc,
                          xmin_rel = input$xmin_rel,
                          xmax_rel = input$xmax_rel,
                          eqs = input$eqs,
                          proref = input$proref,
                          folder = folder_results, 
                          data = dat_all_prep3, 
                          data_series = dat_series_trend, data_trend = df_trend, 
                          allsamples = TRUE)
tsplot

