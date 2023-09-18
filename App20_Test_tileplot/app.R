#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dat_groups <- read.csv("../Input_data/Lookup_tables/Lookup table - substance groups.csv")  

current_year <- 2021

library(ggiraph)
library(ggplot2)
library(lubridate)
library(flextable)
library(readxl)
library(purrr)
library(glue)
library(scico)            # colour palettes incl. "Vik" (https://github.com/thomasp85/scico)
library(cowplot)

library(dplyr)    # need to load dplyr AFTER ggiraph fro some reason, otherwise error "%>% is not found" 

# library(safejoin) # https://github.com/moodymudskipper/safejoin

source("../431_Report_parameter_functions.R")
source("../002_Utility_functions.R")

#
# Stations - for correct ordering
#
lookup_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv")

# Text strings for the plot y axis  
eider_blood <- "19N Kongsfjorden (eider duck blood)"
eider_egg <- "19N Kongsfjorden (eider duck egg)"


param_values <- c("AG", "AS", "CD", "CO", "CR", "CU", "HG", "NI", "PB", "SN", "ZN")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("param", "Parameter", choices = param_values, selected = "HG")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # plotOutput("distPlot")
          girafeOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  serieslastuntil <- 2021
  basis_medians <- "WW"
  species3 <- "Mytilus edulis"
  
  header_param <- reactive({
    subset(dat_groups, PARAM %in% param, select = Parameter.Name)[1,1]
  })
  
  species3 <- strsplit(species3, split = ",")[[1]]
  
  # debugonce(get_data_medians)
  

  # For server part in Shiny
  
  dat_temporary <- reactive({
    
    result <- list(
      # Cod
      get_data_medians(param = input$param,     # In Shiny: input$param
                       species = "Gadus morhua",
                       tissue = ifelse(input$param %in% "HG", "Muskel", "Lever"),
                       basis = basis_medians, 
                       include_year = serieslastuntil,
                       folder_110 = "../Data",
                       filename_110 = "110_mediandata_updated_2022-09-23.rds",
                       filename_lookup_substancegroups = "../Input_data/Lookup_tables/Lookup table - substance groups.csv",
                       filename_lookup_stations= "../Input_data/Lookup_tables/Lookup_stationorder.csv",
                       filename_lookup_eqs = "../Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                       filename_lookup_proref = "../Input_data/Lookup_tables/Lookup_proref.csv"),
      # Eider duck
      get_data_medians(param = input$param, 
                       species = "Somateria mollissima",
                       tissue = "Blod",
                       basis = "WW", 
                       include_year = serieslastuntil,
                       folder_110 = "../Data",
                       filename_110 = "110_mediandata_updated_2022-09-23.rds",
                       filename_lookup_substancegroups = "../Input_data/Lookup_tables/Lookup table - substance groups.csv",
                       filename_lookup_stations= "../Input_data/Lookup_tables/Lookup_stationorder.csv",
                       filename_lookup_eqs = "../Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                       filename_lookup_proref = "../Input_data/Lookup_tables/Lookup_proref.csv"),
      # Eider duck
      get_data_medians(param = input$param, 
                       species = "Somateria mollissima",
                       tissue = "Egg",
                       basis = "WW", 
                       include_year = serieslastuntil,
                       folder_110 = "../Data",
                       filename_110 = "110_mediandata_updated_2022-09-23.rds",
                       filename_lookup_substancegroups = "../Input_data/Lookup_tables/Lookup table - substance groups.csv",
                       filename_lookup_stations= "../Input_data/Lookup_tables/Lookup_stationorder.csv",
                       filename_lookup_eqs = "../Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                       filename_lookup_proref = "../Input_data/Lookup_tables/Lookup_proref.csv"),
      # Blue mussel  
      get_data_medians(param = input$param, 
                       species = species3,
                       tissue = "Whole soft body",
                       basis = "WW", 
                       include_year = serieslastuntil,
                       folder_110 = "../Data",
                       filename_110 = "110_mediandata_updated_2022-09-23.rds",
                       filename_lookup_substancegroups = "../Input_data/Lookup_tables/Lookup table - substance groups.csv",
                       filename_lookup_stations= "../Input_data/Lookup_tables/Lookup_stationorder.csv",
                       filename_lookup_eqs = "../Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                       filename_lookup_proref = "../Input_data/Lookup_tables/Lookup_proref.csv")
    )
    
    # browser()
    result
    
  })
  
  # test <- reactive({
  #   x <- dat_temporary()
  #   # browser()
  #   x
  # })

  
  #
  # Make 'dat_medians_list'  
  #
  
  dat_medians_list <- reactive({
    
    result <- list()
    
    # List element 1: cod and eider duck
    
    if (nrow(dat_temporary()[[2]]) > 0){
      result[[1]] <- bind_rows(
        dat_temporary()[[1]],
        dat_temporary()[[2]] %>% mutate(Station = eider_blood),
        dat_temporary()[[3]] %>% mutate(Station = eider_egg)
      )
    } else {
      result[[1]] <- dat_temporary()[[1]]
    }
    
    result[[1]]$Station <- factor(result[[1]]$Station, 
                                            levels = c(levels(dat_temporary()[[1]]$Station), eider_blood, eider_egg))
    
    
    # For Hg: add an asterisk after station names for stations that are NOT length-adjusted (for Hg only)
    # - these data are also used for 'ratio plots' (last year's concentrations)  
    if (input$param %in% "HG" & basis_medians %in% "WWa"){
      # View(result[[1]])
      tab <- xtabs(~Basis + Station, result[[1]])
      not_length_adjusted <- colnames(tab)[tab[2,] == 0]
      sel <- levels(result[[1]]$Station) %in% not_length_adjusted
      levels(result[[1]]$Station)[sel] <- paste(levels(result[[1]]$Station)[sel], "*")
    }
    
    
    # levels(result[[1]]$Station)
    
    # List element 2 = blue mussel
    
    result[[2]] <- dat_temporary()[[4]]
    
    # browser()
    result
    
  })
  

  
  #
  # From lines 207 onwards in script 434
  #
  
  proref_cols <- c(RColorBrewer::brewer.pal(6, "Blues")[5:2],
                   RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])
  
  col_func <- function(x){cols[x]}
  
  startyr <- 2012
  # Trick to avoid red frames also in legend:
  # - put 'fill = Proref_ratio_cut' in aes of the first geom_tile (not in the aes of ggplot)
  # - set 'alpha = 0' so there is no fill in the EQS tiles 
  
  gg_tile_list <- reactive({
    
    result <- list()
    
    speciesgroup_txt <- c("cod and eider duck", "blue mussel")
    speciesgroup_txt_proref <- c("cod", "blue mussel")
    
    for (speciesgroup in 1:2){
      
      if (nrow(dat_medians_list()[[speciesgroup]]) > 0){
        
        dat_medians <- dat_medians_list()[[speciesgroup]] %>%
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
        
        # browser()
        
        
        # Set plot title (mercury in cod+birds is treated as a special case)
        if (input$param %in% "HG" & speciesgroup == 1 & basis_medians == "WW"){
          plot_title <- paste0(dat_medians$Parameter.Name[1], " (not length-adjusted) in ", speciesgroup_txt[[speciesgroup]])
        } else if (input$param %in% "HG" & speciesgroup == 1 & basis_medians == "WWa"){
          plot_title <- paste0(dat_medians$Parameter.Name[1], " (length-adjusted) in ", speciesgroup_txt[[speciesgroup]])
        } else {
          plot_title <- paste0(dat_medians$Parameter.Name[1], " in ", speciesgroup_txt[[speciesgroup]])
        }
        
        result[[speciesgroup]] <- ggplot(dat_medians, aes(MYEAR, Station)) +
          geom_tile(aes(fill = Proref_ratio_cut)) + 
          geom_tile(data = subset(dat_medians, Above_EQS %in% "Over"),
                    color = "red", size = 1, height = 0.9, width = 0.9, alpha = 0) +
          geom_text_interactive(aes(label = Value_txt, tooltip = Tooltip_txt), nudge_y = 0, size = 3) +
          # geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
          scale_fill_manual("Proref ratio", values = proref_cols, na.value = "grey85") +
          scale_color_manual(values = c("red", "white")) +
          scale_alpha_manual(values = c(1, 0)) +
          scale_x_continuous(breaks = seq(startyr, 2020, 2), 
                             limits = c(startyr-0.5, current_year+0.5)) +
          theme_bw() +
          guides(colour = "none") +
          labs(
            title = plot_title,
            x = "Year", y = "")
        
      } else {
        
        result[[speciesgroup]] <- NULL
        
      }
      
    }
    
    # browser()
    
    # WE HAD FORGOTTEN THE NEXT LINE:
    result
    
  })
  

  # output$distPlot <- renderPlot({
  #   gg_tile_list()[[1]]
  # })
  output$distPlot <- renderGirafe({
      gg <- gg_tile_list()[[1]]
      girafe(ggobj = gg)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
