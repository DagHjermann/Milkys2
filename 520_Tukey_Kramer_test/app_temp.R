#
# TEST VERSION: Trying to include Delta13C (negative vamues)
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("functions/function_HSDtest.R")
library(shiny)
library(ggplot2)
library(dplyr)

current_year <- 2023

# Data and station metadata
dat <- readRDS("../Data/109_adjusted_data_ELU_2024-09-25.rds")
dat_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv") %>%
  mutate(Station = paste(STATION_CODE, substr(Station_name, 1, 15)))

c <- 0.1

dat2 <- dat %>%
    left_join(dat_stations %>% select(STATION_CODE, Order, Station), by = "STATION_CODE") %>%
    mutate(
        Matrix = paste0(LATIN_NAME, ", ", TISSUE_NAME),
        value_type = case_when(
          PARAM %in% c("Delta13C", "Delta15N") ~ "VALUE_WW",
          TRUE ~ "log_VALUE_WW"),
        conc_for_test = case_when(
          value_type == "VALUE_WW" ~ VALUE_WW,
          value_type == "log_VALUE_WW" ~ log10(VALUE_WW + c)),
        LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")
    ) %>%
    filter(!is.na(conc_for_test))

# dat2 %>% filter(MYEAR == 2023 & PARAM == "Delta13C") %>% pull(VALUE_WW) %>% cat()

matrices <- dat2 %>%
    filter(MYEAR %in% (current_year-5):current_year) %>%
    count(Matrix) %>%
    arrange(desc(n)) %>%
    pull(Matrix)

years <- sort(unique(dat2$MYEAR))

group_seq <- c("Metals and metalloids", "Chlorobiphenyls", 
               "Polycyclic aromatic hydrocarbons (PAHs)", 
               "Organobromines", 
               "Dichloro-diphenyl-trichloroethane (DDTs)", "Organochlorines (general)",
               "Chlorinated paraffins", "Organofluorines", "Phenols/chlorophenols", 
               "Organo-metallic compounds", 
               "Chlorinated flame retardants",
               "Biological effects: molecular/biochemical/cellular/assays", 
               "Organic esters", "Isotopes",  
               "Cyclodienes", "Dioxins", "Biomarkers", "Phthalates", 
               "Phosphorus flame retardant (PFR)", "Major inorganic constituents", 
               "Support parameters", "Hexachlorocyclohexanes", "Triazines", 
               "Siloxanes", 
               ""
)

param_meta <- read.csv("../Input_data/Lookup_tables/Lookup table - substance groups.csv") %>%
  select(PARAM, Substance.Group) %>%
  mutate(Substance.Group = factor(Substance.Group, levels = group_seq))

# test
# groups <- unique(param_meta$Substance.Group)
# setdiff(groups, group_seq)
# setdiff(group_seq, groups)

parameters <- dat2 %>%
  filter(MYEAR %in% (current_year-5):current_year) %>%
  distinct(PARAM) %>%
  left_join(param_meta, by = "PARAM") %>%
  arrange(Substance.Group, PARAM) %>%
  pull(PARAM)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Milkys - sammenligning mellom stasjoner samme år"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("matrix", "Matrix", choices = matrices),
            selectInput("param", "Parameter", choices = parameters),
            selectInput("year", "Year", choices = years, selected = current_year)
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ggplot"),
            plotOutput("multcomp_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat_select <- reactive({
        dat2 %>%
            filter(
                PARAM %in% input$param & 
                    Matrix %in% input$matrix & 
                    MYEAR %in% input$year) %>%
        arrange(Order) %>%
        mutate(Station = forcats::fct_inorder(Station))
        
    }) 
    
    multcomp <- reactive({
        model <- aov(conc_for_test ~ Station, data = dat_select())
        outHSD <-  HSD.test(model, "Station", alpha = 0.05)
    })
    
    df_summ <- reactive({
        multcomp_val <- multcomp()
        multcomp_val$means %>%
            mutate(Station = rownames(multcomp_val$means)
            ) 
    })
    
    # For plotting Min, Max, Q25, Q75, Station_group labels
    df_groups <- reactive({l
        multcomp_val <- multcomp()
        data.frame(
            Station = rownames(multcomp_val$groups),
            Station_group = multcomp_val$groups$groups) %>%
            left_join(df_summ(), by = "Station") %>%
          rename(
                Mean = conc_for_test,
                Median = Q50) %>%
          left_join(dat_stations %>% select(Order, Station), by = "Station") %>%
          arrange(Order) %>%
          mutate(Station = forcats::fct_inorder(Station))
          
    })
    
    # For plotting mean and median
    df_groups2 <- reactive({
        df_groups_val <- df_groups()
        df_groups_val %>%
            select(Station, Mean, Median) %>%
            tidyr::pivot_longer(-Station, names_to = "Type", values_to = "Value")
    })
    
    output$ggplot <- renderPlot({
        ggplot(dat_select(), aes(Station, VALUE_WW, color = LOQ)) +
            geom_jitter(width = 0.1, height = 0, size = rel(1.6)) +
            scale_color_manual(values = c("Under LOQ" = "red2", "Over LOQ" = "black")) +
            scale_y_log10() +
            theme(axis.text.x = element_text(angle = -45, hjust = 0, size = rel(1.5))) +
            labs(x = "")
    })
    
    # output$multcomp_plot <- renderPlot({
    #     par(mfrow=c(1,1), mar=c(3,10,2,3))
    #     plot(multcomp(), horiz=TRUE, las = 1, cex.names = 1, 
    #          main = "Tukey-Kramer HSD test")
    # })
    
    output$multcomp_plot <- renderPlot({
        dat_select_val <- dat_select()
        ggplot(df_groups2(), aes(Station)) +
            geom_linerange(data = df_groups(), aes(ymin = Min, ymax = Max)) +
            geom_linerange(data = df_groups(), aes(ymin = Q25, ymax = Q75), size = 1.25) +
            geom_point(aes(y = Value, shape = Type), color = "white", fill = "red2", size = 3.5) +
            scale_shape_manual(values = c(22, 23)) +
            geom_text(data = df_groups(),
                      aes(
                          y = Max + diff(range(dat_select_val$conc_for_test))*0.07,
                          label = Station_group,
                          color = Station_group)) +
            labs(title = "Tukey-Kramer HSD post-hoc test", 
                 subtitle = "Lines show min-max (thin lines) and 25th-75th percentile (thick lines)",
                 y = "log10(Concentration + 0.1)", x = "") +
            theme(axis.text.x = element_text(angle = -45, hjust = 0, size = rel(1.5)))
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
