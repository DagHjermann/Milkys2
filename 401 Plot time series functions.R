#
# "Master" function: 'plot_medians_and_trends()'
#
# For making plot, one calls 'plot_medians_and_trends()'
#   - returns a list object 
# For showing plot,  use 'gg' from this list object
# For saving plot, call 'plot_medians_and_trends()' using this list object
#   
#
# TODO: allow for more trend symbols: presently handles only arrowup, arrowdown, circle
# * Functions involved in trend symbols
#     - plot_medians_and_trends 
#     - add_trend
# * How does it work? 
#     - plot_medians_and_trends() reads big excel file and translates (say) 
#       c("¢","é") to c("circle", "arrowup") -> object "trends"
#     - "trends" is sent to add_trend(), which takes the existing ggplot and adds the trend symbols 


# Note: trend symbols can be added using the following code inside the last gg part:
# geom_text(aes(x=Inf, y=Inf, hjust=1.6, vjust=1.6, label = "\u21E7 \u0337 \u21E9 \u0337 \u2606 \u0337 \u26AA \u0337 \u25FE"), size = rel(8)) +  
  
# Test of Unicode symbols
# http://unicode-search.net/unicode-namesearch.pl?term=upwards%20arrow  => UPWARDS WHITE ARROW, DoWNWARDS WHITE ARROW (21E7, 21E9)
# http://unicode-search.net/unicode-namesearch.pl?term=slash  => COMBINING SHORT SOLIDUS OVERLAY (0337)
# http://unicode-search.net/unicode-namesearch.pl?term=circle  => MEDIUM WHITE CIRCLE (26AA)
# http://unicode-search.net/unicode-namesearch.pl?term=star => WHITE STAR (2606)
# http://unicode-search.net/unicode-namesearch.pl?term=square => BLACK MEDIUM SMALL SQUARE (25FE), BLACK SMALL SQUARE (25AA), BLACK SQUARE (25A0)

# Also see http://www.fileformat.info/info/unicode/block/supplemental_arrows_c/fontsupport.htm 



#
# plot_medians_and_trends()
# Function for full-color plots based on one "line" in the data, including trend symbols  
#
# Using
# * get_plotdata
# * get_dfbig 
# * plot_medians_color
# * add_trend   
#
# OVERVIEW:
#   calls get_plotdata() 
#      - gets data from data_med2
#      - runs trend analysis using model_from_medians()
#      - returns median data + trend lines as a big list object (X)
#   calls get_dfbig()
#      - gets the relevant line from the big excel file
#      - gets EQS and trend symbols from here
#      - returns dfbig = one line of the big excel file
#   calls plot_medians_color() with X (for points + trend), proref and EQS (from dfbig)
#      - makes plot, except trend symbols
#      - returns plot (gg) 
#   creates trends (= arrowup, circle etc.)
#   calls add_trend() with gg + trends as input
#      - adds plot symbols to the plot (gg)
#   create file name
#   returns plot (gg) and file name (fn) 


plot_medians_and_trends <- function(ser, x_rel = 0.8, y_rel = 0.9, xlim = NULL, ylim = 1.15, 
                                    titlesize = 0.85, trend_years = NULL, ...){
  
  if (is.null(trend_years) & !is.null(xlim))
    trend_years <- seq(xlim[1], xlim[2])
  if (is.null(trend_years) & is.null(xlim))
    stop("You must supply either xlim or trend_years")
  X <- get_plotdata(ser[1], ser[2], ser[3], ser[4], ser[5],
                    trend_years = trend_years)
  start_yr <-  2*floor(min(X$df_data$MYEAR)/2)

  # The next is needed because the big excel file for 2018 (made in 2019) uses
  #   English tissue names
  dfbig_tissue <- case_when(
    ser[3] == "Lever" ~ "Liver",
    ser[3] == "Muskel" ~ "Muscle",
    TRUE ~ ser[3])
  dfbig <- get_dfbig(ser[1], ser[2], dfbig_tissue, ser[4], ser[5])
  
  # Last year
  last_year <- grep("Yr", names(dfbig), value = TRUE) %>% sub("Yr_", "", .)  %>% as.numeric() %>% max()
  
  # Proref from median data
  # proref <- X$df_data$Q95[1]

  # Proref from big excel data
  proref <- dfbig$Q95[1]
  
  cat("PROREF = ", proref, ", EQS = ", dfbig$EQS, "\n")
  
  # Axis limits - note some hard-coding if xlim = NULL
  if (is.null(xlim))
    xlim <- c(start_yr,2020) - 2  # to make room for PROREF text
  if (is.null(ylim))
    if (length(ylim) == 1){
      ylim <- c(0, max(X$df_data$Median)*ylim)
    }
  
  # This is making the right tissue to use in the plot title
  X$tissue <- case_when(
    X$tissue %in% "Lever" ~ "liver",
    X$tissue %in% "Muskel" ~ "muscle",
    X$tissue %in% "Whole soft body" ~ "soft body",
    TRUE ~ X$tissue
  )
  
  # Title
  plot_title <- paste0(X$paramname, " in ", tolower(X$speciesname), " ", X$tissue, ", ", X$stationname)
  if (ser[5] %in% c("WWa", "DWa", "FBa"))
    plot_title <- paste0(plot_title, ". Note: Length-adjusted concentrations")
  # Insert line breaks if text is longer than 69 characters
  plot_title <- break_text(plot_title, 69, type = "max", 
                           no_break_after = c("st.", "Note:")
  )
  
  # Find breaks to supply to 'scale_x_continuous'
  yrstep <- case_when(
    diff(xlim) < 18 ~ 2,     # show every second year on x axis 
    diff(xlim) >= 18 ~ 4     # show every fourth year on x axis
  )
  
  # Years to show on x axis
  breaks <- seq(
    2*ceiling(xlim[1]/2), 
    2*floor(xlim[2]/2), 
    yrstep)
  
  # Start plot.   ELU: testing "geom_text(aes(x=Inf, y=Inf, hjust=10.0, vjust=2.7, label = "\u25AA"), size = rel(8))"
  
  gg <- plot_medians_color(X, proref, xlim = xlim, eqs = dfbig$EQS, ylim = ylim, ...) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2]), breaks = breaks) +
    ggtitle(plot_title) +
    
    #ELU: evt små firkanter som mangler:
    #geom_text(aes(x=Inf, y=Inf, hjust=10.1, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    #geom_text(aes(x=Inf, y=Inf, hjust=8.6, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    
    theme(title = element_text(size = rel(titlesize)))
  
  # Add trend symbols. 
  trend_variable <- paste0("Trends.", last_year)
  trendsymbol_xl <- c(substr(dfbig[[trend_variable]], 1, 1), substr(dfbig[[trend_variable]], 3, 3))
  trends <- case_when(
    trendsymbol_xl %in% "¢" ~ "circle",
    trendsymbol_xl %in% "é" ~ "arrowup",
    trendsymbol_xl %in% "ê" ~ "arrowdown",
    trendsymbol_xl %in% "§" ~ "filledsquare",
  )
 
   gg <- add_trend(gg, trendsymbols = trends, x_rel = x_rel, y_rel = y_rel, 
                  x_spacing = c(0, 0.035, 0.065), fontsize = 10)
  
  
  # Make file name (but does not save file)
  fn <- paste0("TSplot_", ser[1], "_", ser[2], "_", ser[3], "_", ser[4], "_", ser[5], ".png")
  
  # Return plot (gg) and file name (fn)
  list(gg = gg, fn = fn)
}

# ylim <- 0.44
# X <- plot_medians_and_trends(c("HG", "Gadus morhua", "Muskel", "13B",  "WW"), #
#                              eqs_type = "background", xlim = c(1978, 2018), ylim = c(0, ylim))
# X$gg


#
# Function . ELU: 1980:2018 -> 1980:2019?
#

get_plotdata <- function(param, species, tissue = NULL, station, basis = "WW", 
                                            title = NULL, 
                                            include_zero = TRUE, proref_x = NULL, show_proref = 1:5,
             
                                            
                                                                           xlim = NULL, ylim = NULL, ylim_proref = NULL,
                                            trend_years = 1980:2018,
                                            data_medians = data_med2,
                                            lookup_stations = df_stationnames,
                                            lookup_params = df_paramnames,
                                            lookup_species = df_speciesnames){
  
  if (is.null(tissue)){
    if (species %in% "Gadus morhua" & param %in% c("HG","C/N","Delta13C","Delta15N", "DRYWT%")){
      tissue <- "Muskel"
    } else if (species %in% "Gadus morhua"){
      tissue <- "Lever"
    } else {
      tissue <- "Whole soft body"
    }
  }
  
  if (!param %in% c("VDSI", "BAP3O")){      # only these two have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  
  # get saved raw data
  df_data <- subset(data_medians, PARAM %in% param & LATIN_NAME %in% species & 
                      TISSUE_NAME %in% tissue & STATION_CODE %in% station & Basis %in% basis & !is.na(Median))
  
  if (is.null(xlim))
    xlim <- range(df_data$MYEAR, na.rm = TRUE) + c(-1,0)
  if (is.null(title))
    title <- paste0(param, " (", basis, ") in ", species, " ", tissue, ", ", station)
  if (is.null(proref_x)){
    proref_x <- xlim[1]
  }
  

  mod <- model_from_medians(param, species, tissue, station, basis, yrs = trend_years, data_medians = data_medians)
  # mod_long <- model_from_medians(param, species, tissue, station, basis, yrs = 1980:2018, data_medians = data_medians)
  # mod_10yr <- model_from_medians(param, species, tissue, station, basis, yrs = 2008:2018, data_medians = data_medians)
  
  if (mod$statistics$Model_used %in% "Nonlinear"){
    # Make smoothed line for the non-linear fit
    # https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points-maybe-polynomial-interpolation
    dfpred <- mod$result_object$modelresults$mod_nonlin$yFit
    mod_est <- as.data.frame(spline(dfpred$Year, dfpred$Estimate))
    mod_lo <- as.data.frame(spline(dfpred$Year, dfpred$LowLimit))
    mod_hi <- as.data.frame(spline(dfpred$Year, dfpred$HighLimit))
  } else {
    dfpred <- mod$result_object$modelresults$mod_lin_yFit
    mod_est <- as.data.frame(spline(dfpred$Year, dfpred$Estimate))
    mod_lo <- as.data.frame(spline(dfpred$Year, dfpred$LowLimit))
    mod_hi <- as.data.frame(spline(dfpred$Year, dfpred$HighLimit))
  }  
  
  #"browser()
  
  # Model data
  mod_lohi <- tibble(x = mod_lo$x, y_min = mod_lo$y, y_max = mod_hi$y)
  
  # Station names (from station code)
  stationname <- lookup_stations$Report_version_name[lookup_stations$STATION_CODE %in% station]
  
  # Parameter names (from parameter code)
  paramname <- lookup_params$Param_name[lookup_params$PARAM %in% param]
  if (length(paramname) == 0){
    paramname <- param
  }
  
  # Species ordinary name (from latin name)
  speciesname <- lookup_species$Species_name[lookup_species$LATIN_NAME %in% species]
  

  list(df_data = df_data, mod_est = mod_est, mod_lohi = mod_lohi, 
       log_transform = log_transform, 
       param = param, basis = basis, species=species, tissue=tissue, station=station,
       stationname = stationname, paramname = paramname, speciesname = speciesname)
}


# Test 
# D <- c("HBCDA", "Mytilus edulis", "Whole soft body", "30A",  "WW")
# X <- get_plotdata(D[1], D[2], D[3], D[4], D[5])
# str(X, 1)
  
#
# Given unit, basis and param, make a suitable "unit text" for y axis label
#
get_unit_text <- function(unit, basis, param){
  # Unit
  if (param %in% "VDSI"){                                  # special treatment of VDSI
    unit_print <- "VDS average"    
  } else if (basis %in% "WW" & unit %in% "MG_P_KG"){       # if it is something else than VDSI, we check basis and unit
    unit_print <- "mg/kg (w.w.)"
  } else if (basis %in% "WW" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(w.w.))
  } else if (basis %in% "DW" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (d.w.)"
  } else if (basis %in% "DW" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(d.w.))
  } else if (basis %in% "FB" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (lipid basis)"
  } else if (basis %in% "FB" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(lipid~basis))
  } else if (basis %in% "WWa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (w.w.) for 50 cm cod"
  } else if (basis %in% "WWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(w.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "DWa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (d.w.) for 50 cm cod"
  } else if (basis %in% "DWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(d.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "FBa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (lipid basis) for 50 cm cod"
  } else if (basis %in% "FBa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(lipid~basis)~plain("for 50 cm cod"))
  } else {
    unit_print <- unit
  }
  unit_print
}

#
# Used by plot_medians_and_trends
#
# Default colors are from Colorbrewer2.org, "Set1", except the light blue which is from "Paired"
#
# Color options for trend/CI
#   Dark/light green: trend_color = "#33a02c", ci_fill = "#b2df8a"  (colorbrewer: PuBuGn)
#   Dark/light blue: trend_color = "#045a8d", ci_fill = "#bdc9e1" (colorbrewer: Blues )
plot_medians_color <- function(X, proref,
                 title = NULL, 
                 include_zero = TRUE, proref_x = NULL, show_proref = 1:5, 
                 xlim = NULL, ylim = NULL, ylim_proref = NULL,
                 trend_color = "black", trend_size = 2,    # For eqs_type = "background", use e.g. dark/light green for trends:
                 ci_fill = "grey80",                        # trend_color = "#33a02c", ci_fill = "#b2df8a"  (rom  PuBuGn)
                 points_color = "black", points_fill = "grey30", points_size = 3, points_shape = 21, 
                 points_color_loq = "black", points_fill_loq = "#a6cee3", points_size_loq = 3, points_shape_loq = 25,  # light blue
                 eqs = NULL, eqs_type = "line",                                           # eqs and type of EQS to show (background or line)
                 eqs_x = proref_x,                                                        # x position of the text "EQS" 
                 eqs_label_transparent = TRUE,                                            # if TRUE, 'EQS' will be on a white rectangle 
                 proref_label_transparent = TRUE,                                         # if TRUE, 'PROREF ...' will be on a white rectangle 
                 color_eqs_line = "#e31a1c",                                              # color of EQS line (background)
                 color_below_eqs = "#c6dbef", color_above_eqs = "#fc9272",                # color of EQS rectangles (background)
                 points_color_eqs_under = "black", points_fill_eqs_under = "#377eb8",     # blue
                 points_color_eqs_over = "black", points_fill_eqs_over = "#d7301f"){      # red
  
  # points_fill = "#737373"
  
  if (!is.null(eqs)){
    if (length(eqs) == 0)
      eqs <- NULL
    if (length(eqs) > 0 & is.na(eqs))
      eqs <- NULL
  }
  if (is.null(xlim))
    xlim <- range(X$df_data$MYEAR, na.rm = TRUE) + c(-1,0)
  if (is.null(title))
    title <- paste0(X$param, " (", X$basis, ") in ", X$species, " ", X$tissue, ", ", X$station)
  if (is.null(proref_x)){
    proref_x <- rep(xlim[1], 5)
  }
  
  if (is.null(ylim) & X$log_transform)
    ylim_result <- range(exp(X$mod_lohi$y_max), na.rm = TRUE)
  if (is.null(ylim) & !X$log_transform)
    ylim_result <- range(X$mod_lohi$y_max, na.rm = TRUE)
  if (!is.null(ylim))
    ylim_result <- ylim
  if (!is.null(ylim_proref))
    ylim_result <- range(ylim_result, ylim_proref*proref)
  if (include_zero)
    ylim_result <- range(0, ylim_result)

  # Get unit to print on y axis
  unit_print <- get_unit_text(tail(X$df_data$UNIT, 1), X$basis, X$param)
  
  txt_lim <- data.frame(
    MYEAR = proref_x, 
    Median = c(1,2,5,10,20)*proref, 
    txt = c("PROREF", "PROREF x 2", "PROREF x 5", "PROREF x 10", "PROREF x 20")
  )
  
  gg <- ggplot(X$df_data, aes(x = MYEAR))
  
  # EQS background (if applicable)
  if (!is.null(eqs) & eqs_type %in% "background"){
    if (eqs < ylim_result[2]){           # if EQS is "below the upper edge" -> blue + red
      gg <- gg +
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = ylim_result[1], ymax = eqs), fill = color_below_eqs) +
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = eqs, ymax = ylim_result[2]), fill = color_above_eqs)
    } else if (!is.null(eqs) & eqs_type %in% "background" & eqs >= ylim_result[2]){   # if EQS is "above the upper edge" -> blue only
      gg <- gg +
        geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = ylim_result[1], ymax = ylim_result[2]), fill = color_below_eqs)
    }
  }
  # Trend lines
  if (X$log_transform){
    gg <- gg +  
      geom_ribbon(data = X$mod_lohi, aes(x=x, ymin = exp(y_min), ymax = exp(y_max)), fill = ci_fill) +
      geom_line(data = X$mod_est, aes(x=x, y=exp(y)), color = trend_color, size = trend_size)
  } else {
    gg <- gg +  
      geom_ribbon(data = X$mod_lohi, aes(x=x, ymin = y_min, ymax = y_max), fill = ci_fill) +
      geom_line(data = X$mod_est, aes(x=x, y=y), color = trend_color, size = trend_size)
    
  }
  sel_detlimit <- with(X$df_data, Over_LOQ <= N*0.5) 
  if (!is.null(eqs)){
    sel_over_eqs <- X$df_data$Median > eqs
  } else {
    sel_over_eqs <- rep(FALSE, nrow(X$df_data))
  }
  if (!is.null(eqs)){
  gg <- gg + 
    geom_point(data = X$df_data[sel_over_eqs,], aes(y = Median), 
               shape = points_shape, color = points_color_eqs_over, fill = points_fill_eqs_over, size = points_size) +
    geom_point(data = X$df_data[!sel_over_eqs & !sel_detlimit,], aes(y = Median), 
               shape = points_shape, color = points_color_eqs_under, fill = points_fill_eqs_under, size = points_size) +
    geom_point(data = X$df_data[!sel_over_eqs & sel_detlimit,], aes(y = Median), 
               shape = points_shape_loq, color = points_color_eqs_under, fill = points_fill_eqs_under, size = points_size_loq)
  } else {
    gg <- gg + 
      geom_point(data = X$df_data[!sel_detlimit,], aes(y = Median), 
                 shape = points_shape, color = points_color, fill = points_fill, size = points_size) +
      geom_point(data = X$df_data[sel_detlimit,], aes(y = Median), 
                 shape = points_shape_loq, color = points_color_loq, fill = points_fill_loq, size = points_size_loq)
  }
  gg <- gg + 
    coord_cartesian(xlim = xlim, ylim = ylim_result) +  
    labs(y = unit_print, x = "") +
    theme_bw() +
    theme(axis.title.y = element_text(size = rel(1.2)))

  # EQS line (if applicable)
  if (!is.null(eqs) & eqs_type != "background")
    gg <- gg +
    geom_hline(yintercept = eqs, color = color_eqs_line, size = rel(2)) +
    # 'EQS' label 
    # - note that 'vjust' determines the height above the EQS line (more negative = higher)
    if (eqs_label_transparent){
    annotate("text", x = eqs_x, y = eqs, label = "EQS", hjust = 0, vjust = -0.5, size = 5, 
             color = color_eqs_line)
    } else {
      annotate("label", x = eqs_x, y = eqs, label = "EQS", hjust = 0, vjust = -0.5, size = 5, 
               color = color_eqs_line, label.padding = unit(0.1, "lines"), label.size = 0)
    }
  
  # PROREF lines
  if (!is.na(proref) & proref_label_transparent){
    for (i in show_proref){
      gg <- gg +
        geom_hline(yintercept = txt_lim[i, "Median"], linetype = 2, col = "grey25") +
        geom_text(data = txt_lim[show_proref,], aes(y = Median, label = txt), hjust = 0, vjust = -0.5, size = 3)
    }
      
    } else if (!is.na(proref) & !proref_label_transparent){
      for (i in show_proref){
        gg <- gg +
          geom_hline(yintercept = txt_lim[i, "Median"], linetype = 2, col = "grey25") +
          geom_label(data = txt_lim[show_proref,], aes(y = Median, label = txt), hjust = 0, vjust = -0.5, size = 3, label.size = 0)
      }
    }
  
    gg
    
  }

# debugonce(test)

# gg <- gg + 
#   geom_point(aes(y = Det_limit), shape = points_shape_loq, color = points_color_loq, size = points_size_loq) +
#   geom_point(shape = points_shape, color = points_color, size = points_size) +
#   labs(y = unit_print, x = "") +
#   coord_cartesian(xlim = xlim, ylim = ylim_result) +  
#   theme_bw()


#
# Adds trend symbols to plot; by defaoult in the top left corner (given by x_rel and y_rel)
#
# NOTE: depends on 
#   library(extrafont)
# and (if not already done)
#   font_import(pattern = "Wingdings.ttf")
#
# NOTE 2: only 3 symbols are implemented!
# 
#
add_trend <- function(gg, 
                      trendsymbols = c("arrowup", "arrowdown"),
                      fontsize = 14, 
                      x_rel = 0.8, y_rel = 0.93, x_spacing = c(0, 0.035, 0.070),
                      windows = FALSE){
  library(extrafont)
  # xr <- ggplot_build(gg)$layout$panel_ranges[[1]]$x.range
  # yr <- ggplot_build(gg)$layout$panel_ranges[[1]]$y.range
  xr <- ggplot_build(gg)$layout$coord$limits$x
  yr <- ggplot_build(gg)$layout$coord$limits$y
  xd <- diff(xr)
  yd <- diff(yr)
  x_pos <- xr[1] + x_rel*xd + x_spacing*xd
  # x_pos <- base::as.Date(x_pos, origin = "1970-01-01")
  y_pos <- yr[1] + y_rel*yd
  # For Windows PCs
  if (windows){
    # txt <- case_when(
    #   trendsymbols == "arrowup" ~ "h",
    #   trendsymbols == "arrowdown" ~ "i",
    #   trendsymbols == "circle" ~ "\u2B58"
    # )
    # fontfamily <- case_when(
    #   trendsymbols == "arrowup" ~ "Wingdings 3",
    #   trendsymbols == "arrowdown" ~ "Wingdings 3",
    #   trendsymbols == "circle" ~ "Wingdings 1"
    # )
    txt <- case_when(
      trendsymbols == "arrowup" ~ "\u2191",    # Wingdings 3, h
      trendsymbols == "arrowdown" ~ "\u2193",  # Wingdings 3, i
      trendsymbols == "circle" ~ "\u26AA",     # Wingdings, U+2B58 
      trendsymbols == "star" ~ "\u2606",
      trendsymbols == "square" ~ "\u25FE"
    )
    fontsize_rel <- case_when(
      trendsymbols == "arrowup" ~ 0.8,
      trendsymbols == "arrowdown" ~ 0.8,
      trendsymbols == "circle" ~ 0.7,
      trendsymbols == "star" ~ 1,
      trendsymbols == "square" ~ 1.3
    )
    # gg <- gg +
    #   annotate("text", x = x_pos[1], y = y_pos, label = txt[1], hjust = 0.5, vjust = 0.5, size = fontsize, family = fontfamily[1]) +
    #   annotate("text", x = x_pos[2], y = y_pos, label = "/", hjust = 0.5, vjust = 0.5, size = fontsize) +
    #   annotate("text", x = x_pos[3], y = y_pos, label = txt[2], hjust = 0.5,  vjust = 0.5, size = fontsize, family = fontfamily[2])
    gg +
      annotate("text", x = x_abs[1], y = y_abs, label = txt[1], adj = 0.5, size = fontsize*fontsize_rel[1]) +
      annotate("text", x = x_abs[2], y = y_abs, label = "/", adj = 0.5, size = fontsize) +
      annotate("text", x = x_abs[3], y = y_abs, label = txt[2], adj = 0.5, size = fontsize*fontsize_rel[2])
    
    
    # For Linux (e.g. Jupyterhub)
  } else {
    txt <- case_when(
      trendsymbols == "arrowup" ~ "\u2191",
      trendsymbols == "arrowdown" ~ "\u2193",
      trendsymbols == "circle" ~ "\u25CB",
      trendsymbols == "filledsquare" ~ "\u25FE"  # 25FE
    )
    relsize <- case_when(
      trendsymbols == "filledsquare" ~ 0.55,
      TRUE ~ 1
    )
    gg <- gg +
      annotate("text", x = x_pos[1], y = y_pos, label = txt[1], hjust = 0.5, vjust = 0.5, size = fontsize*relsize[1]) +
      annotate("text", x = x_pos[2], y = y_pos, label = "/", hjust = 0.5, vjust = 0.7, size = round(fontsize*0.8, 0)) +
      annotate("text", x = x_pos[3], y = y_pos, label = txt[2], hjust = 0.5, vjust = 0.5, size = fontsize*relsize[2])
  
    }
  gg
}



#
# Get data from big excel
#
get_dfbig <- function(param, species, tissue = NULL, station, basis = "WW"){
  data_xl_lessthans %>% 
    filter(PARAM == param & LATIN_NAME == species & TISSUE_NAME == tissue & STATION_CODE == station &  Basis == basis)
}


#
# Saving plots
# 

save_trendplot <- function(plot_single_result, folder, suffix = ""){
  fn <- paste0(folder, "/", plot_single_result$fn)
  # Add suffix to file name (note: works only on png and jpg!)
  fn <- sub(".png", paste0(suffix, ".png"), fn)
  fn <- sub(".jpg", paste0(suffix, ".jpg"), fn)
  ggsave(fn, plot_single_result$gg, height = 4.4, width = 5.6, dpi = 450)
  invisible(fn)   # returns file name invisibly
}

# save_trendplot(X, "Figures_41")


#
# Inserts line shift ("\n" character) in string where the string contains space or comma
#

# If where_split is between 0 and 1, it refers to "fraction" in each part
# So if where_split = 0.5, it splits the string approximately n the middle
# If where_split is > 1, it refers to number of characters in the first part
# If type = 'closest', finds space/comma closest to the split you want
# If type = 'max', sets split so the first part is max 'where_split' characters
#   (not including comma)

break_text <- function(txt, 
                       where_split = 0.5,   # Can also be a number
                       type = "closest",
                       no_break_after = NULL){   # 'closest' or 'max'
  
  # Find middle of txt, if that is what we want
  if (where_split <= 1){
    where_split <- nchar(txt)*where_split
  }
  
  # We can split on spaces or on commas
  x_space <- data.frame(x = gregexpr(" ", txt, fixed = TRUE)[[1]], 
                        char = "space", 
                        stringsAsFactors = FALSE)
  x_comma <- data.frame(x = gregexpr("\\,", txt)[[1]], 
                        char = "comma", 
                        stringsAsFactors = FALSE)
  # Combine and sort
  df <- rbind(x_space, x_comma)
  df <- df[order(df$x),]
  
  if (!is.null(no_break_after)){
    for (no_break_txt in no_break_after){
      i <- gregexpr(no_break_txt, txt, fixed = TRUE)[[1]] + nchar(no_break_txt)
      df <- df[!df$x %in% i,]
    }
  }
  
  # df
  if (type == "closest"){
    # Find 'x' closest to the where_split
    index <- which.min(abs(df$x - where_split))
  } else if (type == "max"){
    # Find the last 'x' before where_split
    index <- max(which(df$x - where_split <= 0))
  }
  
  # df[index,]
  # If comma followed by space, replace space by line shift
  if (df$char[index] == "comma" & 
      df$char[index+1] == "space" & 
      df$x[index+1] - df$x[index] == 1){
    result <- paste0(substr(txt, 1, df$x[index]), "\n", substr(txt, df$x[index]+2, nchar(txt)))
    # If comma not followed by space, insert line shift
  } else if (df$char[index] == "comma"){
    result <- paste0(substr(txt, 1, df$x[index]), "\n", substr(txt, df$x[index]+1, nchar(txt)))
    # If space, replace space by line shift
  } else if (df$char[index] == "space"){
    result <- paste0(substr(txt, 1, df$x[index]-1), "\n", substr(txt, df$x[index]+1, nchar(txt)))
  }
  result
}


if (FALSE){

  # Test without space after comma
  txt <- "Mercury (Hg) in cod muscle,Bømlo,Outer Selbjørnfjord (st. 23B)"
  break_text(txt)
  # "Mercury (Hg) in cod muscle,Bømlo,\nOuter Selbjørnfjord (st. 23B)"
  
  # Test with space after comma
  txt <- "Mercury (Hg) in cod muscle, Bømlo, Outer Selbjørnfjord (st. 23B)"
  break_text(txt)
  # "Mercury (Hg) in cod muscle,Bømlo,\nOuter Selbjørnfjord (st. 23B)"
  
  # Split as close to 26 characters as possible
  break_text(txt, 19)
  # "Mercury (Hg) in cod\nmuscle, Bømlo, Outer Selbjørnfjord (st. 23B)"

  # Split so part before line shift is max. 26 characters
  break_text(txt, 19, type = "max")
  # "Mercury (Hg) in\ncod muscle, Bømlo, Outer Selbjørnfjord (st. 23B)"
  
  # Split string so the first part is approximately 2x as long as the second part
  break_text(txt, 2/3)
  # "Mercury (Hg) in cod muscle, Bømlo, Outer\nSelbjørnfjord (st. 23B)"

  # Avoid break after a certain string. Here: avoid break between
  #   "st." and the station number
  break_text(txt, 60, type = "max")
  break_text(txt, 60, type = "max", no_break_after = "st.")
}


