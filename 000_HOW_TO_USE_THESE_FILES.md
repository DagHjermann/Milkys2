---
title: "How to use the Milkys2 R scripts"
output: 
  html_document:
    keep_md: true
    toc: true
---

How to use the scripts in `shared/DHJ/Milkys2` 

## Getting started  
1. In JupyterLab, start RStudio from the RStudio icon (in the Launcher tab). NOTE: If there is no Launcher tab,select "File : New Launcher" in the menu.   
2. In RStudio, choose **File:Recent Projects** (in the menu) and choose **Milkys2** (or navigate to this folder and open `Milkys2.Rproj`)   

## How to use RStudio  
- The scripts to use are those whose file name (extension) is **.Rmd**  
- Scripts open in the **script window** (usually the upper left part of RStudio)  
- Code is found in **code chunks** (grey areas of the document, starting with `{r}`). The scripts are (usually) run by running all code chunks in order  
- To run **parts of a code chunk**, mark one or several lines of code and use **Ctrl-Enter** (if nothing is marked, it runs the line where your cursor is)  
- To run an **entire code chunk**, use **Ctrl-Shift-Enter** (or use the small "run" menu in the top right of the script window)  
- To run the **next code chunk**, use **Ctrl-Alt-N** (or the "Run" menu, see above)    
- To run **all code chunks**, use **Ctrl-Alt-R** (or the "Run" menu)    
- To run all code chunks and also **create/update html and md files**, use the "Knit" button in the top left of the script window. (For instance, if you read this in a browser, you are looking at the file `000_HOW_TO_USE_THESE_FILES.html` made by opening `000_HOW_TO_USE_THESE_FILES.Rmd` and clicking "Knit".)     
- In an Rmd (R markdown) script, **Ctrl+Shift+O** opens a clickable table of contents    
  


## Work flow for the Milkys project   
**Summary of workflow: 100 -> 101 -> 109 -> 110 -> 111 -> 120 -> 201**  
  (below, 'Script 100' refers to the file starting with '100' and ending with 'Rmd')
- Script 100 - Downloads Aquamonitor data, as an Excel file (with several sheets)  
- Script 101 - Takes the Aquamonitor data for last year (from 100) and combines them
with the old data (up to 2018). Also makes concentrations on dry-weight and
fat basis
- Script 109 - Combines the data (from 101) with fish length data (from 105) and makes length-adjusted concentrations  
- Script 110 - Makes median data (per station/year) based on the data (from 109) and adds PROREF   
- Script 111 - Makes some information measures on sample size etc. that are used by script 201 
- Script 120 - Calculates time trends based on the medians (from 110)  
- Script 201 - Makes the big excel file based on medians (from 110) and time trends (from 120)  

## Main script files  
#### Scripts 001-099: Scripts containing functions used by several other scripts    
001_Add_trends_functions.R               
002_Utility_functions.R
  
#### Scripts 100-149: Main pipeline for collecting data and calculating all medians and trends <br> (Uses data from Nivabasen, and possibly also extra data files made using scripts 150-199)   
100_Download_Aquamonitor_data.Rmd            
100_Download_Aquamonitor_data_R_functions.R (functions used by the file above)   
101_Combine_with_legacy_data_2019.Rmd             
101_Combine_with_legacy_data_functions.R (functions used by the file above)      
109_Adjust_for_fish_length_functions.R (functions used by the file above)       
109_Adjust_for_fish_length.Rmd               
110_Medians_and_PROREF.Rmd             
111_Nstring_SD_DDI.Rmd
120_Calculate_trends.Rmd                    
  
#### Scripts 150-199: Scripts for reading data from other sources and (optionally) make SQL for insertion into NIVAbase  
150_Read_fish_individual_data_2018.Rmd       
150_Read_fish_individual_data_2019.Rmd      
150_Get_fish_individual_data_functions.R (functions used by the files above)     
161_Read_NILU_excel_data_2019.R                   
161_Read_NILU_excel_data_functions.R (functions used by the files above)               
171_Read_imposex_data_2018.Rmd                    
  
#### Scripts 201-299: Scripts for output to report (incl. big excel file)  
201_Make_big_excel_file.Rmd                  
201_Make_big_excel_file_functions.R (functions used by the file above)         
201_test.Rmd                                 
201_Time_series_write_to_Excel_functions.R (functions used by the file above)   
210_Make_overview_tables.Rmd  
210_Make_overview_tables_functions.R (functions used by the file above)  
  
#### Scripts 401-499: Scripts for making figures 
401 Plot time series.Rmd                     
401 Plot time series functions.R (functions used by the file above)             
401 Plot time series.nb.html                 
  
#### Scripts 501-599: Scripts for non-Milkys use (e.g. Naturindeks)   
501_Naturindeks_bluemussel.Rmd               
  
#### Scripts 800-899: Scripts that doesn't work on Jupyterhub (must be downloaded and run on your local PC)    
801_Download_Labware_sample_data.Rmd         
  
#### Scripts 900-999: Various test scripts etc.  
990 Markdown test.Rmd                        
990-Markdown-test.docx                       
990-Markdown-test.html                       
990-Markdown-test.pdf                        
991_Stuff_to_install.Rmd       

#### Other files  
**Milkys2.Rproj** - the *project file* that you open using RStudio                                
**README.md** - the "home page" seen when you look at this project (repo) in Github       
**CEMP database structures.pptx** - A Powerpoint file showing the most relevant tables of Nivabasen and their relationships. Must be downloaded to your computer in order to use it.      

## Overview of folders  
*Note that all files in `Data` or `Figures` should have a number indicating which script that was used to make it*                                     
  
**Input_data** - Files *used by* by the R scripts. Normally uploaded from a PC to Jupyterhub. Source of the files should be noted in the _README.txt file.  **If you add new files, please edit _README.txt accordingly.**                     
**Data** - Files (often R data files, extenstion `rds`) *produced* by the R scripts. Should be numbered (see above)               
**Data_Nivabasen** - Excel files produced by script 100                                 
**Big_excel_table** - csv files produced by script 201  
**Figures** - Figure files (usually jpg) produced by scripts. Should be numbered (see above)                                         
**Figures_401** - Figure files (usually jpg) produced by script 401  


## Overview of data flow  



<!--html_preserve--><div id="htmlwidget-d2cb844f2179cf4a35fd" style="width:864px;height:1248px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-d2cb844f2179cf4a35fd">{"x":{"diagram":"\ndigraph boxes_and_circles {\n\n  # a \"graph\" statement\n  graph [overlap = true, fontsize = 10]\n\n  # Scripts\n  node [shape = ellipse,\n        fixedsize = true,\n        width = 1.5] // ellipse\n  100 [label = \"Script 100\", \n    tooltip = \"Downloading all data from the project that can be downloaded from Jupyterhub (using the Aquamonitor API)\"]; \n  LastYear [label = \"From last year\"]; \n  802 [label = \"Script 802 (PC)\", tooltip = \"Extra parameters not returned by script 101. Must be run on a PC. Will need an Oracle client and access to the Oracle Nivadatabase\"];\n  101 [label = \"Script 101\"];\n  150 [label = \"Script 150\"]; \n  109 [label = \"Script 109\"]; \n  161 [label = \"Script 161\"]; \n  171 [label = \"Script 171\"]; \n  172 [label = \"Script 172\"]; \n  110 [label = \"Script 110\"]; \n  111 [label = \"Script 111\"]; \n  120 [label = \"Script 120\"]; \n  201 [label = \"Script 201\"]; \n  501 [label = \"Script 501\"]; \n  510 [label = \"Script 510\"]; \n  \n  # Data sets\n  node [shape = box,\n        fontname = Helvetica]\n  Raw1 [label = \"Raw data from AqM\", width = 2];\n  Legacy [label = \"Legacy data\", width = 2];\n  Nivabasen [label = \"Extra data from Nivabase\", width = 2.5];\n  NILU [label = \"NILU data\", width = 2];\n  Imposex [label = \"Imposex data\", width = 2];\n  Codbiol [label = \"Cod biological effects\", width = 2];\n  Raw2 [label = \"Raw data, complete records\", width = 2.5];\n  Lengths [label = \"Fish length data\", width = 2.5];\n  Raw3 [label = \"Raw data with length-adjusted conc.\", width = 3];\n  Medians [label = \"Medians per station/year\", width = 2.5];\n  Nstring [label = \"Nstring, SD and DDI\", width = 2.5];\n  Trends [label = \"Trends per time series\", width = 2.5];\n  Bigexcel [label = \"Big excel file\", width = 2.5];\n  Graphs [label = \"Time series graphs\", width = 2.5];\n  Tables [label = \"Summary tables\", width = 2.5];\n\n  # Connections (edges)\n  100 -> Raw1; LastYear -> Legacy; 802 -> Nivabasen\n  161 -> NILU; 171 -> Imposex; 172 -> Codbiol\n  {Raw1 Nivabasen Legacy NILU Imposex Codbiol} -> 101; \n  101 -> Raw2; 150 -> Lengths; \n  {Raw2 Lengths} -> 109;\n  109 -> Raw3; Raw3 -> 110; 110 -> Medians; \n  Raw3 -> 111; 111 -> Nstring; \n  Medians -> 120; 120 -> Trends;\n {Raw2 Raw3 Medians Trends Nstring} -> 201; 201 -> Bigexcel\n  Bigexcel -> {501 510}; 501 -> Graphs; 510 -> Tables\n  \n  subgraph {rank = same; Medians; Nstring}\n\n\n }\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



