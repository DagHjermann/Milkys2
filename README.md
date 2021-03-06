# Milkys2  
Milkys scripts to run on Jupyterhub. The main aim of these files is to read the relevant contaminant data from Nivabasen, use these to update the contaminant time series, and to produce figures and tables for the annual "Contaminants in coastal waters of Norway" report (including the big Excel table give to Miljødirektoratet as an Appendix to the annual report).   

## Getting started  
1. In JupyterLab, start RStudio from the RStudio icon (in the Launcher tab). NOTE: If there is no Launcher tab,select "File : New Launcher" in the menu.   
2. In RStudio, choose **file:Open Project** and open `Milkys2.Rproj` in the folder `Shared/DHJ/Milkys2`. (Or try **File:Recent Projects**). The work flow starts with script 100 (`100_Download_Aquamonitor_data.Rmd`), see below. See [this minimal guide](https://github.com/DagHjermann/Milkys2#how-to-use-rstudio) for getting started in RStudio.     


## Work flow for the Milkys project   
**Summary of workflow: 100 -> 101 -> 109 -> 110 -> 111 -> 120 -> 201**
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
100_Download_Aquamonitor_data_R_functions.R   
100_Download_Aquamonitor_data.Rmd            
101_Combine_with_legacy_data_functions.R     
101_Combine_with_legacy_data_2019.Rmd             
109_Adjust_for_fish_length_functions.R       
109_Adjust_for_fish_length.Rmd               
110_Medians_and_PROREF.Rmd             
111_Nstring_SD_DDI.Rmd
120_Calculate_trends.Rmd                    
  
#### Scripts 150-199: Scripts for reading data from other sources and (optionally) make SQL for insertion into NIVAbase  
150_Get_fish_individual_data_functions.R     
150_Read_fish_individual_data_2018.Rmd       
150_Read_fish_individual_data_2019.Rmd      
161_Read_NILU_excel_data_2019.R                   
161_Read_NILU_excel_data_functions.R              
171_Read_imposex_data_2018.Rmd                    
  
#### Scripts 201-299: Scripts for output to report (incl. big excel file)  
201_Make_big_excel_file_functions.R          
201_Make_big_excel_file.Rmd                  
201_test.Rmd                                 
201_Time_series_write_to_Excel_functions.R   
  
#### Scripts 401-499: Scripts for making figures 
401 Plot time series functions.R             
401 Plot time series.nb.html                 
401 Plot time series.Rmd                     
  
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

## How to use RStudio  
- Scripts open in the **script window** (usually the upper left part of RStudio)   
- Code is found in **code chunks** (grey areas of the document, starting with `{r}`). The scripts are (usually) run by running all code chunks in order  
- To run **parts of a code chunk**, mark one or several lines of code and use **Ctrl-Enter** (if nothing is marked, it runs the line where your cursor is)  
- To run an **entire code chunk**, use **Ctrl-Shift-Enter** (or use the small "run" menu in the top right of the script window)  
- To run the **next code chunk**, use **Ctrl-Alt-N** (or the "Run" menu, see above)    
- To run **all code chunks**, use **Ctrl-Alt-R** (or the "Run" menu)    
- To run all code chunks and also **create/update html and md files**, use the "Knit" button in the top left of the script window. (For instance, if you read this in a browser, you are looking at the file `000_HOW_TO_USE_THESE_FILES.html` made by opening `000_HOW_TO_USE_THESE_FILES.Rmd` and clicking "Knit".)     
- In an Rmd (R markdown) script, **Ctrl+Shift+O** opens a clickable table of contents    
   
