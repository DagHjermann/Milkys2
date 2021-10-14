# Milkys2  
Scripts for the *MILKYS* project (Norway's implementation of OSPAR CEMP) for running on Jupyterhub. This repo is an RStudio project that runs the main data flow / manipulation as well as making time series graphs and a cuple of the tables for the annual "Contaminants in coastal waters of Norway" report (including the big Excel table give to Miljødirektoratet as an Appendix to the annual report).   

## Getting started  
1. In JupyterLab, start RStudio from the RStudio icon (in the Launcher tab). NOTE: If there is no Launcher tab,select "File : New Launcher" in the menu.   
2. In RStudio, choose **file:Open Project** and open `Milkys2.Rproj` in the folder `Shared/DHJ/Milkys2`. (Or try **File:Recent Projects**). The work flow starts with script 100 (`100_Download_Aquamonitor_data.Rmd`), see below. See [this minimal guide](https://github.com/DagHjermann/Milkys2#how-to-use-rstudio) for getting started in RStudio.     

## Work flow for the Milkys project   
*For a more thorough overview, see [000_HOW_TO_USE_THESE_FILES](000_HOW_TO_USE_THESE_FILES.html)*  
This RStudio project has a "sibling" RStudio project/repo called *'Milkys2_pc'* which needs to run on your own computer. The workflow can be summarized as follows (JH = Jupyterhub):  
  
**[Download NIVAdatabase data on pc] -> [Upload to JH] -> [Data manipulation in JH] -> [Download results to pc]**  
  
The *'Data manipulation in JH'* part can be summarized as follows, using script numbers:    
**NIVAdatabase data -> 101 -> 109 -> 110 -> 111 -> 120 -> 201 -> graphs, tables, big excel table**  

The main scripts in this used are  
- [Script 101](101_Combine_with_legacy_data_2020.Rmd) - Takes the Nivadatabase data for the last year (created in *Milkys2_pc* and uploaded to Jupyterhub) and combines them with the 'legacy data', i.e. the data produced by script 101 last year. Also makes concentrations on dry-weight and fat basis  
- [Script 109](109_Adjust_for_fish_length.Rmd) - Combines the concentratkion data (from 101) with fish length data and makes length-adjusted concentrations   
- [Script 110](110_Medians_and_PROREF.Rmd) - Makes median data (per station/year) based on the data (from 109) and adds PROREF   
- [Script 111](111_Nstring_SD_DDI.Rmd) - Makes some information measures on sample size etc. that are used by script 201 
- [Script 120](120_Calculate_trends.Rmd) - Calculates time trends based on the medians (from 110)  
- [Script 201](201_Make_big_excel_file.Rmd) - Makes the big excel file based on medians (from 110) and time trends (from 120)    
- [Script 401](401 Plot time series.Rmd) - Make time series plots  

#### Other files  
**Milkys2.Rproj** - the *project file* that you open using RStudio                                
**README.md** - (this file!) - the "home page" seen when you look at this project (repo) in Github       
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
   
