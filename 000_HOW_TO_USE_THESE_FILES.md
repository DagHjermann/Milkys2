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
**Summary of workflow: 050 -> 051 -> 080 -> 101 -> 103 -> 105 -> 109 -> 110 -> 111 -> 120 -> 201**  
- Script 050 - Add cod biological effects data to Nivabasen     
- Script 051 - Add imposex data to Nivabasen     
- Script 080 - Download last year's data from Nivabasen     
- Script 101 - Takes the downloaded Nivabasen data for last year (from 080) and combines them
with the legacy data from last year. Also makes concentrations on dry-weight and
fat basis
- Script 103 - Add sum parameters for all years    
- Script 105 - Add measurement uncertainty    
- Script 109 - Combines the data (from 101) with fish length data (from 105) and makes length-adjusted concentrations  
- Script 110 - Makes median data (per station/year) based on the data (from 109) and adds PROREF   
- Script 111 - Makes some information measures on sample size etc. that are used by script 201 
- Script 120 - Calculates time trends based on the medians (from 110)  
- Script 201 - Makes the big excel file based on medians (from 110) and time trends (from 120)  

## Main script files  
#### Scripts 001-049: Scripts containing functions used by several other scripts    
001_Add_trends_functions.R               
002_Utility_functions.R


#### Scripts 050-049: Scripts for inserting data into Nivabasen, and reading data from Nivabasen

Read data from excel and insert into Nivabasen (thus, must be used *before* 080):  
* 050: Biological effects and bile metabolites in cod    
* 051: Imposex and intersex data  
    - For both of these, you need username/password to read and write from the Oracle Nivadatabase, as well as SQL Developer installed on your computer (talk to Jens)    
    - These scripts are run on Jupyterhub, but they create SQL code scripts that must be downloaded to your PC and then opened in SQL Developer in order to insert data into the database.    
    - These scripts must be run step by step: Run the code until you have created SQL script number 1. Then download and run this script in SQL Developer to insert data into a table. Then go back to the script and continue from where you stopped, until the script creates SQL script number 2. Then download and run this script in SQL Developer, etc. The reason for this is that when running SQL script number 1, new ID values are created in the process which you need to read back using R before you can create SQL script number 2.

Read data from Nivabasen:  
* 080 Reads Milkys data from Nivabasen   
    - creating `01_df_2020_notstandard_...` RData file, used by script 101   
    - needs username/password to read from the Oracle Nivadatabase (talk to Jens)    

NILU data  
* Should now be submitted in a template by NILU, which then is imported into LIMS (and thereby Nivabase) by Dorna/Viviane.  


#### Scripts 100-149: Main pipeline for collecting data and calculating all medians and trends <br> (Uses data from Nivabasen, and possibly also extra data files made using scripts 150-199)   
101_Combine_with_legacy_data_2019.Rmd (uses data downloaded on a pc - see 'Scripts 800-899' below)              
101_Combine_with_legacy_data_functions.R (functions used by the file above)      
109_Adjust_for_fish_length_functions.R (functions used by the file above)       
109_Adjust_for_fish_length.Rmd               
110_Medians_and_PROREF.Rmd             
111_Nstring_SD_DDI.Rmd
120_Calculate_trends.Rmd                    

NOTE: the following two scripts are not used, as Aquamonitor doen't prduce all the data we need:  
- 100_Download_Aquamonitor_data.Rmd            
- 100_Download_Aquamonitor_data_R_functions.R (functions used by the file above)   

#### Scripts 150-199: Scripts for reading data from other sources and (optionally) make SQL for insertion into NIVAbase  
*NOTE: These are not in use, they have been replaced by scripts in Milkys2_pc** (see 'Scripts 800-899' below)  
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
  
#### Scripts 800-899: Reserved for scripts in the project Milkys2_pc      

These scripts are used 'locally', i.e. on your own computer  
* Can be found in https://github.com/NIVANorge/Milkys2_pc   
   
Important scripts to make files *used by* Milkys2 scripts    
* 801 Reads Labware data from the database   
    - creating `Labware_samples_xxxx` rds file, used by scripts 109, 111 and 201               
    - Must be run on a PC  
    - the files resulting from this script is automatically saved in the `Files_to_Jupyterhub_xxxx` folder on your computer, and then you must manually uploaded it to Jupyterhub (folder `Input_data`)   
    - need an Oracle client and access to the Oracle Nivadatabase  

Important scripts that uses files *produced by* Milkys2 scripts:      
* 841-844: Submission to ICES      
Files used by these scripts must first be manually downloaded from Jupyterhub and manually saved to the `Files_from_Jupyterhub_xxxx` folder on your computer. These scripts are used to make a data file (text format) which is tested on ICES' DATSU service (https://dome.ices.dk/datsu/)  and then sent to accessions@ices.dk   
  
In addition, some csv + excel files produced by Milkys2 scripts are manipulated on your PC using Excel macros. This includes  
* Script 201 produces `Data_xl ... .csv`       
* Script 210 produces `Summarytable_trend from R ... .xlsx` and `Summarytable_EQS from R ... .xlsx`    
These files are manually downloaded to your pc and saved to the `Files_from_Jupyterhub_xxxx` folder on your computer. Detailed instructions for manipulation are found in the two scripts (201 and 210)  


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

If you don't see a graph here, click the small "Trust HTML" button in the top left corner of this window.




```{=html}
<div class="grViz html-widget html-fill-item" id="htmlwidget-97fedb7371032abcfeac" style="width:864px;height:1248px;"></div>
<script type="application/json" data-for="htmlwidget-97fedb7371032abcfeac">{"x":{"diagram":"\ndigraph boxes_and_circles {\n\n  # a \"graph\" statement\n  graph [overlap = true, fontsize = 10]\n\n  # Scripts\n  node [shape = ellipse,\n        fixedsize = true,\n        color = IndianRed,\n        fontcolor = IndianRed,\n        width = 1.5] // ellipse\n  801 [label = \"Script 801\", width = 2, \n    tooltip = \"Downloading sample data from LIMS/Nivabasen (must run on a PC)\"]; \n  LastYear [label = \"Script 101,\nrun last year\n\", height = 0.6, width = 2]; \n  080 [label = \"Script 080 (PC)\", tooltip = \"Download all Milkys data from last year from Nivabasen. Needs username/password for reading from the Oracle Nivadatabase\"];\n  101 [label = \"Script 101\"];\n  103 [label = \"Script 103\"];\n  150 [label = \"Script 150\"]; \n  109 [label = \"Script 109\"]; \n  812 [label = \"Imported by lab\n(Viviane/Dorna)\", width = 2]; \n  051 [label = \"Script 051\", width = 2]; \n  050 [label = \"Script 050\n(on PC)\", width = 2]; \n  110 [label = \"Script 110\"]; \n  111 [label = \"Script 111\"]; \n  120 [label = \"Script 120\"]; \n  201 [label = \"Script 201\"]; \n  501 [label = \"Script 501\"]; \n  510 [label = \"Script 510\"]; \n  download_1 [label = \"Downloaded to pc\", width = 2]; \n  download_2 [label = \"Downloaded to pc\", width = 2]; \n  \n  # Data sets\n  node [shape = box,\n        color = RoyalBlue,\n        fontcolor = RoyalBlue,\n        fontname = Helvetica]\n  NILU_excel [label = \"NILU excel\", \n    width = 2, fillcolor = MistyRose, penwidth = 2];\n  Imposex_excel [label = \"Imposex excel\", \n    width = 2, fillcolor = MistyRose, penwidth = 2];\n  Codbiol_excel [label = \"Cod biol. effects excel\", \n    width = 2, fillcolor = MistyRose, penwidth = 2];\n  Nivadatabase [label = \"Nivadatabase\", \n    height = 0.6, width = 2, fillcolor = PaleGreen, penwidth = 2];\n  Labware_db [label = \"Labware\ndatabase\", \n    height = 0.6, width = 2, fillcolor = PaleGreen, penwidth = 2];\n  Lengths_excel [label = \"Fish length data\n(excel, uploaded to Jupyterhub)\", \n    height = 0.6, width = 3, penwidth = 2];\n  Labware [label = \"Samples and pooling\n(uploaded to Jupyterhub)\", \n    height = 0.6, width = 3];\n  Legacy [label = \"Raw data produced last year\n(legacy data)\",\n    height = 0.6, width = 3];\n  Nivabasen [label = \"Last year\\\"s data from Nivabase\n(uploaded to Jupyterhub)\",\n    height = 0.6, width = 3];\n  NILU [label = \"NILU data\", width = 2];\n  Imposex [label = \"Imposex data\", width = 2];\n  Codbiol [label = \"Cod biological effects\", width = 2];\n  Raw2 [label = \"Raw concentration data\n(all years)\", width = 2.5];\n  Raw2b [label = \"Data with sums\", width = 2.5];\n  Lengths [label = \"Fish length data\", width = 2.5];\n  Raw3 [label = \"Raw data with length-adjusted conc.\", width = 3.5];\n  Medians [label = \"Medians per station/year\", width = 2.5];\n  Nstring [label = \"Nstring, SD and DDI\", width = 2.5];\n  Trends [label = \"Trends per time series\", width = 2.5];\n  Bigexcel_rmd [label = \"Big excel file (rmd)\", width = 2.5];\n  Bigexcel_csv [label = \"Big excel file (csv)\", width = 2.5];\n  Bigexcel [label = \"Big excel file (xlsx)\n(manipulated using macros)\", \n    height = 0.6, width = 2.5];\n  Graphs [label = \"Time series graphs\", width = 2.5];\n  Tables_raw [label = \"Summary tables (xlsx)\", width = 2.5];\n  Tables [label = \"Summary tables (xlsx)\n(manipulated using macros)\", \n    height = 0.6, width = 3];\n\n  # Connections (edges)\n  NILU_excel -> 812; 812 -> NILU; \n  Imposex_excel -> 051; 051 -> Imposex; \n  Codbiol_excel -> 050; 050 -> Codbiol;\n  {NILU Imposex Codbiol} -> Nivadatabase;\n  Nivadatabase -> 080;\n  Labware_db -> 801; 801 -> Labware; \n  LastYear -> Legacy; 080 -> Nivabasen\n  {Nivabasen Legacy} -> 101; \n  101 -> Raw2; \n  Raw2 -> 103; \n  103 -> Raw2b; \n  Lengths_excel -> 150; 150 -> Lengths; \n  {Raw2b Lengths Labware} -> 109;\n  109 -> Raw3; Raw3 -> 110; 110 -> Medians; \n  Raw3 -> 111; Labware -> 111; 111 -> Nstring; \n  Medians -> 120; 120 -> Trends;\n {Raw2 Raw3 Medians Trends Nstring} -> 201; 201 -> Bigexcel_rmd\n  Bigexcel_rmd -> {501 510}; 501 -> Graphs; 510 -> Tables_raw;\n  Bigexcel_rmd -> Bigexcel_csv;\n  Bigexcel_csv -> download_1; download_1 -> Bigexcel\n  Tables_raw -> download_2; download_2 -> Tables\n  \n  subgraph {rank = same; Medians; Nstring}\n\n\n }\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```



