# Climate-Ratings

Code for the paper "Rising Temperatures, Falling Ratings: The Effect of Climate Change on Sovereign Creditworthiness". 

[![RCP 8.5 2100 Rating Downgrades](https://github.com/mattdburke/Climate-Ratings/blob/main/figures/BS_map_robinson1_v2.jpg)](https://github.com/mattdburke/Climate-Ratings/blob/main/figures/BS_map_robinson1_v2.jpg)

## Instructions for running the code

* Download raw data from [here](https://pubsonline.informs.org/doi/10.1287/mnsc.2023.4869) and add to the `rawdata` folder. Some files are too large to be pushed to GitHub so there is a .gitignore in place.
* Use the `setwd()` command to set your current working directory to the `climate-ratings/` parent directory.
* Run `source("main.r")`

Make sure the packages listed `load_dependencies.r` are installed, the code should do this for you if not. Details relating to the specific versions of the packages are given below as per a `sessionInfo()` command and the repo is accompanied with `renv` commands. 

The running order for the code is given in `main.r`. There are two exceptions to this. First, to produce key figures as discussed in the text you should run the script from `key_figures_in_text.r` directly into the console. The numbers are printed directly and not exported anywhere. The second exception is to produce Figure 6 in the manuscript. This figure is quite sensitive to conflicts from other packages. To reproduce this figure you should run the code from `produce Figure 6.r` in a seperate console altogether without loading the dependences within `load_dependencies.r`, the packages needed to run this script are within the file itself. 

The code takes around 10-12 minutes to complete

## Code and data dictionaries

* Raw data: For descriptions of the raw data click [here](rawdata/data%20dictionary%20and%20instruction.md)
* Clean data: For descriptions of the clean data files produced in data cleaning click [here](cleandata/data_dictionary.md)
* Adjusted ratings by scenario: For descriptions of the adjusted ratings results output click [here](output/data_dictionary.md)
* Code description: For descriptions of the code and it's running order in the `src` folder click [here](src/script_description.md)

## Details on Table, Figure and Results reproduction in order of appearance in the paper

* Figure 2: Run the `createFig2()` command in `figures.r`.
* Table 1: Run the `printTable1()` command in `tables.r`.
* Table 2: Run the `printTable2()` command in `tables.r`.
* Figure 4: Run the `createFig4()` command in `figures.r`.
* Figure 5: Run the `createFig5()` command in `figures.r`.
* Figure 6: Run the `produce Figure 6.r` script on a seperate `R` console to `main.r`.
* Table 3: Run the `printTable3a()`, `printTable3b()`, and `printTable3c()` commands in `tables.r`.
* Figure 7: Run the `createFig7()` command in `figures.r`.
* Figure 8: Run the `robinson_maps_v2.r` script.
* Figure 9: Run the `createFig9()` command in `figures.r`.
* Figure 10: Run the `createFig10()` command in `figures.r`.
* Figure 11: Run the `createFig11()` command in `figures.r`.
* Figures 12 & 13: Run the `robinson_maps_v2.r` script.
* Figure 14: Run the `createFig14()` command in `figures.r`.
* Figure 15: Run the `createFig15()` command in `figures.r`.
* Figure 16: Run the `createFig16()` command in `figures.r`.
* Table 4: Run the `printTable4()` command in `tables.r`.
* Table 5: Run the `costTables("Kahn_2.6_2100_estimates", "sovereignDebt", "G7")` command in `tables.r`.
* Table 6: Run the `costTables("Kahn_8.5_2100_estimates", "sovereignDebt", "G7")` command in `tables.r`.
* Table 7: Run the `costTables("Kahn_2.6_2100_estimates", "corporateDebt", "G7")` command in `tables.r`.
* Table 8: Run the `costTables("Kahn_8.5_2100_estimates", "corporateDebt", "G7")` command in `tables.r`.
* Table C1: Run `printTableC1a()`, `printTableC1b()`, and `printTableC1c()` in `tables.r`.
* Figure C1: Run `createFigC1()` in `figures.r`.
* Figure C2: Run `createFigC2()` in `figures.r`.
* Tables D1, D2, D3 and D4: Run;
  -`costTables("Kahn_2.6_2100_estimates", "sovereignDebt", "FULL")`

  -`costTables("Kahn_8.5_2100_estimates", "sovereignDebt", "FULL")` 
  
  -`costTables("Kahn_2.6_2100_estimates", "corporateDebt", "FULL")` 
  
  -`costTables("Kahn_8.5_2100_estimates", "corporateDebt", "FULL")` 

in `tables.r`

* Table E1 and E2: Run `printTableE("2030")` and `printTableE("2050")`in `tables.r`.

## Folders

* src: Contains all the scripts. Can all be run from the parent folder.
* rawdata: Contains all the raw data files to produce everything else in the parent folder. Details are given in a markdown file in the folder.
* cleandata: Outputs from `datacleaning.r` scripts. Details given in a markdown file in the folder. These files are later used to generate the primary results.
* output: This folder contains all the rating estimates from the primary `analysis.r` scripts.
* figures: Holds all the picture outputs from `figures.r`.
* tables: Holds all of the produced tables which are in `.csv` and `.txt` formats.
* renv: Details on the `R` environment.

## Requirements

This code is accompanied by an `renv` profile. Use the command `renv::restore()` to load the packages. Note this must be done with the `R 3.6.1`. There is also a `sessionInfo()` print out given below.

This code is run on the following R environment
```
R version 3.6.1 (2019-07-05)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rgdal_1.5-23        sp_1.4-4            sjPlot_2.8.10       wesanderson_0.3.6   transformr_0.1.3    gganimate_1.0.8     gtable_0.3.0       
 [8] dichromat_2.0-0.1   colorspace_1.4-1    reshape2_1.4.4      viridis_0.5.1       viridisLite_0.3.0   stringr_1.4.0       caTools_1.18.0     
[15] gridExtra_2.3       DALEX_2.3.0         lubridate_1.7.10    quantmod_0.4.20     TTR_0.24.2          xts_0.12.1          zoo_1.8-6          
[22] caret_6.0-86        ggplot2_3.3.5       lattice_0.20-38     countrycode_1.2.0   data.table_1.13.0   tidyr_1.1.3         missForest_1.4     
[29] itertools_0.1-3     iterators_1.0.13    foreach_1.5.1       randomForest_4.6-14 broom_1.0.3         geojsonio_0.9.2     PEIP_2.2-3         
[36] ranger_0.12.1       dplyr_1.0.6         passport_0.3.0     

loaded via a namespace (and not attached):
  [1] backports_1.1.10     spam_2.5-1           systemfonts_1.0.2    plyr_1.8.6           lazyeval_0.2.2       splines_3.6.1        jqr_1.2.0           
  [8] RPMG_2.2-3           TH.data_1.0-10       digest_0.6.26        fansi_0.4.1          magrittr_2.0.1       Rwave_2.4-8          recipes_0.1.14      
 [15] modelr_0.1.8         gower_0.2.2          sandwich_3.0-1       lpSolve_5.6.15       prettyunits_1.1.1    textshaping_0.3.4    xfun_0.22           
 [22] crayon_1.4.1         jsonlite_1.7.2       lme4_1.1-25          geigen_2.3           survival_3.2-7       glue_1.4.2           ipred_0.9-9         
 [29] emmeans_1.7.0        sjstats_0.18.1       sjmisc_2.8.9         V8_3.4.0             maps_3.3.0           scales_1.0.0         mvtnorm_1.0-11      
 [36] DBI_1.1.0            ggeffects_1.1.1      Rcpp_1.0.6           xtable_1.8-4         progress_1.2.2       performance_0.9.0    units_0.6-7         
 [43] foreign_0.8-72       bvls_1.4             RSEIS_3.9-3          dotCall64_1.0-0      stats4_3.6.1         lava_1.6.8.1         prodlim_2019.11.13  
 [50] datawizard_0.4.0     ellipsis_0.3.2       pkgconfig_2.0.3      farver_2.0.3         nnet_7.3-14          utf8_1.1.4           crul_1.0.0          
 [57] labeling_0.4.2       tidyselect_1.1.0     rlang_0.4.11         effectsize_0.6.0.1   munsell_0.5.0        tools_3.6.1          generics_0.1.0      
 [64] sjlabelled_1.1.8     gifski_1.4.3-1       ragg_1.1.2           ModelMetrics_1.2.2.2 knitr_1.36           purrr_0.3.4          nlme_3.1-149        
 [71] pracma_2.2.9         compiler_3.6.1       curl_4.3             e1071_1.7-4          statmod_1.4.35       tibble_3.1.1         tweenr_1.0.1        
 [78] stringi_1.4.6        iBreakDown_2.0.1     parameters_0.17.0    fields_11.6          rgeos_0.5-5          Matrix_1.2-18        nloptr_1.2.2.2      
 [85] classInt_0.4-3       vctrs_0.3.8          pillar_1.6.2         lifecycle_1.0.0      estimability_1.3     bitops_1.0-6         maptools_1.0-2      
 [92] insight_0.17.0       R6_2.5.0             KernSmooth_2.23-18   codetools_0.2-16     boot_1.3-25          MASS_7.3-53          assertthat_0.2.1    
 [99] withr_2.5.0          httpcode_0.3.0       multcomp_1.4-14      bayestestR_0.11.5    parallel_3.6.1       hms_1.1.2            geojson_0.3.4       
[106] rpart_4.1-15         timeDate_3043.102    minqa_1.2.4          coda_0.19-4          class_7.3-17         ingredients_2.2.0    sf_0.9-7            
[113] pROC_1.16.2   
```




