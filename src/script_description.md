# Description of code

## Main scripts

Description for each script in the folder explaining function in order of running.

1. **load_dependencies.r**: Downloads and loads necessary packages
2. **pre-cleaning.r**: Converts moving average 20 and 40 data into similar format as other GDP series' so it can be processed in the same way
3. **kalkuhl_preperation.r**: As above, but for the Kalkuhl and Wenz data. This data is at a regional level so we aggregate to make it comparable
4. **datacleaning.r**: Cleaning benchmark data, most of this script is annotated for summary. Results pushed to the cleandata folder
5. **datacleaning_ma_20.r**: As above, for Kahn moving average 20
6. **datacleaning_ma_40.r**: As above, for Kahn moving average 40 
7. **datacleaning_burke.r**: As above, for Burke et al
8. **datacleaning_kalkuhl.r**: As above, for Kalkuhl and Wenz
9.  **analysis.r**: Produces climate-adjusted ratings for Kahn et al and pushes them to the output folder
10.  **analysis_burke.r**: As above for Burke et al
11.  **analysis_kalkuhl.r**: As above for Kalkuhl and Wenz
12.  **model_accuracy.r**: Produces the model accuracy datasets and pushes them to the cleandata folder, reused later for `tables.r`
13. **tables.r**: Produces all of the results tables in the paper and pushes either `.txt.` or `.csv` files to the tables folder
14. **figures.r**: Produces all of the figures and pushes them to the figures folder
15. **robinson_maps_v2.r**: Produces the three choropleth maps and pushes them to the figures folder

## Additional scripts

* **errorbar.R**: Supplementary file required for building Figure 7 (`createFig7()`), Figure 10 (`createFig10()`) and Figure C1 (`createFigC1()`)
* **key_figures_in_text.r**: A couple of the results discussed directly in the text are reproduced with this script. Details in the annotations.
* **produce Figure 6.r**: Figure 6 has some dependency conflicts in the `main.r` running order. This script is to be run independently in a new console without running `load_dependencies.r`.
