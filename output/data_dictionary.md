# Adjusted-ratings dictionary

## Data format

Each of the files in this folder have the same format;

* **country**: Country name
* **ISO2**: ISO-2 country code
* **actual**: The average actual rating over the sample period (1-20)
* **est**: The average estimated rating in the forecast period (t+h) on the same scale (1-20)
* **est_lower**: Lower bound of the estimated rating
* **est_upper**: Upper bound of the estimated rating

The content for each file is as follows;

* `Burke_2.6_2100_estimates.csv`: Burke et al (2015) 'no-climate' scenario estimates for 2100. Labelled as 2.6 for convenience
* `Burke_8.5_2100_estimates.csv`: Burke et al (2015) climate change scenario estimates for 2100.
* `Insample_estimates.csv`: Full insample estimates of the random forest model.
* `Kahn_scenario_year.csv`: Each of the `Kahn` labelled files are for the following scenarios. 1 = RCP 2.6 with increased temperature variability. 2.6 = RCP 2.6. 8.5 = RCP 8.5. 9 = RCP 8.5 with increased temperature variability. They are also given for the years 2030, 2050, 2070 and 2100. Finally, there are corresponding `.csv` files for the moving average estimates 20 and 40, which are included for robustness.
* `Kalkuhl_scenario_year.csv`: We only report for their climate scenario but reconstruct for the same time horizons. There are 4 files for the Kalkuhl & Wenz study.


