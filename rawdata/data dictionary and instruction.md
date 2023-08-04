# Raw data dictionary

Add the raw data files from the online appendices to this folder. 

## Climate data

A number of the files in this directory represent economic loss data from various climate scenarios. I provide a list below of the datasets and their corresponding published papers.

* [Burke, M., Hsiang, S. M., & Miguel, E. (2015). Global non-linear effect of temperature on economic production. Nature, 527(7577), 235-239.](https://www.nature.com/articles/nature15725)
    - `Burke_climatechange.csv` 
    - `Burke_noclimatechange.csv` 


* [Kalkuhl, M., & Wenz, L. (2020). The impact of climate conditions on economic production. Evidence from a global panel of regions. Journal of Environmental Economics and Management, 103, 102360.](https://www.sciencedirect.com/science/article/pii/S0095069620300838)
    - `damage_estimate.csv`
    - `panel_short.csv`

* [Kahn, M. E., Mohaddes, K., Ng, R. N., Pesaran, M. H., Raissi, M., & Yang, J. C. (2021). Long-term macroeconomic effects of climate change: A cross-country analysis. Energy Economics, 104, 105624.](https://www.sciencedirect.com/science/article/pii/S0140988321004898?casa_token=n_5qQd13CXwAAAAA:Bg79FyBmBoTqpSmTqrd6mLEPJbuXcI6KNuWAEJ8WVQhuMZrTM2cAk61kvEJpov_wcLlp60mcTbQ)]
    1. Baseline findings
    - `KAM_RCP26_spain.csv` 
    - `KAM_RCP85_spain.csv`
    2. Increased temperature variability
    - `KAM_RCP26_vol.csv`
    - `KAM_RCP85_vol.csv`
    3. Data for different moving averages
    - `GDPLosses.csv`

## Economic and other data

Corporate and Sovereign debt data from the Bank of International Settlements and Standars & Poor's respectively
- `corporateDebt.csv`
- `sovereignDebt.csv`

Produced within `kalkuhl_preperation.csv`. This is data in a similar format to other loss data above.
- `Kalkuhl.csv`

Data from Table 3 in "Storm Alert: Natural Disasters Can Damage Sovereign Creditworthiness" by Standard & Poor's
- `T3.csv`

Fundemental economic data
- `economic.csv`
Data on the economic fundementals of the sovereigns under study. The variables of interest in this dataset are the following;
* **CountryName**: List of country names
* **Year**: Year of observation
* **scale20**: 1-20 scale corresponding to S&P Sovereign rating scale
* **S_GDPpercapitaUS**: GDP per capita in US$ as defined by S&P SRI
* **S_RealGDPgrowth**: Real GDP growth as defined by S&P SRI
* **S_NetGGdebtGDP**: Net general government debt to GDP as defined by S&P SRI
* **S_GGbalanceGDP**: General government balance to GDP as defined by S&P SRI
* **S_NarrownetextdebtCARs**: Narrow net external debt to CARs as defined by S&P SRI
* **S_CurrentaccountbalanceGDP**: Current account balance to GDP as defined by S&P SRI

Moving average loss data from Kahn et al. Produced within the code `pre-cleaning.r`.
- `ma_20_26.csv`
- `ma_20_85.csv`
- `ma_30_26.csv`
- `ma_30_85.csv`