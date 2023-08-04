# Title: Kalkuhl.R

# Description: Convert Kalkuhl regional losses to country level

# Author: Matt Burke
# Last modified: 02/09/2021
options(scipen=999)
# set working directory, load data and packages

df1_damage <- read.csv("rawdata/damage_estimate.csv")
df1_panel <- read.csv("rawdata/panel_short.csv")

# Take out variables
df1_panel_1 <- df1_panel %>% select(
	year, 
	gdp_pc_usd, 
	ID, 
	wrld1iso)

# Remove observations with no defined country 
df1_damage <- dplyr::filter(df1_damage, iso!="")
df1_panel_1 <- dplyr::filter(df1_panel_1, wrld1iso!="")

# Produce yearly groupings
df1_panel_1 <- as.data.frame(df1_panel_1 %>% 
	mutate(yearGrouping = ifelse(year > 2000 & year < 2004, 1, 
		ifelse(year > 2004 & year < 2008, 2, 
		ifelse(year > 2011 & year < 2015, 3, 0)))))

# Calculate country wide gdp
df1_panel_1 <- as.data.frame(df1_panel_1 %>%
    group_by(wrld1iso, year) %>%
    mutate(gdpCountry = sum(gdp_pc_usd, na.rm=TRUE)))

# Calculate regional weighting by regional gdp over country gdp
df1_panel_1$gdpRegionWeight <- df1_panel_1$gdp_pc_usd/df1_panel_1$gdpCountry

# Select for each country the most recent grouping. This gives us
# the most coverage.
df1_panel_1 <- as.data.frame(df1_panel_1 %>%
	group_by(wrld1iso) %>%
	filter(yearGrouping == max(yearGrouping)))

# Establish the average regional weight from each year grouping.
df1_panel_1 <- as.data.frame(df1_panel_1 %>%
    group_by(ID) %>%
    mutate(gdpRegionWeight = mean(gdpRegionWeight)))
	
# Leave behind other variables and remove duplicates
df1_panel_1 <- unique(dplyr::select(df1_panel_1, 
    ID, 
    wrld1iso, 
    gdpRegionWeight))

# Select only useful variables for the forward look
df1_damage_1 <- as.data.frame(df1_damage %>% select(year, 
	ID,
	dGDP_panel_exact))
	
df1 <- inner_join(df1_damage_1, df1_panel_1, by=c("ID"))

# calculate the weighted regional gdp percentage change
df1$weighted_gdp <- df1$dGDP_panel_exact*df1$gdpRegionWeight

# for each year and country, sum the total of the weighted regional
# losses to produce the country loss for year t.
df1 <- as.data.frame(df1 %>% group_by(year, wrld1iso) %>%
    mutate(gdpLossCountry = sum(weighted_gdp, na.rm=TRUE)))

# Remove duplicates
df1 <- unique(dplyr::select(df1, year, wrld1iso, gdpLossCountry))

# Establish the cumulative loss
x <- as.data.frame(df1 %>% 
    group_by(wrld1iso) %>%
    mutate(gdpLossCountryCum = cumprod(gdpLossCountry+1)-1) %>%
    ungroup() %>%
    mutate(ISO2 = countrycode(wrld1iso, origin = "iso3c", destination = "iso2c")) %>%
    dplyr::select(year, ISO2, gdpLossCountryCum) %>%
    group_by(ISO2) %>%
    dplyr::filter(sum(gdpLossCountryCum)!=0))



write.csv(x, "rawdata/Kalkuhl.csv")




