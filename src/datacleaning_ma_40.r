df1 <- read.csv("rawdata/economic.csv", header=TRUE)
Kamiar_Climate <- read.csv("rawdata/ma_40_85.csv", header=TRUE)
Kamiar_NoClimate <- read.csv("rawdata/ma_40_26.csv", header=TRUE)
SP_data <- read.csv("rawdata/T3.csv", header=TRUE)
cl <- read.table("rawdata/country_list.txt")
cl <- cl$V1

df1 <- as.data.frame(df1 %>% group_by(CountryName) %>%
	# Grouped by country we recreate nominal growth as a
	# comparible variable to the nomial growth we'll be able
	# to observe after adjustments for climate scenarios.
	mutate(S_RealGDPgrowth = Delt(S_GDPpercapitaUS)))

df1 <- as.data.frame(df1 %>% dplyr::select(
	CountryName,
	Year,
	scale20,
	S_GDPpercapitaUS,
	S_RealGDPgrowth,
	S_NetGGdebtGDP,
	S_GGbalanceGDP,
	S_NarrownetextdebtCARs,
	S_CurrentaccountbalanceGDP, 
	))

# Create a standardised ISO2 variable for ease of
df1$ISO2 <- parse_country(df1$CountryName, to="iso2c")


clim_data_management <- function(clim_df1){
	# Sort all years, placeholder x is positioned to produce string
	years <- paste0("x", 2015:2114)
	years <- c(years, "Country")
	# replace iso3 with iso2
	clim_df1$Country <- countrycode(clim_df1$iso, origin = "iso3c", destination = "iso2c")
	# remove original iso and country column
	clim_df1 <- dplyr::select(clim_df1, !(c(country, iso)))
	# replace column names with years rather than n period
	colnames(clim_df1) <- years
	clim_df1 <- clim_df1 %>% gather(Year, GDPpercapita, x2015:x2114)
	Year <- clim_df1$Year
	Year <- as.vector(Year)
	Year <- sub('.', '', Year)
	clim_df1$Year <- Year
	clim_df1_wide <- (tidyr::spread(clim_df1, Year, GDPpercapita))
	colnames(clim_df1_wide)[colnames(clim_df1_wide)
		== "Country"] <- "CountryName"
	return (clim_df1_wide)
}

Kamiar_Climate_wide <- clim_data_management(Kamiar_Climate)
Kamiar_NoClimate_wide <- clim_data_management(Kamiar_NoClimate)

addLossSeries<-function(dataframe, climateframe, years_added, RCP){
	# This function looks at the year we want for the simulation. i.e.
	# 2030. It then, looks at the year for the given country in the 
	# economic dataframe and adds the scenario year onto this number. 
	# For example, Albania in 2009. The function will then look up 
	# 2009 + 10 years = 2019 in the climate data and retrieve the
	# corresponding loss. The same would be true for the final year
	# in the economic data (2020), retrieving the 80 year loss would 
	# correspond to 2100.
	test <- inner_join(dataframe, climateframe, by=c("ISO2"="CountryName"))
	test <- as.data.frame(test)
	test$Year <- as.vector(test$Year)
	test$Year_F <- as.numeric(test$Year) + years_added
	test$Year_F <- as.character(test$Year_F)
	RCP <- as.character(RCP)
	test <- data.table(test)
	# Key look up function. The rest is restructuring the data and then 
	# putting it back together again. This could probably be sped up.
	# Reminder to raise a pull request.
	test <- (test[,test[,Climate_F_GDP := .SD[[.BY[[1]]]], by=Year_F], by=CountryName])
	test$CountryName <- NULL
	test <- unique(test)
	test <- as.data.frame(test %>% dplyr::select(
		CountryName, 
		ISO2,
		Year,
		Climate_F_GDP))
	new_col <- paste("Kahn_", as.character(years_added), "_", RCP, sep="")
	colnames(test) <- c("CountryName", "ISO2", "Year", new_col)
	return(test)
}

Kahn85_10 <- addLossSeries(df1, Kamiar_Climate_wide, 10, 8.5)
Kahn26_10 <- addLossSeries(df1, Kamiar_NoClimate_wide, 10, 2.6)
Kahn85_9 <- addLossSeries(df1, Kamiar_Climate_wide, 9, 8.5)
Kahn26_9 <- addLossSeries(df1, Kamiar_NoClimate_wide, 9, 2.6)

Kahn85_30 <- addLossSeries(df1, Kamiar_Climate_wide, 30, 8.5)
Kahn26_30 <- addLossSeries(df1, Kamiar_NoClimate_wide, 30, 2.6)
Kahn85_29 <- addLossSeries(df1, Kamiar_Climate_wide, 29, 8.5)
Kahn26_29 <- addLossSeries(df1, Kamiar_NoClimate_wide, 29, 2.6)

Kahn85_50 <- addLossSeries(df1, Kamiar_Climate_wide, 50, 8.5)
Kahn26_50 <- addLossSeries(df1, Kamiar_NoClimate_wide, 50, 2.6)
Kahn85_49 <- addLossSeries(df1, Kamiar_Climate_wide, 49, 8.5)
Kahn26_49 <- addLossSeries(df1, Kamiar_NoClimate_wide, 49, 2.6)

Kahn85_80 <- addLossSeries(df1, Kamiar_Climate_wide, 80, 8.5)
Kahn26_80 <- addLossSeries(df1, Kamiar_NoClimate_wide, 80, 2.6)
Kahn85_79 <- addLossSeries(df1, Kamiar_Climate_wide, 79, 8.5)
Kahn26_79 <- addLossSeries(df1, Kamiar_NoClimate_wide, 79, 2.6)

df1 <- inner_join(df1, Kahn85_10, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_10, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn85_9, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_9, by=c("CountryName", "ISO2", "Year"))

df1 <- inner_join(df1, Kahn85_30, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_30, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn85_29, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_29, by=c("CountryName", "ISO2", "Year"))

df1 <- inner_join(df1, Kahn85_50, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_50, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn85_49, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_49, by=c("CountryName", "ISO2", "Year"))

df1 <- inner_join(df1, Kahn85_80, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_80, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn85_79, by=c("CountryName", "ISO2", "Year"))
df1 <- inner_join(df1, Kahn26_79, by=c("CountryName", "ISO2", "Year"))


# This small section of straight code defines the relationships
# between the various government performance variables and GDP losses.
# Using data from S&P to do accomplish it.
gdp_losses <- SP_data$GDP_per_capita/100
NGGD <- log(SP_data$NGGD)
fit_NGGD <- lm(NGGD ~ poly(gdp_losses, 3, raw=TRUE))
GGB <- SP_data$GGB
sub_data <- data.frame(gdp_losses, GGB)
sub_data <- dplyr::filter(sub_data, GGB<0)
GGB <- sub_data$GGB
gdp_losses_GGB <- sub_data$gdp_losses
GGB <- log((GGB)*-1)
fit_GGB <- lm(GGB ~ poly(gdp_losses_GGB, 3, raw=TRUE))
NNED <- SP_data$NNED
sub_data <- data.frame(gdp_losses, NNED)
sub_data <- dplyr::filter(sub_data, NNED>0)
NNED <- sub_data$NNED
gdp_losses_NNED <- sub_data$gdp_losses
NNED <- log(NNED)
fit_NNED <- lm(NNED ~ poly(gdp_losses_NNED, 3, raw=TRUE))
CAB <- log(SP_data$CAB*-1)
fit_CAB <- lm(CAB ~ poly(gdp_losses, 3, raw=TRUE))


GPI_scores <- function(dataframe, year, RCP){
	# This function takes the defined relationships above and applies
	# them to the various scenarios we have. 
	equa <- function(A, x1,x2,x3,x4){
	x1 + A*x2 + (A^2)*x3 + (A^3)*x4
	}
	loss_series <- paste("Kahn_", as.character(year), "_", as.character(RCP), sep="")
	names(loss_series) <- loss_series
	temp_dataframe <- dplyr::select(dataframe, 
		ISO2,
		Year,
		S_NetGGdebtGDP,
		S_GGbalanceGDP,
		S_NarrownetextdebtCARs,
		S_CurrentaccountbalanceGDP
		)
	loss_series_vector <- dataframe[[loss_series]]
	# Don't want to pass 'negative' losses through the model. GDP gains
	# are replaced with 0 for the purposes of GPI generation.

	# They are not replaced completely so that we can accurately calculate
	# GDP gains for the stock and flow variables later.

	loss_series_vector[loss_series_vector>0] <- 0
	Kahn_climate_NGGD <- equa(loss_series_vector,
		fit_NGGD$coefficients[1],
		fit_NGGD$coefficients[2],
		fit_NGGD$coefficients[3],
		fit_NGGD$coefficients[4])
	Kahn_climate_NGGD <- exp(Kahn_climate_NGGD)
	Kahn_climate_GGB <- equa(loss_series_vector,
		fit_GGB$coefficients[1],
		fit_GGB$coefficients[2],
		fit_GGB$coefficients[3],
		fit_GGB$coefficients[4])
	Kahn_climate_GGB <- exp(Kahn_climate_GGB)
	Kahn_climate_GGB <- Kahn_climate_GGB*-1
	Kahn_climate_NNED <- equa(loss_series_vector,
		fit_NNED$coefficients[1],
		fit_NNED$coefficients[2],
		fit_NNED$coefficients[3],
		fit_NNED$coefficients[4])
	Kahn_climate_NNED <- exp(Kahn_climate_NNED)
	Kahn_climate_CAB <- equa(loss_series_vector,
		fit_CAB$coefficients[1],
		fit_CAB$coefficients[2],
		fit_CAB$coefficients[3],
		fit_CAB$coefficients[4])
	Kahn_climate_CAB <- exp(Kahn_climate_CAB)
	Kahn_climate_CAB <- Kahn_climate_CAB*-1
	temp_dataframe$S_NetGGdebtGDP <- temp_dataframe$S_NetGGdebtGDP + Kahn_climate_NGGD
	temp_dataframe$S_GGbalanceGDP <- temp_dataframe$S_GGbalanceGDP + Kahn_climate_GGB
	temp_dataframe$S_NarrownetextdebtCARs <- temp_dataframe$S_NarrownetextdebtCARs + Kahn_climate_NNED
	temp_dataframe$S_CurrentaccountbalanceGDP <- temp_dataframe$S_CurrentaccountbalanceGDP + Kahn_climate_CAB

	NGGD <- paste("NGGD_", as.character(year), "_", as.character(RCP), sep="")
	GGB <- paste("GGB_", as.character(year), "_", as.character(RCP), sep="")
	NNED <- paste("NNED_", as.character(year), "_", as.character(RCP), sep="")
	CAB <- paste("CAB_", as.character(year), "_", as.character(RCP), sep="")
	colnames(temp_dataframe) <- c("ISO2", "Year", NGGD, GGB, NNED, CAB)
	dataframe <- inner_join(dataframe, temp_dataframe, by=c("ISO2", "Year"))
	return(dataframe)}

df1 <- GPI_scores(df1, 10, 8.5)
df1 <- GPI_scores(df1, 10, 2.6)
df1 <- GPI_scores(df1, 30, 8.5)
df1 <- GPI_scores(df1, 30, 2.6)
df1 <- GPI_scores(df1, 50, 8.5)
df1 <- GPI_scores(df1, 50, 2.6)
df1 <- GPI_scores(df1, 80, 8.5)
df1 <- GPI_scores(df1, 80, 2.6)

Baseline<- as.data.frame(df1 %>%
	# This dplyr pipe removes all the climate data generated.
	# We are only left with the original economic fundemental data. 
	# With the addition of log GDP per capita and the relevant yearly
	# scales for ML training.

	# Alternative specifications could investigate scaling the GPI.

	dplyr::filter(ISO2 %in% cl) %>%
	dplyr::select(
		CountryName,
		ISO2,
		Year,
		scale20,
		S_GDPpercapitaUS,
		S_RealGDPgrowth,
		S_NetGGdebtGDP,
		S_GGbalanceGDP,
		S_NarrownetextdebtCARs,
		S_CurrentaccountbalanceGDP) %>%
	dplyr::mutate(
		ln_S_GDPpercapitaUS = log(S_GDPpercapitaUS)) %>%
	dplyr::group_by(Year) %>%
	dplyr::mutate(
			ln_S_GDPpercapitaUS_Z = scale(ln_S_GDPpercapitaUS),
			S_RealGDPgrowth_Z = scale(S_RealGDPgrowth)) %>%
	dplyr::filter(Year > 2014))
Baseline <- Baseline[complete.cases(Baseline),]


Baseline_longterm<- as.data.frame(df1 %>%
	# As above, with the exception that we dont restrict the timeline.
	dplyr::filter(ISO2 %in% cl) %>%
	dplyr::select(
		CountryName,
		ISO2,
		Year,
		scale20,
		S_GDPpercapitaUS,
		S_RealGDPgrowth,
		S_NetGGdebtGDP,
		S_GGbalanceGDP,
		S_NarrownetextdebtCARs,
		S_CurrentaccountbalanceGDP) %>%
	dplyr::mutate(
		ln_S_GDPpercapitaUS = log(S_GDPpercapitaUS)) %>%
	dplyr::group_by(Year) %>%
	dplyr::mutate(
			ln_S_GDPpercapitaUS_Z = scale(ln_S_GDPpercapitaUS),
			S_RealGDPgrowth_Z = scale(S_RealGDPgrowth)))
Baseline_longterm <- Baseline_longterm[complete.cases(Baseline_longterm),]


filterFrames <- function(dataframe, year, scenario){
	# This code extracts the relevant climate data for the 
	# specificed scenario. See inputs, year and scenario.
	
	# Defining strings for selection later on.
	year_delta <- year-2020
	year <- as.character(year)
	year_delta <- as.character(year_delta)
	scenario <- as.character(scenario)
	G <- paste("Kahn_",year_delta,"_",scenario, sep="")
	nggd <- paste("NGGD_",year_delta,"_",scenario, sep="")
	ggb <- paste("GGB_",year_delta,"_",scenario, sep="")
	nned <- paste("NNED_",year_delta,"_",scenario, sep="")
	cab <- paste("CAB_",year_delta,"_",scenario, sep="")
	new_frame <- as.data.frame(dataframe %>%
		dplyr::filter(ISO2 %in% cl) %>%		
		# Restrict sample to produce same as Burke et al (2015)
		group_by(ISO2) %>%
		# Producing growth figures requires grouping.
		mutate(
			S_GDPpercapitaUS_Clim = S_GDPpercapitaUS * (1+!!as.name(G)),
			# Climate GDP is the current figure adjusted for the loss
			# in n years.
			growth = Delt(S_GDPpercapitaUS_Clim)
			# Growth will simply be the nominal as established for the 
			# current baseline economic data.
		) %>%
		dplyr::select(
			CountryName,
			ISO2,
			Year,
			scale20,
			# Replace all values with the same variable names.
			# Acknowledge that these are simply placeholders.
			# The actual variables used are indicated by the scenario
			# and year labelled in each file name. 
			# This means we can reuse the same ML specification in the 
			# analysis and do not have to define a host of models for 
			# each scenario.
			S_GDPpercapitaUS = S_GDPpercapitaUS_Clim,
			S_RealGDPgrowth = growth,
			S_NetGGdebtGDP = !!as.name(nggd),
			S_GGbalanceGDP = !!as.name(ggb),
			S_NarrownetextdebtCARs = !!as.name(nned),
			S_CurrentaccountbalanceGDP = !!as.name(cab)) %>%
		dplyr::mutate(
			ln_S_GDPpercapitaUS = log(S_GDPpercapitaUS)))
	new_frame <- as.data.frame(new_frame %>% 
		# New group_by replaces an existing grouping in v0.2 milestone on 14 Apr 2014
		dplyr::group_by(Year) %>%
		# Group by year and scale so that we produce a standardised score
		# for the given country in each year. The model can be performed
		# without this, but scaling is probably considered better practice.
		dplyr::mutate(
			ln_S_GDPpercapitaUS_Z = scale(ln_S_GDPpercapitaUS),
			S_RealGDPgrowth_Z = scale(S_RealGDPgrowth)))
	new_frame <- dplyr::filter(new_frame, Year > 2014)
	# # Restrict to the same period. 
	new_frame <- new_frame[complete.cases(new_frame),]
	# # Remove any remaining NA figures that the ML model can't handle.
	return (new_frame)}

Kahn_8.5_2030 <- filterFrames(df1, 2030, 8.5)
Kahn_8.5_2050 <- filterFrames(df1, 2050, 8.5)
Kahn_8.5_2070 <- filterFrames(df1, 2070, 8.5)
Kahn_8.5_2100 <- filterFrames(df1, 2100, 8.5)

Kahn_2.6_2030 <- filterFrames(df1, 2030, 2.6)
Kahn_2.6_2050 <- filterFrames(df1, 2050, 2.6)
Kahn_2.6_2070 <- filterFrames(df1, 2070, 2.6)
Kahn_2.6_2100 <- filterFrames(df1, 2100, 2.6)

write.csv(Kahn_8.5_2030, file = "cleandata/Kahn_RCP8.5_10_ma40_data_clean.csv")
write.csv(Kahn_2.6_2030, file = "cleandata/Kahn_RCP2.6_10_ma40_data_clean.csv")
write.csv(Kahn_8.5_2050, file = "cleandata/Kahn_RCP8.5_30_ma40_data_clean.csv")
write.csv(Kahn_2.6_2050, file = "cleandata/Kahn_RCP2.6_30_ma40_data_clean.csv")
write.csv(Kahn_8.5_2070, file = "cleandata/Kahn_RCP8.5_50_ma40_data_clean.csv")
write.csv(Kahn_2.6_2070, file = "cleandata/Kahn_RCP2.6_50_ma40_data_clean.csv")
write.csv(Kahn_8.5_2100, file = "cleandata/Kahn_RCP8.5_80_ma40_data_clean.csv")
write.csv(Kahn_2.6_2100, file = "cleandata/Kahn_RCP2.6_80_ma40_data_clean.csv")
