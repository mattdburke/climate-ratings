Kahn_8.5_2030 <- read.csv("cleandata/Kahn_RCP8.5_10_data_clean.csv", header=TRUE)
Kahn_2.6_2030 <- read.csv("cleandata/Kahn_RCP2.6_10_data_clean.csv", header=TRUE)
Kahn_8.5_2050 <- read.csv("cleandata/Kahn_RCP8.5_30_data_clean.csv", header=TRUE)
Kahn_2.6_2050 <- read.csv("cleandata/Kahn_RCP2.6_30_data_clean.csv", header=TRUE)
Kahn_8.5_2070 <- read.csv("cleandata/Kahn_RCP8.5_50_data_clean.csv", header=TRUE)
Kahn_2.6_2070 <- read.csv("cleandata/Kahn_RCP2.6_50_data_clean.csv", header=TRUE)
Kahn_8.5_2100 <- read.csv("cleandata/Kahn_RCP8.5_80_data_clean.csv", header=TRUE)
Kahn_2.6_2100 <- read.csv("cleandata/Kahn_RCP2.6_80_data_clean.csv", header=TRUE)


Kahn_8.5_2030_ma20 <- read.csv("cleandata/Kahn_RCP8.5_10_ma20_data_clean.csv", header=TRUE)
Kahn_2.6_2030_ma20 <- read.csv("cleandata/Kahn_RCP2.6_10_ma20_data_clean.csv", header=TRUE)
Kahn_8.5_2050_ma20 <- read.csv("cleandata/Kahn_RCP8.5_30_ma20_data_clean.csv", header=TRUE)
Kahn_2.6_2050_ma20 <- read.csv("cleandata/Kahn_RCP2.6_30_ma20_data_clean.csv", header=TRUE)
Kahn_8.5_2070_ma20 <- read.csv("cleandata/Kahn_RCP8.5_50_ma20_data_clean.csv", header=TRUE)
Kahn_2.6_2070_ma20 <- read.csv("cleandata/Kahn_RCP2.6_50_ma20_data_clean.csv", header=TRUE)
Kahn_8.5_2100_ma20 <- read.csv("cleandata/Kahn_RCP8.5_80_ma20_data_clean.csv", header=TRUE)
Kahn_2.6_2100_ma20 <- read.csv("cleandata/Kahn_RCP2.6_80_ma20_data_clean.csv", header=TRUE)

Kahn_8.5_2030_ma40 <- read.csv("cleandata/Kahn_RCP8.5_10_ma40_data_clean.csv", header=TRUE)
Kahn_2.6_2030_ma40 <- read.csv("cleandata/Kahn_RCP2.6_10_ma40_data_clean.csv", header=TRUE)
Kahn_8.5_2050_ma40 <- read.csv("cleandata/Kahn_RCP8.5_30_ma40_data_clean.csv", header=TRUE)
Kahn_2.6_2050_ma40 <- read.csv("cleandata/Kahn_RCP2.6_30_ma40_data_clean.csv", header=TRUE)
Kahn_8.5_2070_ma40 <- read.csv("cleandata/Kahn_RCP8.5_50_ma40_data_clean.csv", header=TRUE)
Kahn_2.6_2070_ma40 <- read.csv("cleandata/Kahn_RCP2.6_50_ma40_data_clean.csv", header=TRUE)
Kahn_8.5_2100_ma40 <- read.csv("cleandata/Kahn_RCP8.5_80_ma40_data_clean.csv", header=TRUE)
Kahn_2.6_2100_ma40 <- read.csv("cleandata/Kahn_RCP2.6_80_ma40_data_clean.csv", header=TRUE)

Kahn_9_2030 <- read.csv("cleandata/Kahn_RCP9_10_data_clean.csv", header=TRUE)
Kahn_9_2050 <- read.csv("cleandata/Kahn_RCP9_30_data_clean.csv", header=TRUE)
Kahn_9_2070 <- read.csv("cleandata/Kahn_RCP9_50_data_clean.csv", header=TRUE)
Kahn_9_2100 <- read.csv("cleandata/Kahn_RCP9_80_data_clean.csv", header=TRUE)
Kahn_1_2030 <- read.csv("cleandata/Kahn_RCP1_10_data_clean.csv", header=TRUE)
Kahn_1_2050 <- read.csv("cleandata/Kahn_RCP1_30_data_clean.csv", header=TRUE)
Kahn_1_2070 <- read.csv("cleandata/Kahn_RCP1_50_data_clean.csv", header=TRUE)
Kahn_1_2100 <- read.csv("cleandata/Kahn_RCP1_80_data_clean.csv", header=TRUE)

Baseline <- read.csv("cleandata/Baseline_data_clean.csv", header=TRUE)

set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS_Z +
	S_RealGDPgrowth_Z +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP
	,
	data=Baseline,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)


produce_adjusted_ratings <- function(model, df){
	pred <- predict(model, df, type="se")
	est <- pred$predictions
	se <- pred$se
	actual <- df$scale20
	T <- pred$predictions / se
	P = exp(-0.717*T -0.416*(T^2))
	n = length(df$CountryName)
	DF = n - 3
	crit = tinv(.05, DF)
	est_lower = est + crit*se
	est_upper = est - crit*se
	country <- df$CountryName
	ISO2 <- df$ISO2
	m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
	m1 <- do.call(rbind, Map(data.frame, country=country,
		ISO2 = ISO2,
		actual=actual,
		est=est,
		est_lower=est_lower,
		est_upper=est_upper
		))
	m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
		summarise(ISO2 = first(ISO2),
				actual = mean(actual),
				est = mean(est), 
				est_lower = mean(est_lower),
				est_upper = mean(est_upper)))
	return (m1.sum)
}



write.csv(produce_adjusted_ratings(model.forest, Baseline), file="output/Insample_estimates.csv")

write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2030), file="output/Kahn_8.5_2030_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2030), file="output/Kahn_2.6_2030_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2050), file="output/Kahn_8.5_2050_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2050), file="output/Kahn_2.6_2050_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2070), file="output/Kahn_8.5_2070_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2070), file="output/Kahn_2.6_2070_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2100), file="output/Kahn_8.5_2100_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2100), file="output/Kahn_2.6_2100_estimates.csv")

write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2030_ma20), file="output/Kahn_8.5_2030_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2030_ma20), file="output/Kahn_2.6_2030_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2050_ma20), file="output/Kahn_8.5_2050_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2050_ma20), file="output/Kahn_2.6_2050_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2070_ma20), file="output/Kahn_8.5_2070_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2070_ma20), file="output/Kahn_2.6_2070_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2100_ma20), file="output/Kahn_8.5_2100_ma20_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2100_ma20), file="output/Kahn_2.6_2100_ma20_estimates.csv")

write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2030_ma40), file="output/Kahn_8.5_2030_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2030_ma40), file="output/Kahn_2.6_2030_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2050_ma40), file="output/Kahn_8.5_2050_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2050_ma40), file="output/Kahn_2.6_2050_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2070_ma40), file="output/Kahn_8.5_2070_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2070_ma40), file="output/Kahn_2.6_2070_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_8.5_2100_ma40), file="output/Kahn_8.5_2100_ma40_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_2.6_2100_ma40), file="output/Kahn_2.6_2100_ma40_estimates.csv")

write.csv(produce_adjusted_ratings(model.forest, Kahn_9_2030), file="output/Kahn_9_2030_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_9_2050), file="output/Kahn_9_2050_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_9_2070), file="output/Kahn_9_2070_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_9_2100), file="output/Kahn_9_2100_estimates.csv")

write.csv(produce_adjusted_ratings(model.forest, Kahn_1_2030), file="output/Kahn_1_2030_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_1_2050), file="output/Kahn_1_2050_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_1_2070), file="output/Kahn_1_2070_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Kahn_1_2100), file="output/Kahn_1_2100_estimates.csv")