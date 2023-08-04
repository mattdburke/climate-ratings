Burke_8.5_2100 <- read.csv("cleandata/Burke_RCP8.5_80_data_clean.csv", header=TRUE)
Burke_2.6_2100 <- read.csv("cleandata/Burke_RCP2.6_80_data_clean.csv", header=TRUE)

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

write.csv(produce_adjusted_ratings(model.forest, Burke_8.5_2100), file="output/Burke_8.5_2100_estimates.csv")
write.csv(produce_adjusted_ratings(model.forest, Burke_2.6_2100), file="output/Burke_2.6_2100_estimates.csv")
