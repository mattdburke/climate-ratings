Baseline <- read.csv("cleandata/Baseline_data_clean.csv", header=TRUE)


### Ordinarily accuracy figures would split on the dependent variable.
### However, to ensure we get as broad a coverage as possible as to 
### accuracy of each country, we split on the country name. This ensures
### as many countries as possible are in both the training and testing sample.
### We adjust the seed on the splitting of this sample accordingly, however keep 
### the seed on the model unchanged from the benchmark result.
set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)

n.obs.benchmark.train <- length(train$CountryName)
n.obs.benchmark.test <- length(test$CountryName)
n.countries.benchmark.train <- length(unique(train$CountryName))
n.countries.benchmark.test <- length(unique(test$CountryName))

set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS_Z +
	S_RealGDPgrowth_Z +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP,
	data=train,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)

pred <- round(predict(model.forest, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table <- cbind(c(a1, a2, a3, a4))
acc_table

pred <- predict(model.forest, test, type="se")
est <- pred$predictions
se <- pred$se
actual <- test$scale20
T <- pred$predictions / se
P = exp(-0.717*T -0.416*(T^2))
n = length(test$CountryName)
DF = n - 3
crit = tinv(.05, DF)
est_lower = est + crit*se
est_upper = est - crit*se
country <- test$CountryName
ISO2 <- test$ISO2
m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
m1 <- do.call(rbind, Map(data.frame, country=country,
	ISO2=ISO2,
	actual=actual,
	est=est,
	est_lower=est_lower,
	est_upper=est_upper
	))
m1$est_l <- m1$est - m1$est_lower
m1$est_u <- m1$est_upper - m1$est
m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
	summarise(ISO2 = ISO2,
			actual = mean(actual),
			est = mean(est),
			est_l = mean(est_l),
			est_u = mean(est_u),
			est_lower=mean(est_lower),
			est_upper=mean(est_upper)))
m1.sum$notch <- round(abs(m1.sum$est - m1.sum$actual))
write.csv(m1.sum, "cleandata/country_level_accuracy.csv")


Baseline <- read.csv("cleandata/Baseline_longterm_data_clean.csv", header=TRUE)

set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)

n.obs.benchmark.train <- length(train$CountryName)
n.obs.benchmark.test <- length(test$CountryName)
n.countries.benchmark.train <- length(unique(train$CountryName))
n.countries.benchmark.test <- length(unique(test$CountryName))

set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS_Z +
	S_RealGDPgrowth_Z +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP
	,
	data=train,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)

pred <- round(predict(model.forest, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table <- cbind(c(a1, a2, a3, a4))
acc_table

pred <- predict(model.forest, test, type="se")
est <- pred$predictions
se <- pred$se
actual <- test$scale20
T <- pred$predictions / se
P = exp(-0.717*T -0.416*(T^2))
n = length(test$CountryName)
DF = n - 3
crit = tinv(.05, DF)
est_lower = est + crit*se
est_upper = est - crit*se
country <- test$CountryName
ISO2 <- test$ISO2
m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
m1 <- do.call(rbind, Map(data.frame, country=country,
	ISO2=ISO2,
	actual=actual,
	est=est,
	est_lower=est_lower,
	est_upper=est_upper
	))
m1$est_l <- m1$est - m1$est_lower
m1$est_u <- m1$est_upper - m1$est
m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
	summarise(ISO2 = ISO2,
			actual = mean(actual),
			est = mean(est),
			est_l = mean(est_l),
			est_u = mean(est_u),
			est_lower=mean(est_lower),
			est_upper=mean(est_upper)))
m1.sum$notch <- round(abs(m1.sum$est - m1.sum$actual))
write.csv(m1.sum, "cleandata/country_level_accuracy_longterm.csv")


