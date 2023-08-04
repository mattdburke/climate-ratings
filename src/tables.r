figures_path = "figures/"
tables_path = "tables/"
clean_data_path = "cleandata/"
raw_data_path = "rawdata/"
ratings_path = "output/"

G7countryFilter <- function(df){
	G7China <- c("CA", "CN", "FR", "DE","IT", "JP", "GB", "US")
	df <- dplyr::filter(df, ISO2 %in% G7China)
	return (df)}

printTable1 <- function(){
Baseline <- read.csv("cleandata/Baseline_data_clean.csv", header=TRUE)
n_countries <- length(unique(Baseline$CountryName))
mu_rating <- mean(Baseline$scale20)
### Rating events table
sample_set <- as.data.frame(Baseline %>% 
	group_by(CountryName) %>%
	mutate(Rating_change = c(NA, diff(scale20)),
			Changes = ifelse(Rating_change!=0, 1, 0),
			Positive_Change = ifelse(Rating_change>0, 1, 0),
			Negative_Change = ifelse(Rating_change<0, 1, 0)))
# Positive events
pos_sum <- sum(sample_set$Positive_Change, na.rm=TRUE)
neg_sum <- sum(sample_set$Negative_Change, na.rm=TRUE)
events_total <- pos_sum + neg_sum
freq_rat_delta <- table(sample_set$Rating_change)
n <- length(freq_rat_delta) 
up_1_count <- freq_rat_delta[6]
up_2_count <- freq_rat_delta[7]
up_3_count <- sum(freq_rat_delta[8:n])
down_1_count <- freq_rat_delta[4]
down_2_count <- freq_rat_delta[3]
down_3_count <- sum(freq_rat_delta[2:1])
fileConn<-file("tables/table1.txt")
writeLines(c(
	paste0("Countries ", n_countries),
	paste0("Average numerical rating ", round(mu_rating, 2)),
	paste0("Positive events ", pos_sum, " ",round(pos_sum/events_total*100, 2),"%"),
	paste0("Upgrade by 1 notch ", up_1_count, " ",round(up_1_count/events_total*100, 2),"%"),
	paste0("Upgrade by 2 notch ", up_2_count, " ",round(up_2_count/events_total*100, 2),"%"),
	paste0("Upgrade by >2 notch ", up_3_count, " ",round(up_3_count/events_total*100, 2),"%"),
	paste0("Negative events ", neg_sum, " ",round(neg_sum/events_total*100, 2),"%"),
	paste0("Downgrade by 1 notch ", down_1_count, " ",round(down_1_count/events_total*100, 2),"%"),
	paste0("Downgrade by 2 notch ", down_2_count, " ",round(down_2_count/events_total*100, 2),"%"),
	paste0("Downgrade by >2 notch ", down_3_count, " ",round(down_3_count/events_total*100, 2),"%"),
	paste0("Total no of events ", events_total, " ",round(events_total/events_total*100, 2),"%" )
), fileConn)
close(fileConn)
}



printTable2 <- function(){
Baseline <- read.csv("rawdata/economic.csv", header=TRUE)
cl <- read.table("rawdata/country_list.txt")
cl <- cl$V1
Baseline$ISO2 <- countryname(Baseline$CountryName, destination = "iso2c")
Baseline<- as.data.frame(Baseline %>%
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
	dplyr::filter(Year > 2014))
Baseline <- Baseline[complete.cases(Baseline),]
x1 <- round(mean(Baseline$ln_S_GDPpercapitaUS), 2)
x2 <- round(sd(Baseline$ln_S_GDPpercapitaUS), 2)
x3 <- round(min(Baseline$ln_S_GDPpercapitaUS), 2)
x4 <- round(max(Baseline$ln_S_GDPpercapitaUS), 2)
x5 <- round(mean(Baseline$S_RealGDPgrowth), 2)
x6 <- round(sd(Baseline$S_RealGDPgrowth), 2)
x7 <- round(min(Baseline$S_RealGDPgrowth), 2)
x8 <- round(max(Baseline$S_RealGDPgrowth), 2)
x9 <- round(mean(Baseline$S_NetGGdebtGDP), 2)
x10 <- round(sd(Baseline$S_NetGGdebtGDP), 2)
x11 <- round(min(Baseline$S_NetGGdebtGDP), 2)
x12 <- round(max(Baseline$S_NetGGdebtGDP), 2)
x13 <- round(mean(Baseline$S_NarrownetextdebtCARs), 2)
x14 <- round(sd(Baseline$S_NarrownetextdebtCARs), 2)
x15 <- round(min(Baseline$S_NarrownetextdebtCARs), 2)
x16 <- round(max(Baseline$S_NarrownetextdebtCARs), 2)
x17 <- round(mean(Baseline$S_CurrentaccountbalanceGDP), 2)
x18 <- round(sd(Baseline$S_CurrentaccountbalanceGDP), 2)
x19 <- round(min(Baseline$S_CurrentaccountbalanceGDP), 2)
x20 <- round(max(Baseline$S_CurrentaccountbalanceGDP), 2)
x21 <- round(mean(Baseline$S_GGbalanceGDP), 2)
x22 <- round(sd(Baseline$S_GGbalanceGDP), 2)
x23 <- round(min(Baseline$S_GGbalanceGDP), 2)
x24 <- round(max(Baseline$S_GGbalanceGDP), 2)
tab <- matrix(
    c(x1, x2, x3, x4, 
        x5, x6, x7, x8, 
        x9, x10, x11, x12, 
        x13, x14, x15, x16, 
        x17, x18, x19, x20, 
        x21, x22, x23, x24), 
    ncol=4, 
    nrow=6,
    byrow=TRUE)
colnames(tab) <- c('Mean','St. Dev.','Min', 'Max')
rownames(tab) <- c('Log GDP per capita','Real GDP Growth','NGGD', 'NNED', 'CAB', 'GGB')
tab <- as.table(tab)
write.table(tab,file="tables/table2.csv")
}



printTable3a <- function(){
Baseline <- read.csv("cleandata/Baseline_data_clean.csv", header=TRUE)
set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)
set.seed(77)
model.forest.out <- ranger(scale20 ~
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
pred <- round(predict(model.forest.out, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/table3a.txt")
writeLines(c(
	"Accuracy results for out of sample",
	paste0(acc_table.out),
	paste0("Observations in training sample ", length(train$CountryName)),
	paste0("Observations in test sample ", length(test$CountryName)),
	paste0("Countries in training sample ", length(unique(train$CountryName))),
	paste0("Countries in test sample ", length(unique(test$CountryName)))
), fileConn)
close(fileConn)
}


printTable3b <- function(){
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
pred <- round(predict(model.forest, Baseline, type="se")$predictions)
actual <- Baseline$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/table3b.txt")
writeLines(c(
	"Accuracy results for in sample",
	paste0(acc_table.out),
	paste0("Observations in sample ", length(Baseline$CountryName)),
	paste0("Countries in sample ", length(unique(Baseline$CountryName)))
), fileConn)
close(fileConn)
}






printTable3c <- function(){
Baseline <- read.csv("cleandata/Baseline_data_clean.csv", header=TRUE)
Baseline <- dplyr::filter(Baseline, scale20>10)
set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)
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
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/table3c.txt")
writeLines(c(
	"Accuracy results for out of sample investment grade set",
	paste0(acc_table.out),
	paste0("Observations in training sample ", length(train$CountryName)),
	paste0("Observations in test sample ", length(test$CountryName)),
	paste0("Countries in training sample ", length(unique(train$CountryName))),
	paste0("Countries in test sample ", length(unique(test$CountryName)))
), fileConn)
close(fileConn)
}





printTableC1a <- function(){
Baseline <- read.csv("cleandata/Baseline_longterm_data_clean.csv", header=TRUE)
set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)
set.seed(77)
model.forest.out <- ranger(scale20 ~
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
pred <- round(predict(model.forest.out, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/tableC1a.txt")
writeLines(c(
	"Accuracy results for out of sample",
	paste0(acc_table.out),
	paste0("Observations in training sample ", length(train$CountryName)),
	paste0("Observations in test sample ", length(test$CountryName)),
	paste0("Countries in training sample ", length(unique(train$CountryName))),
	paste0("Countries in test sample ", length(unique(test$CountryName)))
), fileConn)
close(fileConn)
}


printTableC1b <- function(){
Baseline <- read.csv("cleandata/Baseline_longterm_data_clean.csv", header=TRUE)
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
pred <- round(predict(model.forest, Baseline, type="se")$predictions)
actual <- Baseline$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/tableC1b.txt")
writeLines(c(
	"Accuracy results for in sample",
	paste0(acc_table.out),
	paste0("Observations in sample ", length(Baseline$CountryName)),
	paste0("Countries in sample ", length(unique(Baseline$CountryName)))
), fileConn)
close(fileConn)
}






printTableC1c <- function(){
Baseline <- read.csv("cleandata/Baseline_longterm_data_clean.csv", header=TRUE)
Baseline <- dplyr::filter(Baseline, scale20>10)
set.seed(5)
sample = sample.split(Baseline$CountryName, SplitRatio = .8)
train = subset(Baseline, sample == TRUE)
test  = subset(Baseline, sample == FALSE)
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
acc_table.out <- cbind(c(round(a1*100, 2), round(a2*100, 2), round(a3*100, 2), round(a4*100, 2)))
fileConn<-file("tables/tableC1c.txt")
writeLines(c(
	"Accuracy results for out of sample investment grade set",
	paste0(acc_table.out),
	paste0("Observations in training sample ", length(train$CountryName)),
	paste0("Observations in test sample ", length(test$CountryName)),
	paste0("Countries in training sample ", length(unique(train$CountryName))),
	paste0("Countries in test sample ", length(unique(test$CountryName)))
), fileConn)
close(fileConn)
}

# standardised cost tables
costTables <- function(estimates, debt, sample){
	# estimates e.g. Kahn_2.6_2100_estimates
	# type "sovereignDebt" or "corporateDebt"
	# sample should be either "FULL" or "G7"

if (debt=="sovereignDebt"){
	lb_s <- 0.0008
	ub_s <- 0.0012
}

if (debt=="corporateDebt"){
	lb_s <- 0.0008*0.6
	ub_s <- 0.0012*0.7
}

estimates <- read.csv(paste0(ratings_path, estimates,".csv"), header=TRUE)
cost <- read.csv(paste0(raw_data_path, debt, ".csv"))
estimates <- dplyr::filter(estimates, round(actual,2)>round(est,2))
cost$ISO2 <- parse_country(cost$CountryName, to="iso2c")
estimates <- inner_join(estimates, cost, by=c("ISO2"))
n <- length(estimates)
estimates$debt <- as.numeric(as.character(estimates[,n]))
estimates <- dplyr::filter(estimates, debt > 0)
if (debt=="corporateDebt"){
	### Remove all non BIS countries, those with NA for corporate debt
	estimates <- estimates[!is.na(estimates$debt),]
}

sample_mu_downgrade <- round(mean(estimates$actual-estimates$est),2)
sample_sum_debt <- round(sum(estimates$debt, na.rm=TRUE), 2)

sov_downgrades <- round(estimates$actual-estimates$est,2)
sov_debt <- round(as.numeric(as.character(estimates$debt)), 2)

sample_lb <- round(sum(sov_downgrades * lb_s * sov_debt), 2)
sample_ub <- round(sum(sov_downgrades * ub_s * sov_debt), 2)


if (sample=="G7"){
	estimates <- G7countryFilter(estimates)
	country <- as.character(estimates$country)
	sov_downgrades <- round(estimates$actual-estimates$est,2)
	sov_debt <- round(as.numeric(as.character(estimates$debt)), 2)
	lb <- round(sov_downgrades * lb_s * sov_debt, 2)
	ub <- round(sov_downgrades * ub_s * sov_debt, 2)
	G7_mu_downgrade <- round(mean(sov_downgrades),2)
	G7_sum_debt <- sum(sov_debt, na.rm=TRUE)
	G7_lb <- sum(lb)
	G7_ub <- sum(ub)
	country <- c(country, "G7 + China", "Full sample total")
	sov_downgrades <- c(sov_downgrades, G7_mu_downgrade, sample_mu_downgrade)
	sov_debt <- c(sov_debt, G7_sum_debt, sample_sum_debt)
	lb <- c(lb, G7_lb, sample_lb)
	ub <- c(ub, G7_ub, sample_ub)
	df1 <- data.frame(country = country, 
	"sovereign downgrades" = sov_downgrades, 
	"outstanding debt" = sov_debt, 
	"lower bound cost" = lb, 
	"upper bound cost" = ub)
}
if (sample=="FULL"){
	country <- as.character(estimates$country)
	lb <- round(sov_downgrades * lb_s * sov_debt, 2)
	ub <- round(sov_downgrades * ub_s * sov_debt, 2)
	country <- c(country, "Full sample total")
	sov_downgrades <- c(sov_downgrades, sample_mu_downgrade)
	sov_debt <- c(sov_debt, sample_sum_debt)
	lb <- c(lb, sample_lb)
	ub <- c(ub, sample_ub)
	df1 <- data.frame(country = country, 
	"outstanding debt" = sov_debt, 
	"sovereign downgrades" = sov_downgrades, 
	"lower bound cost" = lb, 
	"upper bound cost" = ub)
}
print (df1)}

printTableE <- function(year){
x1 <- read.csv(paste0(ratings_path, "Kahn_2.6_",year,"_estimates.csv"), header=TRUE)
x2 <- read.csv(paste0(ratings_path, "Kahn_1_",year,"_estimates.csv"), header=TRUE)
x3 <- read.csv(paste0(ratings_path, "Kahn_8.5_",year,"_estimates.csv"), header=TRUE)
x4 <- read.csv(paste0(ratings_path, "Kahn_9_",year,"_estimates.csv"), header=TRUE)
country <- x4$country
baseline26 <- round(x1$est, 2)
baseline1 <- round(x2$est, 2)
baseline85 <- round(x3$est, 2)
baseline9 <- round(x4$est, 2)
df1 <- data.frame(country = country, 
	RCP2.6 = baseline26, 
	RCP2.6_volatility = baseline1, 
	RCP8.5 = baseline85, 
	RCP8.5_volatility = baseline9)
return (df1)
}



printTable4 <- function(){
eighty2085 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_ma20_estimates.csv"), header=TRUE)
eighty3085 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_estimates.csv"), header=TRUE)
eighty4085 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_ma40_estimates.csv"), header=TRUE)
eighty309 <- read.csv(paste0(ratings_path, "Kahn_9_2100_estimates.csv"), header=TRUE)
eighty2085 <- G7countryFilter(eighty2085)
eighty3085 <- G7countryFilter(eighty3085)
eighty4085 <- G7countryFilter(eighty4085)
eighty309 <- G7countryFilter(eighty309)
country <- eighty2085$ISO2
ma20 <- round(eighty2085$est,2)
ma30 <- round(eighty3085$est,2)
ma40 <- round(eighty4085$est,2)
ma309 <- round(eighty309$est,2)
df1 <- data.frame(country = country, ma20 = ma20, ma30 = ma30, ma40 = ma40, ma30vol = ma309)
write.csv(df1, "tables/table4.csv")
}


# Prints Table 1
printTable1()

# Prints Table 2
printTable2()

# Prints Table 3
printTable3a()
printTable3b()
printTable3c()

# Prints Table 4
printTable4()

# Prints Table 5
write.csv(costTables("Kahn_2.6_2100_estimates", "sovereignDebt", "G7"), "tables/table5.csv")

# Prints Table 6
write.csv(costTables("Kahn_8.5_2100_estimates", "sovereignDebt", "G7"), "tables/table6.csv")

# Prints Table 7
write.csv(costTables("Kahn_2.6_2100_estimates", "corporateDebt", "G7"), "tables/table7.csv")

# Prints Table 8
write.csv(costTables("Kahn_8.5_2100_estimates", "corporateDebt", "G7"), "tables/table8.csv")


# Prints Table C1
printTableC1a()
printTableC1b()
printTableC1c()

# Prints full cost tables D1, D2, D3 and D4
write.csv(costTables("Kahn_2.6_2100_estimates", "sovereignDebt", "FULL"), "tables/tableD1.csv")
write.csv(costTables("Kahn_8.5_2100_estimates", "sovereignDebt", "FULL"), "tables/tableD2.csv")
write.csv(costTables("Kahn_2.6_2100_estimates", "corporateDebt", "FULL"), "tables/tableD3.csv")
write.csv(costTables("Kahn_8.5_2100_estimates", "corporateDebt", "FULL"), "tables/tableD4.csv")

# printTableE()
write.csv(printTableE("2030"), file=(paste0(tables_path,"tableE1.csv")))
write.csv(printTableE("2050"), file=(paste0(tables_path,"tableE2.csv")))
