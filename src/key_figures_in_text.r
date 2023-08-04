
figures_path = "figures/"
tables_path = "tables/"
clean_data_path = "cleandata/"
raw_data_path = "rawdata/"
ratings_path = "output/"

print ("Page 5 - Our results document three key findings...")

calculate_downgrade_number_and_mean <- function(estimates){
    df <- read.csv(paste0(ratings_path,estimates,".csv"))
    df$downgrade <- round(df$actual-df$est,2)
    df <- dplyr::filter(df, downgrade>0)
    print (paste0("The number of downgrades is ",length(df$downgrade)))
    print (paste0("The mean downgrade is ",round(mean(df$downgrade),2)))}

calculate_downgrade_number_and_mean("Kahn_8.5_2030_estimates")
calculate_downgrade_number_and_mean("Kahn_8.5_2100_estimates")

calculate_statistical_significance_between_estimates <- function(estimate_set1, estimate_set2){
    df1 <- read.csv(paste0(ratings_path,estimate_set1,".csv"))
    df2 <- read.csv(paste0(ratings_path,estimate_set2,".csv"))
    df1$downgrade <- df1$actual-df1$est
    df2$downgrade <- df2$actual-df2$est
    p.value <- round(as.numeric(as.character(t.test(df1$downgrade, df2$downgrade)[3])), 4)
    print (paste0("The statistical significance of a t.test is ",p.value))}

print ("Page 5 - When find that the average rating reduction between...")
calculate_statistical_significance_between_estimates("Kahn_2.6_2030_estimates", "Kahn_2.6_2100_estimates")




calculate_statistical_significance_between_now_and_estimates <- function(estimate_set){
    df1 <- read.csv(paste0(ratings_path,estimate_set,".csv"))
    p.value <- round(as.numeric(as.character(t.test(df1$actual, df1$est)[3])), 4)
    print (paste0("The statistical significance of a t.test is ",p.value))}

print ("Are RCP 2.6 2100 ratings different from zero?")
calculate_statistical_significance_between_now_and_estimates("Kahn_2.6_2100_estimates")





calculate_range_increase_in_debt_costs  <- function(estimates, debt){
	# estimates e.g. Kahn_2.6_2100_estimates
	# type "sovereignDebt" or "corporateDebt"
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
sov_downgrades <- round(estimates$actual-estimates$est,2)
sov_debt <- round(as.numeric(as.character(estimates$debt)), 2)
lb <- round(sov_downgrades * lb_s * sov_debt, 2)
ub <- round(sov_downgrades * ub_s * sov_debt, 2)
print (paste0("The range for debt increases is ", sum(lb), " to ", sum(ub), " billion USD"))}

print ("RCP 2.6 2100 sovereign debt increase in costs")
calculate_range_increase_in_debt_costs("Kahn_2.6_2100_estimates", "sovereignDebt")
print ("RCP 8.5 2100 sovereign debt increase in costs")
calculate_range_increase_in_debt_costs("Kahn_8.5_2100_estimates", "sovereignDebt")
print ("RCP 2.6 2100 corporate debt increase in costs")
calculate_range_increase_in_debt_costs("Kahn_2.6_2100_estimates", "corporateDebt")
print ("RCP 8.5 2100 corporate debt increase in costs")
calculate_range_increase_in_debt_costs("Kahn_8.5_2100_estimates", "corporateDebt")


calculate_statement_of_sample <- function(baseline){
    df <- read.csv(paste0(clean_data_path, baseline,".csv"), header=TRUE)
    n <- length(df$CountryName)
    unique_n <- length(unique(df$CountryName))
    print (paste0("Our sample is ",n," long term sovereign ratings, for ",unique_n," countries"))}
print ("Page 7 - Beginning of Section 2.1")
calculate_statement_of_sample("baseline_data_clean")


print ("Page 26 & 27 - In contrast, under RCP 8.5, 59...")
calculate_worst_affected_countries <- function(estimates){
    df1 <- read.csv(paste0(ratings_path,estimates,".csv"))
    df1$downgrade <- round(df1$actual-df1$est,2)
    df1 <- df1[order(-df1$downgrade),]
    print (estimates)
    print (df1[1:10,c(2,8)])
}

calculate_worst_affected_countries("Kahn_2.6_2100_estimates")

calculate_least_affected_countries <- function(estimates){
    df1 <- read.csv(paste0(ratings_path,estimates,".csv"))
    df1$downgrade <- round(df1$actual-df1$est,2)
    df1 <- dplyr::filter(df1, downgrade>0)
    df1 <- df1[order(df1$downgrade),]
    print (estimates)
    print (df1[1:10,c(2,8)])
}

calculate_least_affected_countries("Kahn_8.5_2100_estimates")


