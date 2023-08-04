# Run this script seperately, conflicts with other packages

# This reproduces the plots in Figure 6

library(DALEX)
library(dplyr)
library(ranger)
library(gridExtra)

figures_path = "figures/"
clean_data_path = "cleandata/"
raw_data_path = "rawdata/"
ratings_path = "output/"


createFig6 <- function(){
Baseline <- read.csv(paste(clean_data_path,"Baseline_data_clean.csv", sep=""), header=TRUE)
set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS_Z +
	S_RealGDPgrowth_Z +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP,
	data=Baseline,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	#probability = TRUE,
	keep.inbag=TRUE)

# China
CN <- as.data.frame(Baseline %>%
	filter(ISO2=="CN") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# Canada
CA <- as.data.frame(Baseline %>%
	filter(ISO2=="CA") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# France
FR <- as.data.frame(Baseline %>%
	filter(ISO2=="FR") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# Germany
DE <- as.data.frame(Baseline %>%
	filter(ISO2=="DE") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# Italy
IT <- as.data.frame(Baseline %>%
	filter(ISO2=="IT") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# Japan
JP <- as.data.frame(Baseline %>%
	filter(ISO2=="JP") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# United Kingdom
GB <- as.data.frame(Baseline %>%
	filter(ISO2=="GB") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))
# United States
US <- as.data.frame(Baseline %>%
	filter(ISO2=="US") %>%
	summarise(ln_S_GDPpercapitaUS_Z = mean(ln_S_GDPpercapitaUS_Z),
		S_RealGDPgrowth_Z = mean(S_RealGDPgrowth_Z),
		S_NetGGdebtGDP = mean(S_NetGGdebtGDP),
		S_GGbalanceGDP = mean(S_GGbalanceGDP),
		S_NarrownetextdebtCARs = mean(S_NarrownetextdebtCARs),
		S_CurrentaccountbalanceGDP = mean(S_CurrentaccountbalanceGDP)))

#########

###
explain_rf <- DALEX::explain(model=model.forest, data=Baseline, label="")
bd_rf <- DALEX::predict_parts(explainer = explain_rf,
                 new_observation = CA,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
CA_plot <- plot(bd_rf, vnames=c("", 
								"ln GDP per capita ($)", 
								"Narrow Net External Debt / CARs",
								"Current Account Balance / GDP",
								"Net General Government Debt / GDP",
								"General Government Balance / GDP", 
								"GDP Growth",
								"Predicted rating"), min_max=c(9,22.5), title="Canada")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = CN,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
CN_plot <- plot(bd_rf, vnames=c("", 
								"", 
								"",
								"",
								"",
								"", 
								"",
								""), min_max=c(9,22.5), title="China")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = FR,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
FR_plot <- plot(bd_rf, vnames=c("", 
								"ln GDP per capita ($)", 
								"Narrow Net External Debt / CARs",
								"Current Account Balance / GDP",
								"Net General Government Debt / GDP",
								"General Government Balance / GDP", 
								"GDP Growth",
								"Predicted rating"), min_max=c(9,22.5), title="France")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = DE,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
DE_plot <- plot(bd_rf, vnames=c("", 
								"", 
								"",
								"",
								"",
								"", 
								"",
								""), min_max=c(9,22.5), title="Germany")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = IT,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
IT_plot <- plot(bd_rf, vnames=c("", 
								"ln GDP per capita ($)", 
								"Narrow Net External Debt / CARs",
								"Current Account Balance / GDP",
								"Net General Government Debt / GDP",
								"General Government Balance / GDP", 
								"GDP Growth",
								"Predicted rating"), min_max=c(9,22.5), title="Italy")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = JP,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
JP_plot <- plot(bd_rf, vnames=c("", 
								"", 
								"",
								"",
								"",
								"", 
								"",
								""), min_max=c(9,22.5), title="Japan")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = GB,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
GB_plot <- plot(bd_rf, vnames=c("", 
								"ln GDP per capita ($)", 
								"Narrow Net External Debt / CARs",
								"Current Account Balance / GDP",
								"Net General Government Debt / GDP",
								"General Government Balance / GDP", 
								"GDP Growth",
								"Predicted rating"), min_max=c(9,22.5), title="United Kingdom")
bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = US,
                            type = "break_down",
							order=c("ln_S_GDPpercapitaUS_Z",
									"S_NarrownetextdebtCARs",
									"S_CurrentaccountbalanceGDP",
									"S_NetGGdebtGDP",
									"S_GGbalanceGDP",
									"S_RealGDPgrowth_Z"),
							keep_distributions = TRUE)
US_plot <- plot(bd_rf, vnames=c("", 
								"", 
								"",
								"",
								"",
								"", 
								"",
								""), min_max=c(9,22.5), title="United States")
png(paste0(figures_path, "fig6.png"),
	width=(2480),height=(3508),res=300)
grid.arrange(CA_plot, CN_plot, FR_plot, DE_plot, IT_plot, JP_plot, GB_plot, US_plot, ncol = 2, widths=c(1.2, .8))
dev.off()
}

createFig6()