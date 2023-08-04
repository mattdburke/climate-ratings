
figures_path = "figures/"
clean_data_path = "cleandata/"
raw_data_path = "rawdata/"
ratings_path = "output/"

line_thick = 2

createFig2 <- function(){
Baseline <- read.csv(paste(clean_data_path,"Baseline_data_clean.csv", sep=""), header=TRUE)
start_sample <- dplyr::filter(Baseline, Year==2015)
end_sample <- dplyr::filter(Baseline, Year==2020)
start_sample <- dplyr::select(start_sample, CountryName, scale20, Year)
end_sample <- dplyr::select(end_sample, CountryName, scale20, Year)
hist_sample <- bind_rows(start_sample, end_sample)
hist_sample$Year <- ifelse(hist_sample$Year > 2017, "2020", "2015")
ggplot(hist_sample, aes(x=scale20, color=Year, fill=Year))+
	geom_histogram(aes(y=..density..), position="identity", alpha=0.2)+
	geom_density(alpha=0.6)+
	scale_color_manual(values=c("#999999", "#E69F00"))+
	scale_fill_manual(values=c("#999999", "#E69F00"))+
	labs(title="",x="Rating (20 point scale)", y = "Density")+
	theme_classic()
ggsave("fig2.jpeg",dpi=300, height = 6.27, width = 6.27, path=figures_path)
}

createFig4 <- function(){
Baseline <- read.csv(paste(clean_data_path,"Baseline_data_clean.csv", sep=""), header=TRUE)
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
explain_rf <- DALEX::explain(model=model.forest, data=Baseline, label="")
pdp_1  <- as.data.frame(variable_effect_partial_dependency(explain_rf, variable =  "ln_S_GDPpercapitaUS_Z"))
pdp_2   <- as.data.frame(variable_effect_partial_dependency(explain_rf,  variable =  "S_RealGDPgrowth_Z"))
pdp_3  <- as.data.frame(variable_effect_partial_dependency(explain_rf, variable =  "S_NetGGdebtGDP"))
pdp_4  <- as.data.frame(variable_effect_partial_dependency(explain_rf, variable =  "S_GGbalanceGDP"))
pdp_5  <- as.data.frame(variable_effect_partial_dependency(explain_rf, variable =  "S_NarrownetextdebtCARs"))
pdp_6  <- as.data.frame(variable_effect_partial_dependency(explain_rf, variable =  "S_CurrentaccountbalanceGDP"))
png(paste0(figures_path,"fig4.png"),
	width=(2480),
	height=(3508),
	res=300)
par(mfrow = c(3, 2))
plot(pdp_1$"_x_", pdp_1$"_yhat_", type='l', xlab="ln GDP per capita (US$)", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
plot(pdp_2$"_x_", pdp_2$"_yhat_", type='l', xlab="GDP Growth", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
plot(pdp_3$"_x_", pdp_3$"_yhat_", type='l', xlab="Net General Government Debt / GDP", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
plot(pdp_4$"_x_", pdp_4$"_yhat_", type='l', xlab="General Government Balance / GDP", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
plot(pdp_5$"_x_", pdp_5$"_yhat_", type='l', xlab= "Narrow Net External Debt / CARs", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
plot(pdp_6$"_x_", pdp_6$"_yhat_", type='l', xlab="Current Account Balance / GDP", ylab="Rating", cex.lab=1.6, cex.axis=1.6)
dev.off()
}


createFig5 <- function(){
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
keep.inbag=TRUE)
model_importance <- ranger::importance(model.forest)
model_importance <- as.data.frame(model_importance)
model_importance$variable <- rownames(model_importance)
rownames(model_importance) <- NULL
colnames(model_importance) <- c("full_value", "variable")
model_importance$variable <- recode_factor(model_importance$variable, 
							"ln_S_GDPpercapitaUS_Z" = "ln GDP per capita", 
                            "S_RealGDPgrowth_Z" = "GDP Growth",
							"S_NetGGdebtGDP" = "Net general government debt / GDP",
							"S_GGbalanceGDP" = "General government balance / GDP",
							"S_NarrownetextdebtCARs"  = "Narrow net exeternal debt / CARs" ,
							"S_CurrentaccountbalanceGDP" = "Current account balance / GDP")
df1 <- model_importance %>% 
    rowwise() %>%
    arrange(full_value) %>% 
    mutate(variable=factor(variable, levels=variable))
ggplot(df1) +
  geom_segment( aes(x=variable, xend=variable, y=0, yend=full_value), color="grey", lwd=1.5) +
  geom_point( aes(x=variable, y=full_value), color="#E69F00", size=4 ) +
  coord_flip()+
  theme(panel.grid.major = element_line(colour = "grey50")) +
  theme(panel.grid.major = element_line(colour = "white")) +
  theme(text = element_text(size = 22)) + 
  ylab(expression('% reduction in R'^2))+
  theme(axis.text.y = element_text(face = c('plain','plain','plain','plain','plain','plain','plain','plain','plain','plain','bold', 'bold', 'bold', 'bold', 'bold', 'bold')))+
  xlab("") 
ggsave("fig5.jpeg",dpi=300, height = 6.27, width = 6.27*2, path=figures_path)
}


createFig7 <- function(){
	font_size = 1.37
	source("src/errorbar.R")
	df1 <- read.csv(paste0(clean_data_path, "country_level_accuracy.csv"))
	png(paste0(figures_path, "fig7.png"),
	width=(3508/2),
	height=(3508/2),
	res=300)
	plot(df1$est ~ df1$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
    cex.lab=font_size)
	axis(1, seq(5, 20, 5), cex.axis=font_size)
	axis(2, seq(5, 20, 5), cex.axis=font_size)
	errorbar(df1$actual, 0, 0, 
	df1$est, df1$est_l, 
	df1$est_u, bnd=F)
	abline(0,1, lwd=line_thick)
	dev.off()
}

createFig9 <- function(){
SP_data <- read.csv(paste0(raw_data_path, "T3.csv"), header=TRUE)
png(paste0(figures_path, "fig9.png"),
	width=2480,height=3508,res=300)
par(mfrow=c(2,2))
x <- SP_data$GDP_per_capita/100
y <- log(SP_data$NGGD)
plot(x, y, xlab="GDP per capita (% loss)",
	ylab = "",
	main="ln Net General Government Debt")
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ poly(x, 2, raw=TRUE))
fit3 <- lm(y ~ poly(x, 3, raw=TRUE))
mypredict1 <- predict(fit1)
mypredict2 <- predict(fit2)
mypredict3 <- predict(fit3)
ix <- sort(x, index.return=T)$ix
lines(x[ix], mypredict1[ix], col="red")
lines(x[ix], mypredict2[ix], col="blue")
lines(x[ix], mypredict3[ix], lty=2)

GGB <- SP_data$GGB
sub_data <- data.frame(x, GGB)
sub_data <- dplyr::filter(sub_data, GGB<0)
GGB <- sub_data$GGB
x <- sub_data$x
y <- log((GGB)*-1)
plot(x, y, xlab="GDP per capita (% loss)",
	ylab = "",
	main="ln General Government Balance")
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ poly(x, 2, raw=TRUE))
fit3 <- lm(y ~ poly(x, 3, raw=TRUE))
mypredict1 <- predict(fit1)
mypredict2 <- predict(fit2)
mypredict3 <- predict(fit3)
ix <- sort(x, index.return=T)$ix
lines(x[ix], mypredict1[ix], col="red")
lines(x[ix], mypredict2[ix], col="blue")
lines(x[ix], mypredict3[ix], lty=2)
NNED <- SP_data$NNED
x <- SP_data$GDP_per_capita/100
sub_data <- data.frame(x, NNED)
sub_data <- dplyr::filter(sub_data, NNED>0)
NNED <- sub_data$NNED
x <- sub_data$x
y <- log(NNED)
plot(x, y, xlab="GDP per capita (% loss)",
	ylab = "",
	main="ln Narrow Net External Debt")
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ poly(x, 2, raw=TRUE))
fit3 <- lm(y ~ poly(x, 3, raw=TRUE))
mypredict1 <- predict(fit1)
mypredict2 <- predict(fit2)
mypredict3 <- predict(fit3)
ix <- sort(x, index.return=T)$ix
lines(x[ix], mypredict1[ix], col="red")
lines(x[ix], mypredict2[ix], col="blue")
lines(x[ix], mypredict3[ix], lty=2)
x <- SP_data$GDP_per_capita/100
y <- log(SP_data$CAB*-1)
plot(x, y, xlab="GDP per capita (% loss)",
	ylab = "",
	main="ln Current Account Balance")
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ poly(x, 2, raw=TRUE))
fit3 <- lm(y ~ poly(x, 3, raw=TRUE))
mypredict1 <- predict(fit1)
mypredict2 <- predict(fit2)
mypredict3 <- predict(fit3)
ix <- sort(x, index.return=T)$ix
lines(x[ix], mypredict1[ix], col="red")
lines(x[ix], mypredict2[ix], col="blue")
lines(x[ix], mypredict3[ix], lty=2)
dev.off()
}




createFig10 <- function(){
font_size = 1.37
source("src/errorbar.R")
ten85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2030_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2030_estimates.csv"), header=TRUE)
ten85$est_l <- ten85$est - ten85$est_lower
ten85$est_u <- ten85$est_upper - ten85$est
ten26$est_l <- ten26$est - ten26$est_lower
ten26$est_u <- ten26$est_upper - ten26$est
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "fig10.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(ten85$est ~ ten85$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2030 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
errorbar(ten85$actual, 0, 0, 
ten85$est, ten85$est_l, 
ten85$est_u, bnd=F)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick, lty=2)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
plot(ten26$est ~ ten26$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2030 (RCP 2.6)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
errorbar(ten26$actual, 0, 0, 
ten26$est, ten26$est_l, 
ten26$est_u, bnd=F)
abline(lm(ten26$est ~ ten26$actual),col="#3f92a8", lwd=line_thick, lty=2)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}


createFig11 <- function(){
font_size = 1.37
ten85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2030_estimates.csv"), header=TRUE)
thirty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2050_estimates.csv"), header=TRUE)
fifty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2070_estimates.csv"), header=TRUE)
eighty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2030_estimates.csv"), header=TRUE)
thirty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2050_estimates.csv"), header=TRUE)
fifty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2070_estimates.csv"), header=TRUE)
eighty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2100_estimates.csv"), header=TRUE)
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "fig11.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2030-2100 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty85$est ~ thirty85$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty85$est ~ fifty85$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty85$est ~ eighty85$actual),col="#e43307", lwd=line_thick)
legend(14, 10, legend=c("Present", "2030", "2050", "2070", "2100"),
	col=c("#010f10", "#3f92a8", "#b6ba4c", "#d8a109", "#e43307"),
	lty=1, bty="n")
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2030-2100 (RCP 2.6)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten26$est ~ ten26$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty26$est ~ thirty26$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty26$est ~ fifty26$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty26$est ~ eighty26$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}


createFig14 <- function(){
font_size = 1.37
ten85 <- read.csv(paste0(ratings_path, "Kahn_9_2030_estimates.csv"), header=TRUE)
thirty85 <- read.csv(paste0(ratings_path, "Kahn_9_2050_estimates.csv"), header=TRUE)
fifty85 <- read.csv(paste0(ratings_path, "Kahn_9_2070_estimates.csv"), header=TRUE)
eighty85 <- read.csv(paste0(ratings_path, "Kahn_9_2100_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Kahn_1_2030_estimates.csv"), header=TRUE)
thirty26 <- read.csv(paste0(ratings_path, "Kahn_1_2050_estimates.csv"), header=TRUE)
fifty26 <- read.csv(paste0(ratings_path, "Kahn_1_2070_estimates.csv"), header=TRUE)
eighty26 <- read.csv(paste0(ratings_path, "Kahn_1_2100_estimates.csv"), header=TRUE)
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "fig14.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2030-2100 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty85$est ~ thirty85$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty85$est ~ fifty85$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty85$est ~ eighty85$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
legend(14, 10, legend=c("Present", "2030", "2050", "2070", "2100"),
	col=c("#010f10", "#3f92a8", "#b6ba4c", "#d8a109", "#e43307"),
	lty=1, bty="n")
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2030-2100 (RCP 2.6)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten26$est ~ ten26$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty26$est ~ thirty26$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty26$est ~ fifty26$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty26$est ~ eighty26$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}


createFigC1 <- function(){
font_size = 1.37
source("src/errorbar.R")
ten85 <- read.csv(paste0(ratings_path, "Burke_8.5_2100_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Burke_2.6_2100_estimates.csv"), header=TRUE)
ten85$est_l <- ten85$est - ten85$est_lower
ten85$est_u <- ten85$est_upper - ten85$est
ten26$est_l <- ten26$est - ten26$est_lower
ten26$est_u <- ten26$est_upper - ten26$est
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "figC1.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(ten85$est ~ ten85$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2100 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
errorbar(ten85$actual, 0, 0, 
ten85$est, ten85$est_l, 
ten85$est_u, bnd=F)
abline(lm(ten85$est ~ ten85$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
plot(ten26$est ~ ten26$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2100 (No warming)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
errorbar(ten26$actual, 0, 0, 
ten26$est, ten26$est_l, 
ten26$est_u, bnd=F)
abline(lm(ten26$est ~ ten26$actual), col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}


createFigC2 <- function(){
font_size = 1.37
ten85 <- read.csv(paste0(ratings_path, "Kalkuhl_8.5_2030_estimates.csv"), header=TRUE)
thirty85 <- read.csv(paste0(ratings_path, "Kalkuhl_8.5_2050_estimates.csv"), header=TRUE)
fifty85 <- read.csv(paste0(ratings_path, "Kalkuhl_8.5_2070_estimates.csv"), header=TRUE)
eighty85 <- read.csv(paste0(ratings_path, "Kalkuhl_8.5_2100_estimates.csv"), header=TRUE)
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "figC2.png"),
	width=(3508/2),
	height=(3508/2),
	res=300)
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty85$est ~ thirty85$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty85$est ~ fifty85$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty85$est ~ eighty85$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
legend(14, 10, legend=c("Present", "2030", "2050", "2070", "2100"),
	col=c("#010f10", "#3f92a8", "#b6ba4c", "#d8a109", "#e43307"),
	lty=1, bty="n")
dev.off()
}


createFig15 <- function(){
font_size = 1.37
ten85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2030_ma20_estimates.csv"), header=TRUE)
thirty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2050_ma20_estimates.csv"), header=TRUE)
fifty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2070_ma20_estimates.csv"), header=TRUE)
eighty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_ma20_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2030_ma20_estimates.csv"), header=TRUE)
thirty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2050_ma20_estimates.csv"), header=TRUE)
fifty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2070_ma20_estimates.csv"), header=TRUE)
eighty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2100_ma20_estimates.csv"), header=TRUE)
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "fig15.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2030-2100 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty85$est ~ thirty85$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty85$est ~ fifty85$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty85$est ~ eighty85$actual),col="#e43307", lwd=line_thick)
legend(14, 10, legend=c("Present", "2030", "2050", "2070", "2100"),
	col=c("#010f10", "#3f92a8", "#b6ba4c", "#d8a109", "#e43307"),
	lty=1, bty="n")
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2030-2100 (RCP 2.6)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten26$est ~ ten26$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty26$est ~ thirty26$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty26$est ~ fifty26$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty26$est ~ eighty26$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}



createFig16 <- function(){
font_size = 1.37
ten85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2030_ma40_estimates.csv"), header=TRUE)
thirty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2050_ma40_estimates.csv"), header=TRUE)
fifty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2070_ma40_estimates.csv"), header=TRUE)
eighty85 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_ma40_estimates.csv"), header=TRUE)
ten26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2030_ma40_estimates.csv"), header=TRUE)
thirty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2050_ma40_estimates.csv"), header=TRUE)
fifty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2070_ma40_estimates.csv"), header=TRUE)
eighty26 <- read.csv(paste0(ratings_path, "Kahn_2.6_2100_ma40_estimates.csv"), header=TRUE)
present <- read.csv(paste0(ratings_path, "Insample_estimates.csv"), header=TRUE)
png(paste0(figures_path, "fig16.png"),
	width=(2480*1.6),
	height=(3508/2),
	res=300)
par(mfrow=c(1,2)) 
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel A: Climate-adjusted Ratings: 2030-2100 (RCP 8.5)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten85$est ~ ten85$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty85$est ~ thirty85$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty85$est ~ fifty85$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty85$est ~ eighty85$actual),col="#e43307", lwd=line_thick)
legend(14, 10, legend=c("Present", "2030", "2050", "2070", "2100"),
	col=c("#010f10", "#3f92a8", "#b6ba4c", "#d8a109", "#e43307"),
	lty=1, bty="n")
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
plot(present$est ~ present$actual, 
	xlab="Actual Rating", 
	ylab="Estimated Rating", 
	yaxt="none",
	xaxt="none",
	main="Panel B: Climate-adjusted Ratings: 2030-2100 (RCP 2.6)",
    cex.lab=font_size)
abline(0,1, lwd=line_thick)
abline(lm(ten26$est ~ ten26$actual),col="#3f92a8", lwd=line_thick)
abline(lm(thirty26$est ~ thirty26$actual),col="#b6ba4c", lwd=line_thick)
abline(lm(fifty26$est ~ fifty26$actual),col="#d8a109", lwd=line_thick)
abline(lm(eighty26$est ~ eighty26$actual),col="#e43307", lwd=line_thick)
axis(1, seq(5, 20, 5), cex.axis=font_size)
axis(2, seq(5, 20, 5), cex.axis=font_size)
dev.off()
}


# Print Figure 2
# This returns a suggestion on binsizes, but the figure is still produced
createFig2()

# Print Figure 4
# Marginal effects of credit rating determinants
createFig4()

# Print Figure 5
# Corrected error associated with package conflicts
createFig5()

# Print Figure 7
createFig7()

# Print Figure 9
createFig9()

# Print Figure 10
createFig10()

# Print Figure 11
createFig11()

# Print Figure 14
createFig14()

# Print Figure 15
createFig15()

# Print Figure 16
createFig16()

# Print Figure C1
createFigC1()

# Print Figure C2
createFigC2()




