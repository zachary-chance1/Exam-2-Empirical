################################################################
#name: Exam 2 Graddy Rep.R
#author: zachary chance (baylor university)
#description: completes exam 2 and performs analysis
#             on the Fulmon Fish market data set.
#             Also produces IV tables for said data
#date: may 24, 2020
#################################################################
WORK.DIR = "C:/Users/Owner/Desktop/Exam-2-Empirical"

# set working directory
setwd(WORK.DIR)

library(AER)
library(haven)
library(tidyverse)

read_data <- function(df)
{
  full_path <- paste("https://storage.googleapis.com/causal-inference-mixtape.appspot.com/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

fish <- read_data("fish.dta")

#Define variable 
#(Y1 = Dependent Variable, Y2 = endogenous variable, X1 = exogenous variable, X2 = Instrument)

attach(fish)

fish$ln_q <- log(quantity)
fish$ln_p <- log(price)
fish$day_fe <- cbind(mon, tues, wed, thurs)
fish$instrument1 <- wave2
fish$instrument2 <- speed3

#Part b - OLS
ols_reg <- lm(ln_q ~ ln_p + day_fe)
summary(ols_reg)

#Part c - 2SLS
iv_reg_wave = ivreg(ln_q ~ ln_p + day_fe | day_fe + instrument1)
summary(iv_reg_wave)

iv_reg_wind = ivreg(ln_q ~ ln_p + day_fe | day_fe + instrument2)
summary(iv_reg_wind)


library(sjPlot)
library(sjmisc)
library(sjlabelled)


combined_table = tab_model(ols_reg, iv_reg_wave, iv_reg_wind, dv.labels = c("OLS", "Wave as IV", "Wind as IV"), title = "ln of Quantity", show.r2 = FALSE, show.fstat = TRUE, show.p = FALSE, p.style = "stars", show.intercept = FALSE, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, pred.labels = c("ln of Price", "Monday FE", "Tuesday FE", "Wednesday FE", "Thursday FE"))
combined_table


tab_model(ols_reg, iv_reg_wave, iv_reg_wind, dv.labels = c("OLS", "Wave as IV", "Wind as IV"), file = "Tables/IVCoef.rtf", title = "ln of Quantity", show.r2 = FALSE, show.fstat = TRUE, show.p = FALSE, p.style = "stars", show.intercept = FALSE, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, pred.labels = c("ln of Price", "Monday FE", "Tuesday FE", "Wednesday FE", "Thursday FE"))


#part d - first stages
wave_first_stage = lm(ln_p ~ instrument1)
summary(wave_first_stage)

wind_first_stage = lm(ln_p ~ instrument2)
summary(wind_first_stage)


F_ins_1 = linearHypothesis(wave_first_stage,  c("instrument1 = 0"))
F_ins_2 = linearHypothesis(wind_first_stage,  c("instrument2 = 0"))


library(grid)
library(gridExtra)
library(plotrix)
F_Addendum[1,] = data.frame(cbind(wave_first_stage$coefficients[[2]], wind_first_stage$coefficients[[2]]))
F_Addendum[2,1] = sqrt(diag(vcov(wave_first_stage)))[2]
F_Addendum[2,2] = sqrt(diag(vcov(wind_first_stage)))[2]
F_Addendum[3,1] = F_ins_1$F[[2]]
F_Addendum[3,2] = F_ins_2$F[[2]]
colnames(F_Addendum) = c("Wave as IV", "Wind as IV")
rownames(F_Addendum) = c("Coefficient of IV", "SE of IV", "F Statistic")

#Regrettably, I couldn't find a way to tack the F stats and coefficients for the first stage regressions, so it is in a separate file 

library(rtf)
rtffile <- RTF("Tables/IVCoef2.rtf")
addText.RTF(this = rtffile, "Results of the First Stage Regression", bold = TRUE)
addTable.RTF(this = rtffile, dat = F_Addendum, row.names = TRUE)
done(rtffile)
