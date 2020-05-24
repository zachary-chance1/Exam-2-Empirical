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

ln_q <- log(quantity)
ln_p <- log(price)
day_fe <- cbind(mon, tues, wed, thurs)
instrument1 <- wave2
instrument2 <- speed3

#Part b - OLS
ols_reg <- lm(ln_q ~ ln_p + day_fe)
summary(ols_reg)

#2SLS
iv_reg_wave = ivreg(ln_q ~ ln_p + day_fe | day_fe + instrument1)
summary(iv_reg_wave)

iv_reg_wind = ivreg(ln_q ~ ln_p + day_fe | day_fe + instrument2)
summary(iv_reg_wind)
