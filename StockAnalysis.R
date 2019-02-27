rm(list=ls())

library(tidyverse)
library(corrplot)
library(magrittr) # this fails on occasion when loading tidyverse
library(RcppRoll)
library(keras) # I am having issues loading this, python lib issue, maybe use lib rnn?
library(mvtnorm)
library(reshape2)

stock_data <- read_csv("Documents/Source Control/Fun Code/StockData.csv")
#stock_data <- read_csv("C:/Code/StockData.csv")

bad_data <- !complete.cases(stock_data)
bad_lines_row <- which(bad_data)
bad_ids <- colnames(stock_data)[is.na(stock_data[bad_lines_row, ])]
stock_data[bad_data, bad_ids] <-
  (stock_data[bad_lines_row + 1, bad_ids] +
     stock_data[bad_lines_row - 1, bad_ids]) / 2
# In retrospect, I dont know if this would have worked with multiple lines of missing values
# If that would have been the case, I think making a function then applying it would have worked
# I know that purrr has a function that safely applies.
# Multiple NA rows in a row would also pose a problem

all(complete.cases(stock_data))
# Good to go.

full_data <- stock_data %>%
  dplyr::mutate(
    MSFT_Returns = log(MSFT/lag(MSFT)),
    AAPL_Returns = log(AAPL/lag(AAPL)),
    V_Returns = log(V /lag(V)),
    INTC_Returns = log(INTC/lag(INTC)),
    CSCO_Returns = log(CSCO/lag(CSCO)),
    
    MSFT_Rolling_Mean = RcppRoll::roll_mean(MSFT_Returns, n = 252, fill = "right"),
    AAPL_Rolling_Mean = RcppRoll::roll_mean(AAPL_Returns, n = 252, fill = "right"),
    V_Rolling_Mean = RcppRoll::roll_mean(V_Returns, n = 252, fill = "right"),
    INTC_Rolling_Mean = RcppRoll::roll_mean(INTC_Returns, n = 252, fill = "right"),
    CSCO_Rolling_Mean = RcppRoll::roll_mean(CSCO_Returns, n = 252, fill = "right"),
    
    MSFT_Rolling_Sd = RcppRoll::roll_sd(MSFT_Returns, n = 252, fill = "right")/sqrt(1/252),
    AAPL_Rolling_Sd = RcppRoll::roll_sd(AAPL_Returns, n = 252, fill = "right")/sqrt(1/252),
    V_Rolling_Sd = RcppRoll::roll_sd(V_Returns, n = 252, fill = "right")/sqrt(1/252),
    INTC_Rolling_Sd = RcppRoll::roll_sd(INTC_Returns, n = 252, fill = "right")/sqrt(1/252),
    CSCO_Rolling_Sd = RcppRoll::roll_sd(CSCO_Returns, n = 252, fill = "right")/sqrt(1/252),
    
    MSFT_Rolling_Var = RcppRoll::roll_var(MSFT_Returns, n = 252, fill = "right")/sqrt(1/252),
    AAPL_Rolling_Var = RcppRoll::roll_var(AAPL_Returns, n = 252, fill = "right")/sqrt(1/252),
    V_Rolling_Var = RcppRoll::roll_var(V_Returns, n = 252, fill = "right")/sqrt(1/252),
    INTC_Rolling_Var = RcppRoll::roll_var(INTC_Returns, n = 252, fill = "right")/sqrt(1/252),
    CSCO_Rolling_Var = RcppRoll::roll_var(CSCO_Returns, n = 252, fill = "right")/sqrt(1/252))

### Lets quickly look at some global statistics ###
full_data %>%
  dplyr::filter(!is.na(MSFT_Returns)) %>%
  dplyr::select(dplyr::ends_with("Returns")) %>% apply(., 2, mean)

full_data %>%
  dplyr::filter(!is.na(MSFT_Returns)) %>%
  dplyr::select(dplyr::ends_with("Returns")) %>% apply(., 2, sd)

full_data %>%
  dplyr::filter(!is.na(MSFT_Returns)) %>%
  dplyr::select(dplyr::ends_with("Returns")) %>% apply(., 2, var)

returns_data <- full_data %>%
  dplyr::select(dplyr::ends_with("Returns"), -Date) %>%
  dplyr::filter(!is.na(MSFT_Returns))

returns_data_flat <- dplyr::tbl_df(returns_data) %>% gather()

ggplot(returns_data_flat, aes(x=value, group=key, color = key)) +  geom_line(stat="density")

correlation_matrix <- cor(returns_data)

corrplot(correlation_matrix, method = "number")

# We are generally, 0.5ish correlation, not necessarily strongly 
#correlated, but correlated never the less

### End quick look ###

trimmed_data <- full_data %>% dplyr::filter(!is.na(MSFT_Rolling_Var))

# Our data looks back one year (252 days), so lets look forward 1 year and see how prices look
# What we are going to do, take the last close price at 252 and run a monte carlo simulation
# We will use the correlation matrix above to attempt a more accurate roll
# This correlation matrix will be used the multinormal so that the drifts are correlated

initial_info <- trimmed_data[1,] # Remember we dropped the top row

covariance_matrix <- cov(returns_data[1:252,])
initial_price <- initial_info %>% dplyr::select(MSFT, AAPL, V, INTC, CSCO)
sigma <- initial_info %>% dplyr::select(dplyr::ends_with("Rolling_Sd"))
mu <- initial_info %>% dplyr::select(dplyr::ends_with("Rolling_Mean"))

time_steps <- 252
delta_t <- 1/time_steps
simulations <- 1000

MSFT_mc <- tibble::tibble(S_o = rep(initial_price$MSFT, simulations))
AAPL_mc <- tibble::tibble(S_o = rep(initial_price$AAPL, simulations))
V_mc <- tibble::tibble(S_o = rep(initial_price$V, simulations))
INTC_mc <- tibble::tibble(S_o = rep(initial_price$INTC, simulations))
CSCO_mc <- tibble::tibble(S_o = rep(initial_price$CSCO, simulations))

### Just to avoid redundant expressions ###
MSFT_mu <- dplyr::pull(mu["MSFT_Rolling_Mean"])
AAPL_mu <- dplyr::pull(mu["AAPL_Rolling_Mean"])
V_mu <- dplyr::pull(mu["V_Rolling_Mean"])
INTC_mu <- dplyr::pull(mu["INTC_Rolling_Mean"])
CSCO_mu <- dplyr::pull(mu["CSCO_Rolling_Mean"])

MSFT_sig <- dplyr::pull(sigma["MSFT_Rolling_Sd"])
AAPL_sig <- dplyr::pull(sigma["AAPL_Rolling_Sd"])
V_sig <- dplyr::pull(sigma["V_Rolling_Sd"])
INTC_sig <- dplyr::pull(sigma["INTC_Rolling_Sd"])
CSCO_sig <- dplyr::pull(sigma["CSCO_Rolling_Sd"])
###

for(i in 2:252) {
  if(i == 2) { start_time = Sys.time() }
  rv <- rmvnorm(simulations, mean = t(mu), sigma = covariance_matrix)
  MSFT_mc %<>% dplyr::mutate(!!as.character(i) := t(pull(MSFT_mc[,i-1]) * (1 + (MSFT_mu + MSFT_sig*rv[,1]))))
  AAPL_mc %<>% dplyr::mutate(!!as.character(i) := t(pull(AAPL_mc[,i-1]) * (1 + (AAPL_mu + AAPL_sig*rv[,2]))))
  V_mc %<>% dplyr::mutate(!!as.character(i) := t(pull(V_mc[,i-1]) * (1 + (V_mu + V_sig*rv[,3]))))
  INTC_mc %<>% dplyr::mutate(!!as.character(i) := t(pull(INTC_mc[,i-1]) * (1 + (INTC_mu + INTC_sig*rv[,4]))))
  CSCO_mc %<>% dplyr::mutate(!!as.character(i) := t(pull(CSCO_mc[,i-1]) * (1 + (CSCO_mu + CSCO_sig*rv[,5]))))
  if(i == 2){
    print(sprintf("Expected time is %.0f seconds", (Sys.time() - start_time) * 2 * 251))
    # The extra 2 factor in there is 
  }
}
print(sprintf("Actual time is %.0f seconds", (Sys.time() - start_time)))

predicted_MSFT_price <- mean(dplyr::pull(MSFT_mc[,time_steps]))
predicted_AAPL_price <- mean(dplyr::pull(AAPL_mc[,time_steps]))
predicted_V_price <- mean(dplyr::pull(V_mc[,time_steps]))
predicted_INTC_price <- mean(dplyr::pull(INTC_mc[,time_steps]))
predicted_CSCO_price <- mean(dplyr::pull(CSCO_mc[,time_steps]))

variance_MSFT_price <- sd(dplyr::pull(MSFT_mc[,time_steps]))
variance_AAPL_price <- sd(dplyr::pull(AAPL_mc[,time_steps]))
variance_V_price <- sd(dplyr::pull(V_mc[,time_steps]))
variance_INTC_price <- sd(dplyr::pull(INTC_mc[,time_steps]))
variance_CSCO_price <- sd(dplyr::pull(CSCO_mc[,time_steps]))

real_MSFT_price <- trimmed_data$MSFT[(2*time_steps)-1]
real_AAPL_price <- trimmed_data$AAPL[(2*time_steps)-1]
real_V_price <- trimmed_data$V[(2*time_steps)-1]
real_INTC_price <- trimmed_data$INTC[(2*time_steps)-1]
real_CSCO_price <- trimmed_data$CSCO[(2*time_steps)-1]

print(sprintf("MSFT : Predicted value of $%.2f, standard deviation of %.2f, actual price $%.2f", predicted_MSFT_price, variance_MSFT_price, real_MSFT_price))
print(sprintf("AAPL : Predicted value of $%.2f, standard deviation of %.2f, actual price $%.2f", predicted_AAPL_price, variance_AAPL_price, real_AAPL_price))
print(sprintf("V : Predicted value of $%.2f, standard deviation of %.2f, actual price $%.2f", predicted_V_price, variance_V_price, real_V_price))
print(sprintf("INTC : Predicted value of $%.2f, standard deviation of %.2f, actual price $%.2f", predicted_INTC_price, variance_INTC_price, real_INTC_price))
print(sprintf("CSCO : Predicted value of $%.2f, standard deviation of %.2f, actual price $%.2f", predicted_CSCO_price, variance_CSCO_price, real_CSCO_price))

### This is where I am going to try and successfully create a recurrant neural network and try to predict stock prices
# https://blogs.rstudio.com/tensorflow/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/

rnn_data <- trimmed_data %>% select(-Date, -MSFT, -AAPL, -V, -INTC, CSCO)
#  Removing the prices as you cant really scale prices.  I will back into prices with the stream of returns
#  I think I may need to come back to this, I suspect there is some multicollinearity with a lot of these variables
#  Above I used one year (252), but I am going to use more than that to train this, maybe 1260 (5 year) or more!




