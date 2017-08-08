rm(list=ls())

library(magrittr)
library(dplyr)
library(datasets)
library(stringr)

#  This is just playing with the MTCARS dataset for a little bit, nothing too deep here
#  I like cars :)
#  Lets compare nox reading to and from the Charles River

mtcars %>% 
  dplyr::group_by(cyl) %>% 
  dplyr::summarise(mean(hp), mean(mpg))

# # A tibble: 3 × 3
# cyl   `mean(hp)` `mean(mpg)`
# <dbl>        <dbl>       <dbl>
# 1     4  82.63636364 26.66363636
# 2     6 122.28571429 19.74285714
# 3     8 209.21428571 15.10000000

#  As one would expect the lower the number of cyl, the less horsepower and the more mpg

#  Lets create a new metric that shows a relation between horsepower and mpg
data <- mtcars %>% 
  dplyr::mutate("Name" = rownames(mtcars), "HPperMPG" = hp / mpg) %>% 
  dplyr::arrange(desc(HPperMPG))

# mpg cyl disp  hp drat    wt  qsec vs am gear carb                Name    HPperMPG
# 1 15.0   8  301 335 3.54 3.570 14.60  0  1    5    8       Maserati Bora 22.33333333
# 2 10.4   8  460 215 3.00 5.424 17.82  0  0    3    4 Lincoln Continental 20.67307692
# 3 10.4   8  472 205 2.93 5.250 17.98  0  0    3    4  Cadillac Fleetwood 19.71153846
# 4 13.3   8  350 245 3.73 3.840 15.41  0  0    3    4          Camaro Z28 18.42105263
# 5 14.3   8  360 245 3.21 3.570 15.84  0  0    3    4          Duster 360 17.13286713

#  I like Ferrari's, lets see how many are contained in this list
data %>% dplyr::filter(stringr::str_detect(Name, fixed("Ferrari", ignore_case = TRUE)))
#  Damn, only 1 :/
# mpg cyl disp  hp drat   wt qsec vs am gear carb         Name    HPperMPG
# 1 19.7   6  145 175 3.62 2.77 15.5  0  1    5    6 Ferrari Dino 8.883248731


#  Lets take all cars that have HPperMPG > 10
data_fil <- data %>% dplyr::filter(HPperMPG > 10)

#  Lets do some stats on the quarter mile time, because what else really matters?
mu <- mean(data_fil$qsec)  # 16.657
sig <- sd(data_fil$qsec)  # 1.415

# If all cars are normally distributed with the above mu and sig and everyone is randomly given a car, 
#  what is the probability you will get a 10 sec (or better) car?
pnorm(10, sd = sig, mean = mu)  # 0.000001285251658
#  Double damn, so very rare
#  We are 4.7 standard deviations from the mean so I guess thats what I get {(10-mu)/sig}