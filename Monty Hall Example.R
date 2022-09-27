########################
# Anahi Rodriguez
# ECON 474
# Probability Example
########################



# load packages
library(tidyverse)



# number of doors
doors = 1:3

# number of repitiions
n = 1000

# an empty place holder object
results = tibble(prize = rep(NA,n),
                 guess = rep(NA,n),
                 switch = rep(NA,n))

# experiments
set.seed(474)
for(i in 1:n){
  prize = sample(doors, 1)
  guess = sample(doors, 1)
  empty = setdiff(doors, c(prize,guess))[1]
  switch = setdiff(doors, c(guess,empty))[1]
  
  
  results$prize[i] = prize
  results$guess[i] = guess
  results$switch[i] = switch
  
}


results %>%
  summarise(stayers = mean(prize == guess),
            switches = mean(prize == switch))



# expected value example

# load in data
acs.2019 <- read.csv("~/Desktop/data econ 474/acs 2019.csv")
names(acs.2019) = acs.2019 %>% names %>% tolower

acs = acs.2019


# expected wage in champaign the same as chicago conditional on stuff?
acs_trim = acs %>%
  filter(gq %in% c(1.2) &
         educd == 101 &
         age >= 30 & age <= 55 &
         empstat == 1 &
         met2013 %in% c(16580,16980)) %>%
  mutate(incwage = ifelse(incwage < 999999, incwage, NA)) %>%
  na.omit()


acs_trim %>%
  group_by(met2013) %>%
  summarise(wrong_avg_wage = mean(incwage),
            avg_wage = weighted.mean(incwage,perwt))

champaign = acs_trim %>%
  filter(met2013 == 16580)
chicago = acs_trim %>%
  filter(met2013 == 16980)

library(weights)
wtd.t.test(x= champaign$incwage,
           weight = champaign$perwt,
           y = chicago$incwage)





