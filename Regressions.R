####################
# Anahi Rodriguez
# 09/08/22
# 
####################

##### 1. preliminaries
library(data.table)
library(tidyverse)
library(fixest)
library(stargazer)
library(marginaleffects)


# load data 
setwd("~/Desktop/data econ 474")
cps = fread("cps 2021.csv" )

names(cps) = tolower(names(cps))
# names(cps) = names(cps) %>% tolower()
# names(cps) = cps %>% names %>% tolower


summary(cps)
# cps.ipums.org > get data

# cleaning the data
educ_yos = matrix(c(1, NA,
         2, 0,
         10, 3, # the grades 1,2,3,4
         20, 5.5, # grades 5,6
         30, 7.5, # grades 7,8
         40, 9,
         60, 11,
         71, 12,
         73, 12, # specifically those who have diploma
         85, 13,
         91, 14, # associates,
         111, 16, # bachelors,
         123, 18, # masters
         124, 21, # professional degree
         125, 21), # Ph. D
       ncol = 2, byrow = TRUE) %>%
  data.frame() %>%
  rename(educ = X1,
         yos = X2)



cps = cps %>%
  filter(30 <= age  & age <= 55 &
           incwage > 0 & incwage < 99999999 &
           wkswork1 >= 40 &
           uhrsworkly >= 30 & uhrsworkly != 999) %>%
  left_join(educ_yos) %>%
  mutate(exp = age - yos - 6,
         degree = case_when(educ == 125 ~ 'Ph.D',
                            educ == 124 ~ 'Professional',
                            educ == 123 ~ "Master's",
                            educ == 111 ~ "Bachelor's",
                            educ %in% c(91,92) ~ "Associates",
                            educ >= 73 ~ 'High School',
                            educ >= 2 ~ 'None'),
         female = (sex == 2) * 1,
         children = (nchild > 0) * 1,
         black = (race == 200) * 1,
         asian = (race %in% 651:652) * 1,
         hrly_wage = incwage/(50*40),
         renter = (hhtenure %in% c(2,3) * 1)) %>%
  na.omit()


# remove from environment
rm(educ_yos)


hist(cps$hrly_wage)
hist(log(cps$hrly_wage))

#### 2. Basic regressions

#### 2.1 Coefficients
fit = lm(log(hrly_wage) ~ yos, data = cps)
summary(fit)
# such crazy siginificance is due to large sample
# recall that se(\hat{beta_1} = \sqrt( 1 / (n-2) * SSR/SST_x )
# as n gets large, \sqrt(1/(n-2)) becomes very SMALL:
sqrt(1/fit$df.residual)
# which means the t statistic becomes very large
sfit = summary(fit)
sfit$coefficients[2,3]


#so, about those coefficient formulas
#beta_1
b1 = cov(log(cps$hrly_wage), cps$yos/var(cps$yos))
# beta_0 
mean(log(cps$hrly_wage))- mean(cps$yos) * b1



# Mincer regression
fit_m = lm(log(hrly_wage) ~ yos + exp + I(exp^2), data = cps)
fit_m %>% summary()     


# Mincer plus
fit_mp = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
              female * children + black + asian, data = cps)
fit_mp %>% summary()  

unique(cps$degree)
cps$degree = factor(cps$degree, levels = c('None', 'High School', "Associates", "Bachelor's",
                              "Master's" , "Ph.D","Professional"))
class(cps$degree)


# Mincer plus
fit_mp = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
              female * children + black + asian, data = cps)
fit_mp %>% summary()  
model.matrix(fit_mp) %>%
  head()



# Frisch-Waugh_Lovell
fit_mp_x = lm(yos ~ + exp + I(exp^2) + degree +
              female * children + black + asian, data = cps)
x_tilde = fit_mp_x$residuals
fit_mp
cov(log(cps$hrly_wage), x_tilde/var(x_tilde))


#### 2.2 Output Interpretation
summary(fit_mp)
# yos - an increase in one year of schooling is asscoiated with a 2% increase in hourly wages 
#   (conditional on sample selection and other regressors)
# exp
# w = ... + exp*b2 + exp^2 * b3 + ...
# dw/dexp = b2 + 2*exp*b3
b_mp = coef(fit_mp)
exp = seq(min(cps$exp), max(cps$exp, length = 100))
dw = b_mp[3] + 2*exp*b_mp[4]
plot(dw ~ exp)
w_hat = exp*b_mp[3] + 2*exp^2*b_mp[4]
plot(w_hat ~ exp)

exp_bar = mean(cps$ex)
b_mp[3] + 2*exp_bar*b_mp[4]
# having the average level of experience of 21 years is associated with a 21% increase in hourly wages


# showing results in stargazer
stargazer(fit, fit_m, fit_mp,
          type = 'text',
          omit.stat = c('adj.rsq', 'ser', 'f'),
          keep = c('yos', 'exp', 'I(exp^2)'),
          covariate.labels = c('Years of Schooling', 'Experience', 'Experience Squared'),
          add.lines(list(c('Demographic Controls', 'No','No','Yes'),
                    c('Degree Controls', rep('No',2), 'Yes')))


#### 2.3 Weighted least squares
fit_mp_w = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
              female * children + black + asian, data = cps,
              weights = asecwt)
stargazer(fit_mp, fit_mpw, type = 'text')

#### 2.4 Heteroskedasticity
plot(log(hrly_wage) ~ yos, data = cps)
plot(log(cps$hrly_wage) ~ x_tilde)


fot_mp_r = feols(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
                female * children + black + asian, data = cps,
              vcov = 'hetero'))

stargazer(fit_mp, fit_mp_r)
etable(fit_mp, fit_mp_r)

fit_mp2 = feols(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
           female * children + black + asian, data = cps))
etable(fit_mp2, fit_mp_r,
       se.below = TRUE,
       fitstat = c('n', 'r2'),
       doct = c(yos = 'Years of Schooling',
                exp = 'Experience'),
       keep = c("Years of Schooling", 'Experience'),
       extralines = list("Demographic Controls" = c('Yes','Yes')))