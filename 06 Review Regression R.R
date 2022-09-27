# Reading for next lecutre: Ch 3

##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(data.table)
library(tidyverse)
library(fixest)
library(stargazer)
library(marginaleffects)

cps = fread('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/cps 2021.csv')
cps
names(cps) = tolower(names(cps))
names(cps) = names(cps) %>% tolower()
names(cps) = cps %>% names %>% tolower

summary(cps)


# cps.ipums.org > get data

##==========================##
## ------------------------ ##
#### 1) Cleaning the Data ####
## ------------------------ ##
##==========================##

educ_yos = matrix(c(1, NA,
         2, 0,
         10, 3, # Grades 1, 2, 3, or 4
         20, 5.5, # Grades 5 or 6
         30, 7.5, # Grades 7 or 8
         40, 9,
         60, 11,
         71, 12,
         73, 12, # Diploma
         85, 13,
         91, 14, # Associate's
         111, 16, # Bachelor's 
         123, 18, # Master's
         124, 21, # Professional
         125, 21), # Ph.D.
       ncol = 2, byrow = TRUE) %>%
  data.frame() %>%
  rename(educ = X1,
         yos = X2)

cps = cps %>%
  filter(age >= 30 & age <= 55 &
           incwage > 0 & incwage < 99999999 &
           wkswork1 >= 40 &
           uhrsworkly >= 30 & uhrsworkly != 999) %>%
  left_join(educ_yos) %>%
  mutate(exp = age - yos - 6,
         degree = case_when(educ == 125 ~ 'Ph.D.',
                            educ == 124 ~ 'Professional',
                            educ == 123 ~ "Master's",
                            educ == 111 ~ "Bachelor's",
                            educ %in% c(91, 92) ~ "Associates",
                            educ >= 73 ~ 'High School',
                            educ >= 2 ~ 'None'),
         female = (sex == 2)*1,
         children = (nchild > 0)*1,
         black = (race == 200)*1,
         asian = (race %in% 651:652),
         hrly_wage = incwage/(50*40),
         renter = (hhtenure %in% c(2, 3)*1)) %>%
  na.omit()

rm(educ_yos)

hist(cps$hrly_wage)         
hist(log(cps$hrly_wage))


##==========================##
## ------------------------ ##
#### 2) Basic Regressions ####
## ------------------------ ##
##==========================##

?lm

## --------------------- ##
#### 2.1) Coefficients ####
## --------------------- ##

fit = lm(log(hrly_wage) ~ yos, data = cps)
summary(fit)
# Such crazy significance is due to large sample
# Recall that se(\hat{beta_1}) = \sqrt(  1/(n-2) * SSR/SST_x  )
# As n gets large, \sqrt(1/(n-2)) becomes VERY small:
sqrt(1/(fit$df.residual))
# Which means the t statistics become VERY large
sfit = summary(fit)
sfit$coefficients[2, 3]


# So, about those cofficient formulas
# beta_1
b1 = cov(log(cps$hrly_wage), cps$yos)/var(cps$yos)
# beta_0
mean(log(cps$hrly_wage)) - mean(cps$yos) * b1


# Mincer regression
fit_m = lm(log(hrly_wage) ~ yos + exp + I(exp^2), data = cps)
fit_m %>% summary

# Mincer plus
fit_mp = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
             female*children + black + asian, data = cps)
summary(fit_mp)

?factor
class(cps$degree)
unique(cps$degree)
cps$degree = factor(cps$degree, levels = c("None", "High School", "Associates",
                              "Bachelor's", "Master's", "Ph.D.",
                              "Professional"))
class(cps$degree)

# Mincer plus
fit_mp = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
              female*children + black + asian, data = cps)
summary(fit_mp)

model.matrix(fit_mp) %>% head()

# Frisch-Waugh-Lovell
fit_mp_x = lm(yos ~ exp + I(exp^2) + degree +
                female*children + black + asian, data = cps)
x_tilde = fit_mp_x$residuals

fit_mp # 0.0236937
cov(log(cps$hrly_wage), x_tilde)/var(x_tilde)

## ------------------------------ ##
#### 2.2) Output Interpretation ####
## ------------------------------ ##
summary(fit_mp)
# yos - an increase in one year of schooling is ASSOCIATED WITH a 
#   2% increase in hourly wages (conditional on sample selection and
#   other regressors)
# exp
# w = ... + exp*b2 + exp^2 * b3 + ....
# dw/dexp = b2 + 2*exp*b3
b_mp = coef(fit_mp)
exp = seq(min(cps$exp), max(cps$exp), length = 100)
dw = b_mp[3] + 2*exp*b_mp[4]
plot(dw ~ exp)
w_hat = exp*b_mp[3] + 2*exp^2*b_mp[4]
plot(w_hat ~ exp)

exp_bar = mean(cps$ex)
b_mp[3] + 2*exp_bar*b_mp[4]
# having the average level of experince of 21 years is associated
# with a 21% increase in hourly wages


# Showing reuslts in stargazer
?stargazer
stargazer(fit, fit_m, fit_mp,
          type = 'text',
          omit.stat = c('adj.rsq', 'ser', 'f'),
          keep = c('yos', 'exp', 'I(exp^2)'),
          covariate.labels = c('Years of Schooling', 'Experience',
                               'Experience Squared'),
          add.lines = (list(c('Demographic Controls', 'No', 'No', 'Yes'),
                            c('Degree Controls', rep('No', 2), 'Yes'))))

## ------------------------------- ##
#### 2.3) Weighted Least Squares ####
## ------------------------------- ##
fit_mp_w = lm(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
              female*children + black + asian, 
              data = cps, weights = asecwt)
summary(fit_mp_w)
stargazer(fit_mp, fit_mp_w, type = 'text')

## --------------------------- ##
#### 2.4) Heteroskedasticity ####
## --------------------------- ##
plot(log(hrly_wage) ~ yos, data = cps)
plot(log(cps$hrly_wage) ~ x_tilde)


help(package = 'fixest')
?feols
fit_mp_r = feols(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
             female*children + black + asian, 
           data = cps, vcov = 'hetero')
fit_mp_r

stargazer(fit_mp, fit_mp_r)
?etable
etable(fit_mp, fit_mp_r)

fit_mp2 = feols(log(hrly_wage) ~ yos + exp + I(exp^2) + degree +
                   female*children + black + asian, 
                 data = cps)
etable(fit_mp2, fit_mp_r,
       se.below = TRUE,
       fitstat = c('n', 'r2'),
       dict = c(yos = 'Years of Schooling',
                exp = 'Experience'),
       keep = c('Years of Schooling', 'Experience'),
       extralines = list("Demographic Controls" = c('Yes', 'Yes')))
