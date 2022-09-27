##############
# Anahi Rodriguez
# Thornton recreation
# 09/20/22
##############


#### 1) preliminaries ####
library(tidyverse)
library(fixest)
library(haven)
install.packages('cowplot')
library(cowplot)



df = read_csv('Thornton 2008 main sample.csv')



# balance table

# variables of interes
vrbls = c('got', # outcome
          'any', # treatment
          'tinc', #treatment specifically the dosage - random incentive amount
          'distvct', # distance to vct center
          'simaverage', # siumulated average distance to vct center
          'tb', # taken HIV test before interview
          'male', # male
          'age', # age
          'rumphi', 
          'balaka',
          'mar', # married
          'educ2004', # pre-period education
          'land2004', # pre-period owned land
          'hiv2004', # pre-period HIV result
          'thinktreat', # think HIV treatment will be available in the next 5 years
          'hadsex12', # had sex within the last year
          'usecondom04')  # used a condom in the last year



df %>%
  select(vrbls) %>%
  na.omit()
nrow(df)
# it is a problem that there is incomplete information for the 'full sample' becuase
# then you dont know if changes in results are due to including the variables
# or dropping observations


# checking out the NA counts
lapply(df[, vrbls], function(x) sum(is.na(x)))

temp = lapply(df[, vrbls], function(x) sum(is.na(x))) %>%
  unlist

temp[temp > 0]


# creating the balance table
# we need one column for:
#     1) treatment group averages
#     2) control group averages
#     3) difference in averages
#     4) p-value for differences in significance


bal_tab = df %>%
  select(vrbls) %>%
  group_by(any) %>%
  # summarizing with means ACROSS all variables besides the group/treatment variable 'any'
  summarise(across(vrbls[vrbls 1= 'any'], mean, na.rm = T)) %>%
  # transposing the data by first pivoting longer than wider
  pivot_longer(!any, names_to = 'variable', values_to = 'm') %>%
  pivot_wider(values_from = m, 
              names_from = any,
              names_prefix = 'D_') %>%
  # reordering the columns
  select(variable, D_1, D_0) %>%
  # calculate the difference
  mutate(diff = D_1 - D_0)

bal_tab$pvalue = NA
for(v in vrbls[vrbls != 'any']) {
  bal_tab$pvalue[bal_tab$variable == v] = t.test(df[[v]] ~ df$any)$p.value
}

bal_tab


#### 2) Table 4: Incentive of Learning HIV status effects ####

tab4_1 = feols(got ~ any + tb + male + age + rumphi + balaka, data = df, cluster = 'vilnum')
tab4_2 = feols(got ~ any + tinc +
                 tb + male + age + rumphi + balaka, data = df, cluster = 'vilnum')
tab4_3 = feols(got ~ any + tinc + I(tinc^2) +
                 tb + male + age + rumphi + balaka, data = df, cluster = 'vilnum')
tab4_4 = feols(got ~ any + tinc + I(tinc^2) +
                 tb + distvct + I(distv t^2) +
                 male + age + rumphi + balaka, data = df, cluster = 'vilnum')
tab4_5 = feols(got ~ any + tinc + I(tinc^2) +
                 tb + simaverage + over +
                 male + age + rumphi + balaka, data = df, cluster = 'vilnum')


etable(tab4_1, tab4_2, tab4_3, tab4_4, tab4_5)
dict = c(got = 'Got HIV Incentive',
         any = 'Any Incentive',
         vilnum = 'Village')

etable(tab4_1, tab4_2, tab4_3, tab4_4, tab4_5,
       dict = dict,
       fitstat = c('n', 'r2'),
       se.below = T,
       poly_dict = c('','^2'))


#### 3) Figure 3 ####

# Figure 3 is plitting coefficient estimates from regressions
# so, lets run some regression

#### 3.1) panel A ####

# create a factor variable 'incentive' with figure labels to make plotting easier
# i am doing a factor becaue the default ordering in ggplot2 of characters is 
#   to put them in alphabetical order, which does not match the plot
# factors force your specified order

df = df %>%
  mutate(incentive = ifelse(any == 0, 'No Incentive','Incentive'))
df$incentive = factor(df$incentive, levels = c('No Incentive','Incentive'))

# regressins: note -1 removes the intercept
fit3_a = lm(got ~ incentive - 1, data = df)
summary(fit3_a)
coef(fit3_a)
confint(fit3_a)

df_fig3_a = data.frame(beta = coef(fit3_a),
                       group = factor(c('No Incentive','Incentive'),
                                      levels = c('No Incentive','Incentive'))) %>%
  # adding on the confidence interval data
  bind_cols(confint(fit3_a))


ggplot(df_fig3_a, aes(x = group, y = beta)) +
  geom_bar(stat = 'identity', # because we only have one value per group
           width = 0.4) + 
  geom_error_bar(aes(ymin = '2.5 %', ymax = '97.75 %'),
                 width = 0.1) +
  scale_y_continous(breaks = seq(0,1, by 0.2, limits = c(0,1)))


#### 3.2) Panel B ####
# creating bins of the continous variable 'tinc'
df$inc_group = df$tinc %>%
  cut(breaks = c(-1, 0.1 ,0.2 , 0.3, 0.5, 1, 1.5, 2, 2.5, 3))
levels(df%inc_group)
levels(df$inc_group)[1] = '[0]'

fit3_b = lm(got ~)


