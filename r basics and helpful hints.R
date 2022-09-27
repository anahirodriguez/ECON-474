# matrices


matrix(1,2,3,4)
matrix(c(1,2,3,4))
m = matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = T)



# data.frame

df = data.frame(m)
df$X1


# indexing - i can row and j cannot
  # ith row and jth column
# 1st column, 2nd row
df[2,1]



ex_df = data.frame(states = seq(1,50), year = 2020)
head(ex_df)
tail(ex_df)



# loops

for(i in c(1,3,6:10)) {
  print(i)
}



# integer division
20/3 # regular division
20 %/% 3 # integer division
20 %% 3 # remainder


# print only odd numbers
for(i in c(1,3,6:10)) {
  if(i%%2 != 0){
    print(paste(i, 'is odd'))
  }
}




# real data example

# load in data
gm = read.csv("/Users/anahi/Desktop/data econ 474/gapminder_2007.csv")
gm[gm$continent == 'Oceania',]




# function rules

awaken_R = function(){
  print('Hello, world!')
}


awaken_R()

double = function(x){
  return(x*2)
  
}
double(4)



# plotting
mean(gm$gdpPercap)
median(gm$gdpPercap)
hist(gm$gdpPercap)

hist(log(gm$gdpPercap))



plot(lifeExp ~ gdpPercap, data = gm)
plot(lifeExp ~ gdpPercap, data = gm, 
     xlab = 'GDP Per Capita', ylab = 'Life Expentency',
     main = "GDPc vs. Life Expectancy 2007", pch = 16)






library(ggplot2)


ggplot(data = gm, aes(x = gdpPercap, y = lifeExp, 
                      color = continent, size = pop)) + 
  geom_point(alpha = 0.5) +
  scale_size_continuous(guide = 'none') +
  labs(x = 'GDP per Capita', y = 'Life Expectancy',
       main = 'GDPc vs. Life Expectancy 2007') + 
  #theme(text = element_text(size = 20)) +
  geom_text(aes(label = ifelse(pop >=quantile(pop, .90),
                               country, '')),
            size = 3, hjust = 0, vjust = 0, nudge_x = 0.1) 



install.packages('dplyr')
Yes
library(dplyr)  


