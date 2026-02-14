### intro stuff at the top of every script ###
## note: a line starting with '#' is a comment and not evaluated 
## ctrl + enter runs a line of code or a highlighted chunk of code 

rm(list = ls()) ## clear the environment
library(pacman) ## load the 'pacman' package; the packman package needs to be installed first
# p_load(data.table,psych,lfe,stargazer,foreign,ggplot2,wesanderson)  ## load packages via pacman (installs if not yet installed)


##### define some objects #####
a <- 2

b <- a + 2

c <- a + b

numbers <- c(2, 4, 6)

numbers.2 <- c(a, b, c)

letters <- c('a','b','c')

factors <- as.factor(letters)

factors.2 <- as.factor(numbers)


##### explore classes of objects #####
class(a)
class(numbers)
class(letters)
class(factors)


##### perform functions on objects #####
mean(numbers)

mean(numbers.2)

mean(letters)

mean(factors.2)


### write our own function ###
our_function <- function(arg1, arg2, arg3) {
  arg1*arg2*arg3
}
## see our functin in the 'Functions' section of the 'Environment' panel

our_function(arg1=2, arg2=4, arg3=6)

our_function(2,4,6)

our_function(a,b,c)

our_function('a','b','c')


##### write a function that saves the output #####
our_function2 <- function(arg1, arg2, arg3) {
  out <- arg1*arg2*arg3
}

our_function2(2,4,6)

our_function3 <- function(arg1, arg2, arg3) {
  out <- arg1*arg2*arg3
  return(out)
}

our_function3(2,4,6)

## save the output to an object
output <- our_function3(2,4,6)


##### working with data frames #####
df <- data.frame(x1 = c(a,b,c),
                 x2 = numbers,
                 x3 = letters,
                 x4 = factors,
                 x5 = c(1,2,3))

## perform some operations on the data frame columns (variables)
df$x1

df$x3

df$x4

mean(df$x1)

## add a variable
df$x6 <- 3

df

df$x7 <- seq(1:3)

## make a bigger data frame by replicating our data frame
rbind(df,df,df,df,df,df,df,df,df,df)

## we need to assign it as an object to keep it
df2 <- rbind(df,df,df,df,df,df,df,df,df,df)
df
df2

## we could also overwrite our previous data frame
df <- rbind(df,df,df,df,df,df,df,df,df,df)
df



##### let's look at some packages and some data #####
## I almost exclusively use data.table 
# install.packages('data.table') ## only do this once

library(data.table)

?data.table

## we can convert a data frame into a data.table
dt <- as.data.table(df)
dt

## one nice feature of data.tables is how they present big datasets
dt <- as.data.table(rbind(df,df,df,df))

## compare this to a data frame
rbind(df,df,df,df)

## now let's load a data set with some more realistic data ##
load('seattle_housing.RData') ## this object is an RData file and already has a name 'rain.sales'

## we can remove this file and load a more standard file format (csv)
rm(rain.sales)
read.csv('seattle_housing.csv') ## if we don't assign this as an object it just loads the data but does not 'keep' it

## load and assign the data 
rain.sales <- read.csv('seattle_housing.csv')
rain.sales

## you can also load it directly as a data.table
rain.sales <- as.data.table(read.csv('seattle_housing.csv'))
rain.sales  ## I find data tables easier to summarise and view the data

##### working with data.tables ####

## subset rows
rain.sales[1:1000]
rain.sales[district=='SEATTLE']
rain.sales[district=='SEATTLE',]


## subset columns
rain.sales[,1:9]
rain.sales[,c(1:9)]
rain.sales[,c(1:6,9)]
rain.sales[,list(pin,tract,bg,zip,district,date,price)]

## subset both
rain.sales[1:1000,list(pin,tract,bg,zip,district,date,price)]
rain.sales[district=='SEATTLE',list(pin,tract,bg,zip,district,date,price)]

## take mean of different subsets 
mean(rain.sales[,price])
mean(rain.sales[district=='SEATTLE',price])
mean(rain.sales[district=='SEATTLE' & sale.year==2018,price])

## add a variable
rain.sales[,mean.price := mean(price,na.rm = T)]

## add a variable by group
rain.sales[,mean.district.price := mean(price,na.rm=T), by = district]

rain.sales[,mean.district.year.price := mean(price,na.rm=T), by = c('district','sale.year')]


##### plotting with ggplot2 #####
install.packages('ggplot2')

library(ggplot2)


ggplot(data=rain.sales,aes(x=price)) + geom_density()

ggplot(data=rain.sales[price<3000000],aes(x=price)) + geom_density()

ggplot(data=rain.sales[price<3000000],aes(x=price)) + geom_density() + theme_minimal()

library(scales)
ggplot(data=rain.sales[price<3000000],aes(x=price)) + geom_density(fill='dodgerblue',color=NA) + 
  scale_x_continuous(labels=scales::dollar) + theme_minimal(base_size = 20)


ggplot(data=rain.sales[price<3000000],aes(x=price, color = district)) + geom_density(fill=NA) + 
  scale_x_continuous(labels=scales::dollar) + theme_minimal(base_size = 20)


ggplot(data=rain.sales[price<3000000 & 
                         district %in% c('SEATTLE','BELLEVUE','AUBURN','BOTHEL','MERCER ISLAND','REDMOND','KIRKLAND')],
       aes(x=price, color = district)) + geom_density(fill=NA) + 
  scale_x_continuous(labels=scales::dollar) + theme_minimal(base_size = 20)

ggplot(data=rain.sales[price<3000000 & 
                         district %in% c('SEATTLE','BELLEVUE','AUBURN','BOTHEL','MERCER ISLAND','REDMOND','KIRKLAND')],
       aes(x=price, fill = district)) + geom_density(color=NA, alpha=0.5) + 
  scale_fill_discrete(name="City") + 
  scale_x_continuous(labels=scales::dollar) + theme_minimal(base_size = 20)

ggplot(data=rain.sales[price<3000000 & 
                         district %in% c('SEATTLE','BELLEVUE','AUBURN','BOTHEL','MERCER ISLAND','REDMOND','KIRKLAND')],
       aes(x=price, fill = district)) + geom_density(color=NA, alpha=0.5) + 
  facet_wrap(~district) +
  scale_fill_discrete(guide=FALSE) + 
  scale_x_continuous(labels=scales::dollar) + theme_minimal(base_size = 20)

##### regression in R #####
## create some variables about view
rain.sales[,water.view := ifelse(sound>0 | lake.wash>0 | lake.sam>0,1,0)]
rain.sales[,mount.view := ifelse(rainier>0 | cascades>0 | territorial>0,1,0)]

m1 <- lm(price ~ water.view + mount.view,
         data=rain.sales)
summary(m1)

m2 <- lm(price ~ water.view + mount.view + tract + sale.year,
         data=rain.sales)
summary(m2)


m3 <- lm(price ~ water.view + mount.view + as.factor(tract) + as.factor(sale.year),
         data=rain.sales)
summary(m3)

install.packages('lfe')
library(lfe)
m4 <- felm(price ~ water.view + mount.view |
            tract + sale.year,
         data=rain.sales)
summary(m4)

m5 <- felm(price ~ water.view + mount.view + 
             yearbuilt + sqft + lot + beds + baths |
             tract + sale.year,
          data=rain.sales)
summary(m5)

library(stargazer)

stargazer(m1,m2,m3,m4,m5,
          type='text', digits = 2,
          omit = c('as.factor','Constant'),
          omit.stat	= c('aic','f','ser'))


stargazer(m1,m2,m3,m4,m5,
          type='text', digits = 2,
          keep = c('view'),
          omit.stat	= c('aic','f','ser'))

stargazer(m1,m2,m3,m4,m5,
          type='latex', digits = 2,
          keep = c('view'),
          omit.stat	= c('aic','f','ser'),
          out = 'reg_table.tex')














