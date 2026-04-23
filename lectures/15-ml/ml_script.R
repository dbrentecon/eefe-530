rm(list=ls())

### loack pacakges
library(pacman)
p_load(
  broom, tidyverse,
  ggplot2, ggthemes, ggforce, ggridges,
  latex2exp, viridis, extrafont, gridExtra,
  kableExtra, snakecase, janitor,
  data.table, dplyr,foreign,haven,
  lubridate, knitr,
  estimatr, here, magrittr, stargazer, MASS, glmnet
)

#####  Example 1 - lots of noise #####
set.seed(19875)  # Set seed for reproducibility
n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


for (i in 0:10) {
  assign(paste("yhat", i, sep=""), predict(get(paste0('fit',i)), 
                                           s=get(paste0('fit',i))$lambda.1se,
                                           newx=x.test)
  )
  assign(paste0('mse',i),mean((y.test-get(paste0('yhat',i)))^2))
}

tab.1 <- data.table(
 'alpha' = c('0 (Ridge)','.1','.2','.3','.4','.5','.6','.7','.8','.9','1 (LASSO)'),
  MSE = round(c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10),2)
)

kable(tab.1,row.names = NA) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F) %>%
  row_spec(11, color = 'dodgerblue',bold=T)
  



#####  Example 2 - lots of noise & lots of signal #####
# Generate data
set.seed(342098)
n <- 1000    # Number of observations
p <- 5000     # Number of predictors included in model
real_p <- 1500  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


for (i in 0:10) {
  assign(paste("yhat", i, sep=""), predict(get(paste0('fit',i)), 
                                           s=get(paste0('fit',i))$lambda.1se,
                                           newx=x.test)
  )
  assign(paste0('mse',i),mean((y.test-get(paste0('yhat',i)))^2))
}

tab.2 <- data.table(
  'alpha' = c('0 (Ridge)','.1','.2','.3','.4','.5','.6','.7','.8','.9','1 (LASSO)'),
  MSE = round(c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10),2)
)

kable(tab.2,row.names = NA) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F) %>%
  row_spec(1, color = 'dodgerblue',bold=T)



#####  Example 3 - varying noise and varying signal with high correlation #####
set.seed(19873)
n <- 1000    # Number of observations
p <- 500     # Number of predictors included in model
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n, rep(0,p), CovMatrix)
y <- 10 * apply(x[, 1:20], 1, sum) + 
  5 * apply(x[, 21:40], 1, sum) +
  apply(x[, 41:140], 1, sum) +
  rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


for (i in 0:10) {
  assign(paste("yhat", i, sep=""), predict(get(paste0('fit',i)), 
                                           s=get(paste0('fit',i))$lambda.1se,
                                           newx=x.test)
  )
  assign(paste0('mse',i),mean((y.test-get(paste0('yhat',i)))^2))
}

tab.3 <- data.table(
  'alpha' = c('0 (Ridge)','.1','.2','.3','.4','.5','.6','.7','.8','.9','1 (LASSO)'),
  MSE = round(c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10),2)
)

kable(tab.3,row.names = NA) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F) %>%
  row_spec(2, color = 'dodgerblue',bold=T)

# colnames(tab.1,c('alpha','MSE 1'))
# colnames(tab.2,c('alpha','MSE 2'))
# colnames(tab.3,c('alpha','MSE 3'))

tab.3 %>%
  mutate(
    alpha = cell_spec(alpha, 'html',color = 'dodgerblue',bold=T)
  )

example.tab <- kable(cbind(tab.1,tab.2,tab.3),row.names = NA,escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F) %>%
  add_header_above(c("Dataset 1" = 2, "Dataset 2" = 2, "Dataset 3" = 2), 
                   color = c('red','blue','green')) %>%
  row_spec(11, color = 'red',bold=T) %>%
  row_spec(1, color = 'blue',bold=T) %>%
  row_spec(2, color = 'green',bold=T)
  
example.tab
save(example.tab,file='reg_regress_ex.RData')

rm(example.tab)


rownames(tab) <- c()
kable(tab,col.names = NULL,row.names = NA) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, italic = T) %>%
  row_spec(1, bold = T) %>%
  row_spec(2, italic = T)

