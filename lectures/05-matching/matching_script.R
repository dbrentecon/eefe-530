rm(list=ls())

### loack pacakges
library(pacman)
p_load(
  broom, tidyverse,
  ggplot2, ggthemes, ggforce, ggridges,
  latex2exp, viridis, extrafont, gridExtra,
  kableExtra, snakecase, janitor,
  data.table, dplyr,
  lubridate, knitr,
  estimatr, here, magrittr, stargazer,
  Matching,MatchIt,Hmisc,rgenoud,optmatch
)

##### EXAMPLE 1: MATCHING ON A SINGLE DISCRETE VARIABLE  #####

### create some covariates and error terms
set.seed(5302020)
dat <- data.table(
  x1 = sample(1:10,1000,replace=T), #discrete covariate
  x2 = rnorm(1000,mean=0,sd=2.5), # continuous covariate
  eta = rnorm(1000), # unobserved factor in assigning treatment
  e = rnorm(1000) # unobserved factor in outcome
)

### assign treatment dependent on the discrete (x1), but not continuous variable (x2)
dat[, p := (x1+eta)/10]
dat[p>1, p := 1]
dat[p<0, p := 0]
dat[,  d := rbinom(n = 1000, size = 1, prob = p)]

### assess treatment overlap on x1
table(dat$d,dat$x1)  ## there is overlap, but not a lot in the tails

### assign outcome
tau <- 5 ## treatment effect (constant)
dat[,y := tau*d + 1*x2 + e]


### regression 
tau.hat.reg <- lm(y ~ d + x1, data=dat)  ## linear x1; no control for x2
tau.hat.reg2 <- lm(y ~ d + as.factor(x1), data=dat)  ## saturated in x1; no control for x2
tau.hat.reg3 <- lm(y ~ d + as.factor(x1) + x2, data=dat)  ## saturated in x1; control for x2

stargazer(tau.hat.reg,tau.hat.reg2,tau.hat.reg3,
          type='text',
          keep = 'd', 
          omit.stat	= c('aic','f','ser')
          )

### exact matching by hand
dat[d==1,mean.1 := mean(y),by=x1] ## mean of treatment within x1
dat[d==0,mean.0 := mean(y),by=x1] ## mean of control within x1]

### this just gets rid of NA by treatment
dat[,mean.1 := mean(mean.1,na.rm=T),by=x1]
dat[,mean.0 := mean(mean.0,na.rm=T),by=x1]

### generate differnce in means
dat[,mean.dif := mean.1-mean.0]

### assign weights based on density of x1 among treated
weights <- as.data.table(table(dat[d==1,x1]))  ## get the counts of x1 among treated
setnames(weights,c('x1','weight')) ## rename 
weights[,weight := weight/length(dat[d==1,d])] ## normalize by number of treated obs

### pull out mean differences within each value of x1
tau_x <- unique(dat[,list(x1,mean.dif)])

### merge with weights
class(tau_x$x1)
class(weights$x1)
weights[,x1:=as.integer(x1)]
tau_x <- merge(tau_x,weights,by='x1')

## generate weighted average
tau_x[,mean.dif.weight := mean.dif*weight]
sum(tau_x$mean.dif.weight)
summary(tau.hat.reg)$coef[2]

### Matching pacakge

### exact match 
tau.hat.match.exact <- Match(Y=dat$y, Tr=dat$d, X=dat$x1,
                       exact=T,
                       estimand = 'ATT',
                       BiasAdjust = F)

### asses balance
MatchBalance(d~x1, match.out = tau.hat.match.exact, nboot=100, data=dat)

summary(tau.hat.match.exact)
sum(tau_x$mean.dif.weight)

## NOTE: Exact matching from the package is the same as our weighted difference in means.


##### EXAMPLE 2:  MATCHING ON A CONTINUOUS AND DISCRETE VARIABLE  ####
### create some covariates and error terms
rm(list=ls())
set.seed(5302020)
dat <- data.table(
  x1 = sample(1:10,1000,replace=T), #discrete covariate
  x2 = rnorm(1000, mean=0, sd=2.5), # continuous covariate
  eta = rnorm(1000,mean=0), # unobserved factor in assigning treatment
  e = rnorm(1000) # unobserved factor in outcome
)

### make treatment dependent on the discrete (x1) and the continuous variable (x2)
dat[, p := (x1+x2+eta)/20]
dat[p>1, p := 1]
dat[p<0, p := 0]
dat[,  d := rbinom(n = 1000, size = 1, prob = p)]

### assess treatment overlap on x1
table(dat$d,dat$x1)

### assign outcome
tau <- 5 ## treatment effect (constant)
dat[,y := tau*d + 1*x2 + e]

### regression 
tau.hat.reg <- lm(y~d + x1, data=dat)
tau.hat.reg2 <- lm(y~d +x1 + x2, data=dat)

stargazer(tau.hat.reg,tau.hat.reg2,
          type='text',
          # keep = 'd', 
          omit.stat	= c('aic','f','ser')
)

## NOTE: Now including x2 is important because it is correlted with treatment

# ### inverse variance
# tau.hat.match.var <- Match(Y=dat$y, Tr=dat$d, X=dat[,list(x1,x2)],
#                            Weight=1, ## weight by inverse variance
#                            estimand = 'ATT',
#                            BiasAdjust = F)
# 
# MatchBalance(d ~ x1 + x2,match.out = tau.hat.match.var,nboot=100, data=dat)
# 
# summary(tau.hat.match.var)
# summary(tau.hat.reg2)$coef[2,]

### Maholanobis distance
tau.hat.match.mah <- Match(Y=dat$y, Tr=dat$d, X=dat[,list(x1,x2)],
                           Weight=2,
                           estimand = 'ATT',
                           BiasAdjust = F)

MatchBalance(d ~ x1 + x2,match.out = tau.hat.match.mah,nboot=100, data=dat)

summary(tau.hat.match.mah)
# summary(tau.hat.match.var)
summary(tau.hat.reg2)$coef[2,]

### propensity score with nearest neighbor matching
logit  <- glm(d ~ x1+ x2, family = binomial, data = dat)
dat[,pscore := logit$fitted.values]

tau.hat.match.ps <- Match(Y=dat$y, Tr=dat$d, X=dat$pscore,
                           Weight=2,
                           estimand = 'ATT',
                           BiasAdjust = F)

MatchBalance(dat$d~dat$x1,match.out = tau.hat.match.ps,nboot=100)

summary(tau.hat.match.ps)
summary(tau.hat.match.mah)
# summary(tau.hat.match.var)
summary(tau.hat.reg2)$coef[2,]


### propensity score, augmented
tau.hat.match.ps2 <- Match(Y=dat$y, Tr=dat$d, X=dat[,list(x1,x2,pscore)],
                          Weight=2,
                          estimand = 'ATT',
                          BiasAdjust = F)

MatchBalance(dat$d~dat$x1,match.out = tau.hat.match.ps2,nboot=100)

summary(tau.hat.match.ps2)
summary(tau.hat.match.ps)
summary(tau.hat.match.mah)
# summary(tau.hat.match.var)
summary(tau.hat.reg2)$coef[2]

### reg with pscore
tau.hat.reg.ps <- lm(y ~ d + pscore, data=dat)
summary(tau.hat.reg.ps)$coef
summary(tau.hat.reg2)$coef


## genetic matching 
gen <- GenMatch(Tr = dat$d, X = dat[,list(x1,x2)], 
                BalanceMatrix = dat[,list(x1,x2)],
                pop.size = 500)

tau.hat.match.gen <- Match(Y = dat$y, Tr = dat$d, X = dat[,list(x1,x2)], Weight.matrix = gen)

MatchBalance(d ~ x1 + x2,match.out = tau.hat.match.gen,nboot=100, data=dat)
MatchBalance(d ~ x1 + x2,match.out = tau.hat.match.mah,nboot=100, data=dat)

summary(tau.hat.match.gen)


##### EXAMPLE 3: NONLINEAR ASSIGNMENT & HETEROGENOUS EFFECTS ####

### create some covariates and error terms
rm(list=ls())
set.seed(5302020)
dat <- data.table(
  x1 = sample(1:10,1000,replace=T), #discrete covariate
  x2 = rnorm(1000, mean=0, sd=2.5), # continuous covariate
  eta = rnorm(1000,mean=0), # unobserved factor in assigning treatment
  e = rnorm(1000) # unobserved factor in outcome
)

### make treatment dependent on the discrete (x1), but not continuous variable (x2)
dat[, p := (x1^2+x2+eta)/100]
dat[p>1, p := 1]
dat[p<0, p := 0]
dat[,  d := rbinom(n = 1000, size = 1, prob = p)]

### force some lack of overlap
dat[x1<=2,d:=0]
dat[x1>=9,d:=1]
table(dat$d,dat$x1)

### assign outcome
tau <- 5 ## treatment effect (constant)
dat[,y := tau*d + 1*x2 + e]

### regression 
tau.hat.reg <- lm(y~d + x1 + x2, data=dat)
summary(tau.hat.reg)$coef[2,]


### inverse variance
tau.hat.match.var <- Match(Y=dat$y, Tr=dat$d, X=dat[,list(x1,x2)],
                           Weight=1,
                           estimand = 'ATT',
                           BiasAdjust = F)

MatchBalance(d ~ x1+ x2,match.out = tau.hat.match.var,nboot=500, data=dat)

summary(tau.hat.match.var)
summary(tau.hat.reg)$coef[2]

### Maholanobis distance
tau.hat.match.mah <- Match(Y=dat$y, Tr=dat$d, X=dat[,list(x1,x2)],
                           Weight=2,
                           estimand = 'ATT',
                           BiasAdjust = F)

MatchBalance(d ~ x1+ x2,match.out = tau.hat.match.mah, nboot=500, data=dat)

summary(tau.hat.match.mah)
summary(tau.hat.match.var)
summary(tau.hat.reg2)$coef[2]

### propensity score
logit  <- glm(d ~ x1+ x2, family = 'binomial', data = dat)
dat[,pscore := logit$fitted.values]

tau.hat.match.ps <- Match(Y=dat$y, Tr=dat$d, X=dat$pscore,
                          Weight=2,
                          estimand = 'ATT',
                          BiasAdjust = F)

MatchBalance(d ~ x1+ x2,match.out = tau.hat.match.ps,nboot=500, data=dat)

summary(tau.hat.match.ps)
summary(tau.hat.match.mah)
summary(tau.hat.match.var)
summary(tau.hat.reg)$coef[2]

## logit with interactions
logit2  <- glm(d ~ x1 + x1^2 + x2 + x2^2 + x1:x2, family = 'binomial', data = dat)
dat[,pscore2 := logit2$fitted.values]

tau.hat.match.ps2 <- Match(Y=dat$y, Tr=dat$d, X=dat$pscore2,
                          Weight=2,
                          estimand = 'ATT',
                          BiasAdjust = F)
summary(tau.hat.match.ps)
summary(tau.hat.match.ps2)

## check out overlap in pscore
library(psych)
describeBy(dat$pscore,dat$d)


### propensity score, enforcing overlap
tau.hat.match.ps3 <- Match(Y=dat[x1>2 & x1 <9,y], Tr=dat[x1>2 & x1 <9,d], X=dat[x1>2 & x1 <9,list(x1,x2,pscore)],
                           Weight=2,
                           estimand = 'ATT',
                           BiasAdjust = F)

summary(tau.hat.match.ps3)
summary(tau.hat.match.ps2)
summary(tau.hat.match.ps)
summary(tau.hat.match.mah)
summary(tau.hat.match.var)
summary(tau.hat.reg2)$coef[2]

### reg with pscore
tau.hat.reg.ps <- lm(y ~ d + pscore, data=dat)
summary(tau.hat.reg.ps)$coef
summary(tau.hat.reg)$coef

## genetic matching 
gen <- GenMatch(Tr = dat$d, X = dat[,list(x1,x2)], 
                BalanceMatrix = dat[,list(x1,x2)],
                pop.size = 500)

tau.hat.match.gen <- Match(Y = dat$y, Tr = dat$d, X = dat[,list(x1,x2)], Weight.matrix = gen)
MatchBalance(d ~ x1+ x2,match.out = tau.hat.match.gen,nboot=500, data=dat)

summary(tau.hat.match.gen)






