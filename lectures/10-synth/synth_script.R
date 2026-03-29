### Synthetic control practice
## EEFE 530
## Daniel Brent

## clear workspace
rm(list=ls())

## load packages
library(pacman)
p_load(tidyverse,haven,Synth,devtools,SCtools,data.table)

# source("/Users/dab320/Dropbox/Research/Tools/Software/R/my_functions.R")
options(scipen = 999)

## not %in%
'%!in%' <- function(x,y)!('%in%'(x,y))  ## create your own function

## read data from Causal Mixtape
read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

texas <- as.data.table(read_data("texas.dta"))
                       
## check out data
texas

## data prep
## NOTE: this defines most of the important parameters for the synthetic control
## estimation.

dataprep_out <- dataprep(
  foo = texas, ## data
  predictors = c("poverty", "income"), ## predictor variables (all pre-treatment periods)
  predictors.op = "mean", ## functional form
  time.predictors.prior = 1985:1993, ## define pre-treatment period
  special.predictors = list( ## special predictors 
    list("bmprison", c(1988, 1990:1992), "mean"), ## outcome variable averegares in 1988 and 1990-1992
    list("alcohol", 1990, "mean"), ## alcohol consumption in 1990
    list("aidscapita", 1990:1991, "mean"), ## mean aids rate in 1990-1991
    list("black", 1990:1992, "mean"), ## mean % black in 1990-1992
    list("perc1519", 1990, "mean")), ## mean % age 15-19 in 1990
  dependent = "bmprison", ## outcome variable
  unit.variable = "statefip", ## unit variable
  unit.names.variable = "state", ## unit names
  time.variable = "year", ## time variable 
  treatment.identifier = 48, ## treated state
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56), ## control states
  time.optimize.ssr = 1985:1993,  ## time to optimize SSR
  time.plot = 1985:2000 ## time to plot
)

## check out control states - it's all controls
unique(texas[statefip %in% c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),state])
unique(texas[statefip %!in% c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),state])

## run synthetic control
synth_out <- synth(data.prep.obj = dataprep_out)

## save and load because it takes some time
save(synth_out, file='synth_out.RData')
load('synth_out.RData')


## plot outcome for treated and synthetic control
path.plot(synth_out, dataprep_out,Xlab='Year',Ylab='Predicted black male prisoners per capita')

## plot difference between treated and synthetic control
gaps.plot(synth_out, dataprep_out,Xlab='Year',Ylab='Gap in predicted black male prisoners per capita')

## check out weights
synth_out$solution.w
synth_out$solution.w[synth_out$solution.w[,1]>.01]

## generate placebos (takes some time)
placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)

## save and load
save(placebos, file='placebos.RData')
load('placebos.RData')

## check out placebo plot
plot_placebos(placebos,discard.extreme = F,
              xlab='Year',ylab='Gap in predicted black male prisoners per capita')
plot_placebos(placebos,discard.extreme = T,
              xlab='Year',ylab='Gap in predicted black male prisoners per capita')
plot_placebos(placebos,discard.extreme = T, mspe.limit = 10,
              xlab='Year',ylab='Gap in predicted black male prisoners per capita')
plot_placebos(placebos,discard.extreme = T, mspe.limit = 5,
              xlab='Year',ylab='Gap in predicted black male prisoners per capita')

## look at MSPE ratios with and without outliers
mspe.plot(placebos, discard.extreme = FALSE, plot.hist = TRUE)
mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 5, plot.hist = TRUE)


## perform backdating robustness check
dataprep_out_back <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1990,
  special.predictors = list(
    list("bmprison", c(1988:1990), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990, "mean"),
    list("black", 1990, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1990,
  time.plot = 1985:2000
)

## estimation
synth_out_back <- synth(data.prep.obj = dataprep_out_back)

## save and load
save(synth_out_back, file='synth_out_back.RData')
load('synth_out_back.RData')

## plot path and gaps
path.plot(synth_out_back, dataprep_out_back,Xlab='Year',Ylab='Predicted black male prisoners per capita')
gaps.plot(synth_out_back, dataprep_out_back,Xlab='Year',Ylab='Gap in predicted black male prisoners per capita')
gaps.plot(synth_out, dataprep_out,Xlab='Year',Ylab='Gap in predicted black male prisoners per capita')

### robustness 
dataprep_out_white <- dataprep(
  foo = texas, ## data
  predictors = c("poverty", "income"), ## predictor variables (all pre-treatment periods)
  predictors.op = "mean", ## functional form
  time.predictors.prior = 1986:1993, ## define pre-treatment period
  special.predictors = list( ## special predictors 
    list("wmprison", c(1988, 1990:1992), "mean"), ## outcome variable averegares in 1988 and 1990-1992
    list("alcohol", 1990, "mean"), ## alcohol consumption in 1990
    list("aidscapita", 1990:1991, "mean"), ## mean aids rate in 1990-1991
    list("black", 1990:1992, "mean"), ## mean % black in 1990-1992
    list("perc1519", 1990, "mean")), ## mean % age 15-19 in 1990
  dependent = "wmprison", ## outcome variable
  unit.variable = "statefip", ## unit variable
  unit.names.variable = "state", ## unit names
  time.variable = "year", ## time variable 
  treatment.identifier = 48, ## treated state
  controls.identifier = c(1,2,4:5,9:13,15:33,37:42,44,46:47,49:51,53:56), ## control states
  time.optimize.ssr = 1986:1993,  ## time to optimize SSR
  time.plot = 1986:2000 ## time to plot
)

texas[year==1985]
texas[year==1985 & is.na(wmprison)]
texas[year==1986 & is.na(wmprison)]
texas[is.na(wmprison)]
texas[is.na(bmprison)]
unique(texas[is.na(wmprison & year>1985),state])
unique(texas[is.na(wmprison & year>1985),list(state,statefip)])
texas[state %!in% unique(texas[is.na(wmprison & year>1985),state])]

texas2 <- texas[state %!in% unique(texas[is.na(wmprison) & year>1985,state])]
synth_out_white <- synth(data.prep.obj = dataprep_out_white)


path.plot(synth_out_white, dataprep_out_white,Xlab='Year',Ylab='Predicted white male prisoners per capita')
gaps.plot(synth_out_white, dataprep_out_white,Xlab='Year',Ylab='Gap in predicted white male prisoners per capita')
gaps.plot(synth_out, dataprep_out,Xlab='Year',Ylab='Gap in predicted black male prisoners per capita')
