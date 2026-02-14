rm(list = ls())
library(pacman)
p_load(data.table,psych,lfe,stargazer,foreign,ggplot2,wesanderson,dpylr)

dir <- 'C:/Users/dab320/Dropbox/Research/GreenInfrastructure/Seattle/'
source('c:/Users/dab320/Dropbox/Research/Tools/Software/R/my_functions.R')


##### LOAD DATA #####
load(paste0(dir,'RData/RainwiseSales_Jun19.RData'))
rain.sales[,degree := college + adv.degree]

rain.sales[,list(pin,zip,district,
                 date,price,
                 sqft,lot,stories,beds,baths,yearbuilt,year.ren,condition,
                 rainier,cascades,skyline,sound,lake.wash,lake.sam,waterfront,
                 pop,white,black,med.inc,degree,age)]





rain.sales <- rain.sales[,list(pin,tract,bg,zip,district,
                               date,sale.year,yearmonth,price,
                               sqft,lot,stories,beds,baths,yearbuilt,year.ren,condition,
                               rainier,cascades,territorial,skyline,sound,lake.wash,lake.sam,waterfront,
                               pop,white,black,asian,med.inc,degree,age,
                               rain.year,rainwise,bmp,water.gal,
                               tree.hood,park.hood,park.num.mile,rw.num.mile,pub.num.mile,priv.num.mile)]

# rain.sales <- sample_frac(rain.sales,.1)
save(rain.sales, file = paste0('seattle_housing.RData'))
write.csv(rain.sales, file = paste0('seattle_housing.csv'),row.names = F)



