rm(list = ls())
library(ggplot2)
library(data.table)
library(scales)
library(dplyr)
library(glue)

p <- 0.5 # this is a fair coin
n <- 20
b <- 1000000

heads.50 <- data.table(
  H = rbinom(n = b,
             size = n,
             prob = p)
)

ggplot(heads.50, aes(x = H)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill='dodgerblue',color=NA) +
  geom_text(aes(y = ((..count..)/sum(..count..)),
                label = scales::percent((..count..)/sum(..count..),accuracy=0.1)), 
            stat = "count", vjust = -0.25, size=3) +
  scale_y_continuous(labels = percent) +
  labs(title = "Percentage # heads from 20 tosses of fair coin 1 million times", y = "Percent", x = "# Heads") + 
  theme_minimal()


p <- 0.5 # this is a fair coin
n <- 20
b <- 1000000
heads.50 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.50[, prop := length(H)/length(heads.50$H), by = as.factor(H)]
heads.50.sum <-unique(heads.50[,list(H,prop)])
setorder(heads.50.sum,-H)
heads.50.sum[,alpha := cumsum(prop)]
setorder(heads.50.sum,H)
# https://gist.github.com/Teebusch/db0ab76d31fd31a13ccf93afa7d77df5
area.50 <- as.data.table(
  bind_rows(old = heads.50.sum,
            new = heads.50.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
  )

ggplot(heads.50.sum,aes(x=H,y=prop)) + geom_step(color='dodgerblue') + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  theme_minimal()


p <- 0.75 # this is a not a fair coin
heads.75 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.75[, prop := length(H)/length(heads.75$H), by = as.factor(H)]
heads.75.sum <-unique(heads.75[,list(H,prop)])
setorder(heads.75.sum,-H)
heads.75.sum[,alpha := cumsum(prop)]
setorder(heads.75.sum,H)
area.75 <- as.data.table(
  bind_rows(old = heads.75.sum,
            new = heads.75.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

heads.50.sum[,coin := 'Prob=50%'] 
heads.75.sum[,coin := 'Prob=75%'] 
heads.sum <- rbind(heads.50.sum,heads.75.sum)
setorderv(heads.sum,c('H','coin'))
power.20 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])

ggplot(heads.sum,aes(x=H,y=prop)) + geom_step(aes(color=coin)) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.75[H>=min(area.50[alpha<.025,H])],
              fill='firebrick',alpha=0.5) + 
  ggtitle(glue("Power {round(power.20, 2)}")) +
  scale_color_manual(name='',values = c('dodgerblue','firebrick')) + 
  xlab('# of Heads') + ylab ('Probability') +
  theme_minimal()

power.20 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])

##### 30 flips #####
p <- 0.5 # this is a fair coin
n <- 30
heads.50 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.50[, prop := length(H)/length(heads.50$H), by = as.factor(H)]
heads.50.sum <-unique(heads.50[,list(H,prop)])
setorder(heads.50.sum,-H)
heads.50.sum[,alpha := cumsum(prop)]
setorder(heads.50.sum,H)
area.50 <- as.data.table(
  bind_rows(old = heads.50.sum,
            new = heads.50.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

p <- 0.75
heads.75 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.75[, prop := length(H)/length(heads.75$H), by = as.factor(H)]
heads.75.sum <-unique(heads.75[,list(H,prop)])
setorder(heads.75.sum,-H)
heads.75.sum[,alpha := cumsum(prop)]
setorder(heads.75.sum,H)
area.75 <- as.data.table(
  bind_rows(old = heads.75.sum,
            new = heads.75.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

heads.50.sum[,coin := 'Prob=50%'] 
heads.75.sum[,coin := 'Prob=75%'] 
heads.sum <- rbind(heads.50.sum,heads.75.sum)
setorderv(heads.sum,c('H','coin'))

ggplot(heads.sum,aes(x=H,y=prop)) + geom_step(aes(color=coin)) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.75[H>=min(area.50[alpha<.025,H])],
              fill='firebrick',alpha=0.5) + 
  scale_color_manual(name='',values = c('dodgerblue','firebrick')) + 
  xlab('# of Heads') + ylab ('Probability') +
  theme_minimal()

power.30 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])



##### 40 flips #####
p <- 0.5 # this is a fair coin
n <- 40
heads.50 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.50[, prop := length(H)/length(heads.50$H), by = as.factor(H)]
heads.50.sum <-unique(heads.50[,list(H,prop)])
setorder(heads.50.sum,-H)
heads.50.sum[,alpha := cumsum(prop)]
setorder(heads.50.sum,H)
area.50 <- as.data.table(
  bind_rows(old = heads.50.sum,
            new = heads.50.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

p <- 0.75
heads.75 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.75[, prop := length(H)/length(heads.75$H), by = as.factor(H)]
heads.75.sum <-unique(heads.75[,list(H,prop)])
setorder(heads.75.sum,-H)
heads.75.sum[,alpha := cumsum(prop)]
setorder(heads.75.sum,H)
area.75 <- as.data.table(
  bind_rows(old = heads.75.sum,
            new = heads.75.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

heads.50.sum[,coin := 'Prob=50%'] 
heads.75.sum[,coin := 'Prob=75%'] 
heads.sum <- rbind(heads.50.sum,heads.75.sum)
setorderv(heads.sum,c('H','coin'))

ggplot(heads.sum,aes(x=H,y=prop)) + geom_step(aes(color=coin)) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.75[H>=min(area.50[alpha<.025,H])],
              fill='firebrick',alpha=0.5) + 
  scale_color_manual(name='',values = c('dodgerblue','firebrick')) + 
  xlab('# of Heads') + ylab ('Probability') +
  theme_minimal()

power.40 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])




##### 100 flips #####
p <- 0.5 # this is a fair coin
n <- 100
heads.50 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.50[, prop := length(H)/length(heads.50$H), by = as.factor(H)]
heads.50.sum <-unique(heads.50[,list(H,prop)])
setorder(heads.50.sum,-H)
heads.50.sum[,alpha := cumsum(prop)]
setorder(heads.50.sum,H)
area.50 <- as.data.table(
  bind_rows(old = heads.50.sum,
            new = heads.50.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

p <- 0.75
heads.75 <- data.table(H = rbinom(n = b, size = n,prob = p))
heads.75[, prop := length(H)/length(heads.75$H), by = as.factor(H)]
heads.75.sum <-unique(heads.75[,list(H,prop)])
setorder(heads.75.sum,-H)
heads.75.sum[,alpha := cumsum(prop)]
setorder(heads.75.sum,H)
area.75 <- as.data.table(
  bind_rows(old = heads.75.sum,
            new = heads.75.sum %>% mutate(prop = lag(prop)),
            .id = "source") %>% arrange(H, source)
)

heads.50.sum[,coin := 'Prob=50%'] 
heads.75.sum[,coin := 'Prob=75%'] 
heads.sum <- rbind(heads.50.sum,heads.75.sum)
setorderv(heads.sum,c('H','coin'))

ggplot(heads.sum,aes(x=H,y=prop)) + geom_step(aes(color=coin)) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.75[H>=min(area.50[alpha<.025,H])],
              fill='firebrick',alpha=0.5) + 
  scale_color_manual(name='',values = c('dodgerblue','firebrick')) + 
  xlab('# of Heads') + ylab ('Probability') +
  theme_minimal()

power.100 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])


##### Normal Distribution ####
theme_min <- theme_minimal(base_size = 16) + theme(legend.position="bottom")
  
ggplot(NULL, aes(x = c(-3, 3))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null Distribution'),
                xlim = c(-10, 10),
                show.legend = T) +
  stat_function(fun = dnorm,
                geom = "area",
                aes(fill = "(False) Rejection probability"), color = NA,
                alpha = 0.5,
                xlim = c(-5, -1.96)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "dodgerblue", alpha = 0.5,
                xlim = c(1.96, 5)) +
  annotate(geom="text", x=-2.75, y=.05, label=expression(alpha/2), color="dodgerblue") + 
  annotate(geom="text", x=2.75, y=.05, label=expression(alpha/2), color="dodgerblue") + 
  scale_color_manual(name='', values = 'dodgerblue') +
  scale_fill_manual(name='', values = 'dodgerblue') +
  geom_vline(xintercept = -1.96) + geom_vline(xintercept = 1.96) + 
  scale_x_continuous(name ="",limits=c(-4,4), breaks = c(-1.96,0,1.96)) + 
  theme_min

ggplot(NULL, aes(x = c(-4, 4))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                args = list(mean=1,sd=1),
                geom = "line",
                aes(color = '1 SD Effect'),
                xlim = c(-10, 10)) +
  geom_vline(xintercept = 0, color = 'dodgerblue') + 
  geom_vline(xintercept = 1, color = 'firebrick') + 
  scale_color_manual(name='', values = c('Null'='dodgerblue','1 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-4,4), breaks = c(-1.96,0,1.96)) + 
  theme_min



ggplot(NULL, aes(x = c(-4, 4))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                args = list(mean=1,sd=1),
                geom = "line",
                aes(color = '1 SD Effect'),
                xlim = c(-10, 10)) +
  geom_vline(xintercept = 0, color = 'dodgerblue') + 
  geom_vline(xintercept = 1, color = 'firebrick') + 
  scale_color_manual(name='', values = c('Null'='dodgerblue','1 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-4,4), breaks = c(0,1)) + 
  theme_min



ggplot(NULL, aes(x = c(-4, 4))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                args = list(mean=1,sd=1),
                geom = "line",
                aes(color = '1 SD Effect'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'dodgerblue',alpha = 0.5,
                xlim = c(1.96, 5)) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean=1,sd=1),
                fill = 'firebrick',alpha = 0.5,
                xlim = c(1.96, 5)) +
  scale_color_manual(name='', values = c('Null'='dodgerblue','1 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-5,5), breaks = c(0,1,1.96)) + 
  theme_min




ggplot(NULL, aes(x = c(-6, 6))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                args = list(mean=3,sd=1),
                geom = "line",
                aes(color = '3 SD Effect'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'dodgerblue',alpha = 0.5,
                xlim = c(1.96, 10)) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean=3,sd=1),
                fill = 'firebrick',alpha = 0.5,
                xlim = c(1.96, 10)) +
  scale_color_manual(name='', values = c('Null'='dodgerblue','3 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-4,7), breaks = c(0,1,1.96)) + 
  theme_min



ggplot(NULL, aes(x = c(-6, 6))) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                args = list(mean=3,sd=1),
                geom = "line",
                aes(color = '3 SD Effect'),
                xlim = c(-10, 10)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = 'dodgerblue',alpha = 0.5,
                xlim = c(-1.96, 1.96)) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean=3,sd=1),
                fill = 'firebrick',alpha = 0.5,
                xlim = c(1.96, 10)) +
  geom_vline(xintercept = 0, linetype=2) + 
  geom_vline(xintercept = 1.96, linetype=2) + 
  geom_vline(xintercept = 3, linetype=2) + 
  geom_segment(aes(x=1.96,xend=3,y=.3,yend=.3)) + 
  geom_segment(aes(x=0,xend=1.96,y=.1,yend=.1)) + 
  annotate(geom="text", x=2.5, y=.33, label=expression(t[1-kappa]), color="black") + 
  annotate(geom="text", x=.8, y=.115, label=expression(t[1-alpha/2]), color="black") + 
  annotate(geom="text", x=6, y=.2, label=expression('Area:'~kappa), color="firebrick") + 
  annotate(geom="text", x=6, y=.18, label='Power', color="firebrick") + 
  annotate(geom="text", x=-2.5, y=.2, label=expression('Area:'~1-alpha), color="dodgerblue") + 
  annotate(geom="text", x=-2.5, y=.18, label='Size', color="dodgerblue") + 
  scale_color_manual(name='', values = c('Null'='dodgerblue','3 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-4,7), breaks = c(0,1,1.96)) + 
  theme_min


