
library(pacman)
p_load(
  broom, tidyverse,
  ggplot2, ggthemes, ggforce, ggridges, scales, wesanderson,
  latex2exp, viridis, extrafont, gridExtra,
  kableExtra, snakecase, janitor,xtable,
  data.table, dplyr,
  lubridate, knitr, future, furrr,
  estimatr, huxtable, here, magrittr
)
# Define pink color
blue <- "#0073e6"
turquoise <- "#20B2AA"
orange <- "#FFA500"
red <- "#fb6107"
green <- "#006400"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#006400"
slate <- "#314f4f"
# Dark slate grey: #314f4f
# Knitr options

# Column names for regression results
reg_columns <- c("Term", "Est.", "S.E.", "t stat.", "p-Value")
# Function for formatting p values
format_pvi <- function(pv) {
  return(ifelse(
    pv < 0.0001,
    "<0.0001",
    round(pv, 4) %>% format(scientific = F)
  ))
}
format_pv <- function(pvs) lapply(X = pvs, FUN = format_pvi) %>% unlist()
# Tidy regression results table
tidy_table <- function(x, terms, highlight_row = 1, highlight_color = "black", highlight_bold = T, digits = c(NA, 3, 3, 2, 5), title = NULL) {
  x %>%
    tidy() %>%
    select(1:5) %>%
    mutate(
      term = terms,
      p.value = p.value %>% format_pv()
    ) %>%
    kable(
      col.names = reg_columns,
      escape = F,
      digits = digits,
      caption = title
    ) %>%
    kable_styling(font_size = 20) %>%
    row_spec(1:nrow(tidy(x)), background = "white") %>%
    row_spec(highlight_row, bold = highlight_bold, color = highlight_color)
}

##### chunk 2
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
ggsave('inputs/20_flips_true.png')

##### chunk 3
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
    xlab('# of Heads') + ylab ('Probability') +
theme_minimal(base_size = 16)
ggsave('inputs/20_flips_obs_50.png')

##### chunk 4
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

ggplot(heads.sum,aes(x=H,y=prop)) + geom_step(aes(color=coin)) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.50[alpha<.025],
              fill='dodgerblue',alpha=0.5) + 
  geom_ribbon(aes(x = H, ymin = 0, ymax = prop), data = area.75[H>=min(area.50[alpha<.025,H])],
              fill='firebrick',alpha=0.5) + 
  scale_color_manual(name='',values = c('dodgerblue','firebrick')) + 
  xlab('# of Heads') + ylab ('Probability') +
  theme_minimal(base_size = 16)
ggsave('inputs/20_flips_obs_50_v_75.png')

power.20 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])


##### chunk 5
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
ggsave('inputs/30_flips_obs_50_v_75.png')

power.30 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])

##### chunk 6
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
ggsave('inputs/40_flips_obs_50_v_75.png')

power.40 <- sum(heads.75.sum[H>=min(area.50[alpha<.025,H]),prop])

##### chunk 7
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
ggsave('inputs/100_flips_obs_50_v_75.png')

##### chunk 8
theme_min <- theme_minimal(base_size = 16) + theme(legend.position="bottom")

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm,
                geom = "line",
                aes(color = 'Null Distribution'),
                xlim = c(-10, 10),
                show.legend = T) +
  stat_function(fun = dnorm,
                geom = "area",
                aes(fill = "(False) Rejection probability"), 
                color = NA,
                alpha = 0.5,
                xlim = c(-5, -1.96)) +
  stat_function(fun = dnorm,
                geom = "area",
                aes(fill = "(False) Rejection probability"), 
                alpha = 0.5,
                xlim = c(1.96, 5)) +
  annotate(geom="text", x=-2.75, y=.05, label=expression(alpha/2), color="dodgerblue") + 
  annotate(geom="text", x=2.75, y=.05, label=expression(alpha/2), color="dodgerblue") + 
  scale_color_manual(name='', values = 'dodgerblue') +
  scale_fill_manual(name='', values = 'dodgerblue') +
  geom_vline(xintercept = -1.96) + geom_vline(xintercept = 1.96) + 
  scale_x_continuous(name ="",limits=c(-4,4), breaks = c(-1.96,0,1.96)) + 
  theme_min

##### chunk 9
ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
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

##### chunk 10
ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
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
  # stat_function(fun = dnorm,
  #               geom = "area",
  #               args = list(mean=1,sd=1),
  #               fill = 'firebrick',alpha = 0.5,
  #               xlim = c(1.96, 5)) +
  scale_color_manual(name='', values = c('Null'='dodgerblue','1 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-5,5), breaks = c(0,1,1.96)) + 
  theme_min

##### chunk 11
ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
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

##### chunk 12
ggplot(data = data.frame(x = c(-6, 6)), aes(x)) +
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

##### chunk 13
ggplot(data = data.frame(x = c(-6, 6)), aes(x)) +
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
  annotate(geom="text", x=2.5, y=.33, label=expression(t[1-kappa]), color="black",size=8) + 
  annotate(geom="text", x=.8, y=.115, label=expression(t[1-alpha/2]), color="black",size=8) + 
  annotate(geom="text", x=6, y=.2, label=expression('Area:'~kappa), color="firebrick",size=8) + 
  annotate(geom="text", x=6, y=.18, label='Power', color="firebrick",size=8) + 
  annotate(geom="text", x=-2.5, y=.2, label=expression('Area:'~1-alpha), color="dodgerblue",size=8) + 
  annotate(geom="text", x=-2.5, y=.18, label='Size', color="dodgerblue",size=8) + 
  scale_color_manual(name='', values = c('Null'='dodgerblue','3 SD Effect'='firebrick'),) +
  scale_x_continuous(name ="",limits=c(-4,7), breaks = c(0,1,1.96)) + 
  theme_min

##### chunk 14
# Generate the dataset
set.seed(123)
n = 9
z = tibble(x = 1:n, y = 1 + x + rnorm(n, sd = 5))
b = lm(y ~ x, data = z)$coefficient[2]
boot_colors <- wes_palette("Zissou1", n, type = "continuous")
# boot_colors <- magma(n, begin = 0.1, end = 0.93)
s = 1:n
base_df <- expand.grid(x = 1:sqrt(n), y = 1:sqrt(n)) %>% as_tibble()
# Bootstrap 1
s1 <- sample(1:n, n, replace = T)
z1 <- z[s1,]
b1 <- lm(y ~ x, data = z1)$coefficient[2]
# Bootstrap 2
s2 <- sample(1:n, n, replace = T)
z2 <- z[s2,]
b2 <- lm(y ~ x, data = z2)$coefficient[2]
# Bootstrap 3
s3 <- sample(1:n, n, replace = T)
z3 <- z[s3,]
b3 <- lm(y ~ x, data = z3)$coefficient[2]
# Bootstrap 4
s4 <- sample(1:n, n, replace = T)
z4 <- z[s4,]
b4 <- lm(y ~ x, data = z4)$coefficient[2]

##### chunk 15
# Graph individuals
ggplot(
  data = base_df %>% mutate(fill = 1:n, lab = s),
  aes(x, y, fill = as.factor(fill))
) +
geom_tile(color = "white", size = 1.5) +
geom_text(aes(label = lab), color = "white", size = 20) +
coord_equal() +
scale_fill_manual(values = boot_colors[s]) +
scale_color_manual(values = boot_colors[s]) +
theme_void() +
theme(legend.position = "none")
```

$$\hat\beta = `r b %>% round(3)`$$

```{R, g2-boot0, echo = F, out.width = '100%'}
# Graph individuals
ggplot(
  data = z %>% mutate(s = 1:n),
  aes(x, y, color = as.factor(s))
) +
geom_smooth(method = lm, se = F, color = "grey85", size = 5) +
geom_point(size = 20, alpha = 0.5) +
coord_equal() +
xlim(-0.5,n+0.5) +
scale_color_manual(values = boot_colors[s]) +
theme_void() +
theme(legend.position = "none")

##### chunk 16
# Graph individuals
ggplot(
  data = base_df %>% mutate(fill = 1:n, lab = s1),
  aes(x, y, fill = as.factor(fill))
) +
geom_tile(color = "white", size = 1.5) +
geom_text(aes(label = lab), color = "white", size = 20) +
coord_equal() +
scale_fill_manual(values = boot_colors[s1]) +
scale_color_manual(values = boot_colors[s1]) +
theme_void() +
theme(legend.position = "none")

##### chunk 17
# Graph individuals
ggplot(
  data = z1 %>% mutate(s = 1:n),
  aes(x, y, color = as.factor(s))
) +
geom_smooth(method = lm, se = F, color = "grey85", size = 5) +
geom_point(size = 20, alpha = 0.5) +
coord_equal() +
xlim(-0.5,n+0.5) +
scale_color_manual(values = boot_colors[s1]) +
theme_void() +
theme(legend.position = "none")

##### chunk 18
ggplot(
  data = base_df %>% mutate(fill = 1:n, lab = s2),
  aes(x, y, fill = as.factor(fill))
) +
geom_tile(color = "white", size = 1.5) +
geom_text(aes(label = lab), color = "white", size = 20) +
coord_equal() +
scale_fill_manual(values = boot_colors[s2]) +
scale_color_manual(values = boot_colors[s2]) +
theme_void() +
theme(legend.position = "none")

##### chunk 19
ggplot(
  data = z2 %>% mutate(s = 1:n),
  aes(x, y, color = as.factor(s))
) +
geom_smooth(method = lm, se = F, color = "grey85", size = 5) +
geom_point(size = 20, alpha = 0.5) +
coord_equal() +
xlim(-0.5,n+0.5) +
scale_color_manual(values = boot_colors[s2]) +
theme_void() +
theme(legend.position = "none")

##### chunk 20
# Graph individuals
ggplot(
  data = base_df %>% mutate(fill = 1:n, lab = s3),
  aes(x, y, fill = as.factor(fill))
) +
geom_tile(color = "white", size = 1.5) +
geom_text(aes(label = lab), color = "white", size = 20) +
coord_equal() +
scale_fill_manual(values = boot_colors[s3]) +
scale_color_manual(values = boot_colors[s3]) +
theme_void() +
theme(legend.position = "none")

##### chunk 21
# Graph individuals
ggplot(
  data = z3 %>% mutate(s = 1:n),
  aes(x, y, color = as.factor(s))
) +
geom_smooth(method = lm, se = F, color = "grey85", size = 5) +
geom_point(size = 20, alpha = 0.5) +
coord_equal() +
xlim(-0.5,n+0.5) +
scale_color_manual(values = boot_colors[s3]) +
theme_void() +
theme(legend.position = "none")

##### chunk 22
plan(multiprocess, workers = 10)
# Set a seed
set.seed(123)
# Run the simulation 1000 times
boot_df <- future_map_dfr(
  # Repeat sample size 100 for 1000 times
  rep(n, 1000),
  # Our function
  function(n) {
    # Estimates via bootstrap
    est <- lm(y ~ x, data = z[sample(1:n, n, replace = T), ])
    # Return a tibble
    data.frame(int = est$coefficients[1], coef = est$coefficients[2])
  },
  # Let furrr know we want to set a seed
  .options = future_options(seed = T)
)

##### chunk 23
ggplot(
  data = z,
  aes(x, y, fill = as.factor(1:n))
) +
geom_abline(
  data = boot_df,
  aes(intercept = int, slope = coef),
  color = "grey50",
  alpha = 0.01
) +
geom_abline(
  intercept = lm(y ~ x, z)$coefficient[1],
  slope = lm(y ~ x, z)$coefficient[2],
  color = "black",
  size = 1.25
) +
geom_point(
  size = 10,
  stroke = 0.75,
  color = "white",
  shape = 21
) +
# coord_equal() +
# xlim(-0.5,n+0.5) +
scale_fill_manual(values = boot_colors[s]) +
theme_void() +
theme(legend.position = "none")

##### chunk 24
ggplot(
  data = tibble(x = 2 * (0:4), n = c(1, 16, 36, 16, 1)),
  aes(x = x, y = n)
) +
geom_col() +
geom_hline(yintercept = 0, size = 2) +
scale_x_continuous(
  "N. correct",
  breaks = 2 * (0:4)
) +
scale_y_continuous("Count") +
theme_pander(base_size = 65, base_family = "Fira Sans Book") +
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

##### chunk 25
# Arguments: 'i' (iteration), 'n_t' (# of trt)
fun_randomization <- function(i) {
  # Sample the treatment vector. NOTE: Sampling WITHOUT replacement
  t_i <- sample(nsw_df$treat, size = nrow(nsw_df), replace = F)
  # Regression using our re-randomized treatment
  est_i <- lm_robust(re78 ~ t_i, data = nsw_df) %>% tidy()
  # Return tibble with iteration, point estimate, and test statistic
  tibble(i, est = est_i[2,"estimate"], t_stat = est_i[2,"statistic"])
}
##### chunk 26
# Set up parallelization and seed
plan(multiprocess, workers = 4); set.seed(1234)
# Run the simulation 1e4 times
random_df <- future_map_dfr(
  1:1000,
  fun_randomization,
  .options = future_options(seed = T)
)

##### chunk 27
# Calculate density
gg_df <- density(random_df$est, n = 1e3, kernel = "epanechnikov") %$% tibble(
  est = x,
  density = y,
  reject = abs(est) > est_ols[2,"estimate"]
)
ggplot(
  data = gg_df,
  aes(x = est, ymin = 0, ymax = density)
) +
geom_ribbon(fill = "grey85", alpha = 0.8) +
geom_ribbon(
  data = gg_df %>% filter(est > abs(est_ols[2,"estimate"])),
  fill = green
) +
geom_ribbon(
  data = gg_df %>% filter(est < -abs(est_ols[2,"estimate"])),
  fill = green
) +
geom_hline(yintercept = 0) +
geom_vline(
  xintercept = est_ols[2,"estimate"],
  color = blue, size = 1, linetype = "longdash"
) +
xlab(TeX("$\\widehat{\\beta}_{1}^\\textit{r}$")) +
theme_pander(base_size = 18, base_family = "Fira Sans Book") +
theme(
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_text(family = "TeX Gyre Termes", size = 20),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

##### chunk 28
# Calculate density
gg_df <- density(random_df$t_stat, n = 1e3, kernel = "epanechnikov") %$% tibble(
  stat = x,
  density = y,
  reject = abs(stat) > est_ols[2,"statistic"]
)
ggplot(
  data = gg_df,
  aes(x = stat, ymin = 0, ymax = density)
) +
geom_ribbon(fill = "grey85", alpha = 0.8) +
geom_ribbon(
  data = gg_df %>% filter(stat > abs(est_ols[2,"statistic"])),
  fill = green
) +
geom_ribbon(
  data = gg_df %>% filter(stat < -abs(est_ols[2,"statistic"])),
  fill = green
) +
geom_hline(yintercept = 0) +
geom_vline(
  xintercept = est_ols[2,"statistic"],
  color = blue, size = 1, linetype = "longdash"
) +
xlab(TeX("\\textit{t}$_{stat}^\\textit{r}$")) +
theme_pander(base_size = 18, base_family = "Fira Sans Book") +
theme(
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_text(family = "TeX Gyre Termes", size = 20),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

##### chunk 29
# Arguments: 'null', the null values
fun_invert_i <- function(null) {
  # Sample the treatment vector. NOTE: Sampling WITHOUT replacement
  t_i <- sample(nsw_df$treat, size = nrow(nsw_df), replace = F)
  map_dfr(
    null,
    function(null_j) {
      # Impose the null (generate outcomes)
      y_ij <- nsw_df$re78 + t_i * null_j
      # Regression using our re-randomized treatment
      est_ij <- lm_robust(y_ij ~ t_i) %>% tidy()
      # Return tibble with point estimate, se, test statistic, and null
      tibble(
        est = est_ij[2,"estimate"],
        se = est_ij[2, "std.error"],
        t_stat = est_ij[2,"statistic"],
        null = null_j
      )
    }
  ) %>% mutate(null_group = unlist(null) %>% seq_along())
}
# Function to run the function a bunch of times
fun_invert <- function(null, times = 1e3) {
  plan(multiprocess, workers = 4)
  future_map_dfr(
    rep(null, times),
    fun_invert_i,
    .options = future_options(seed = T)
  )
}


##### chunk 30
ci_df <- fun_invert(
  # null = list(quantile(random_df$est, seq(0, 1, 0.01))),
  null = list(seq(from = est_ols[2,"conf.low"], to = est_ols[2,"conf.high"], length.out = 100)),
  times = 1000
)


##### chunk 31
# Add groups and test
ci_sum <- ci_df %>% mutate(
  reject = 2 * pt(abs((est - est_ols[2,"estimate"])/se), df = 720, lower.tail = F) < 0.05
)
# Summarize
ci_sum %<>% group_by(null_group) %>%
  summarize(
    reject = mean(reject),
    null = first(null)
  )

##### chunk 32
ggplot(
  data = ci_sum,
  aes(x = null, y = reject, color = reject > 0.1)
) +
geom_hline(yintercept = 0.10, linetype = "dotted") +
geom_hline(yintercept = 0) +
geom_point(size = 2.5) +
xlab(TeX("$\\widehat{\\beta}_{1}^\\textit{o}$")) +
ylab("Share rejecting original estimate") +
scale_y_continuous(breaks = seq(0, 0.5, 0.1)) +
scale_color_manual("", values = c(green, "grey85")) +
theme_pander(base_size = 18, base_family = "Fira Sans Book") +
theme(
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.title.x = element_text(family = "TeX Gyre Termes", size = 20, angle = 0, vjust = 0.5),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

