library(tidyverse)
library(patchwork)

shape1 <- 1.3
shape2 <- 1.3

logit <- function(x){
  log(x/(1-x))
}

beta_dat <- 
  tibble(x = seq(0.01,0.99, 0.01),
         p = dbeta(seq(0.01,0.99, 0.01), shape1, shape2)
         )
   
logit_dat <-
  tibble(
    logit = logit(rbeta(n = 4000, shape1 = shape1, shape2 = shape2)),
  )

beta <- 
  ggplot()+
  stat_function(fun = dbeta, n=101, args = list(shape1 = shape1, shape2 = shape2))

log <- 
  ggplot(logit_dat, aes(x = logit))+
  geom_density()

log_prior <- MASS::fitdistr(logit_dat$logit, dnorm, start = list(mean = 0, sd = 1))

prior_dist <- 
  ggplot()+
  stat_function(fun = dnorm,
                n = 101, 
                args = list(mean = log_prior$estimate[[1]], sd = log_prior$estimate[[2]]),
                xlim = c(-5, 5),
                color = 'red'
                )

cat(paste0("mean = ", log_prior$estimate[[1]], '\n',
           "sd = ", log_prior$estimate[[2]]))


beta / log / prior_dist









