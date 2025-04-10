#########################################
# Michael Boateng
# LAB 10
#########################################
# load libraries
#########################################
library(tidyverse)
library(patchwork)
#########################################
# TASK 1
#########################################
samp.size <- 1004

sample <- rbinom(n = 10000, size = samp.size, prob = 0.39)
sims <- tibble(poll = sample/1004)

percentile_2.5 <- quantile(sims$poll, 0.025)
percentile_97.5 <- quantile(sims$poll, 0.975)
moe1 <- (percentile_97.5 - percentile_2.5) / 2

sample.plot <- ggplot(data = sims) +
  geom_histogram(aes(x = poll, y = after_stat(density)),
                 breaks = seq(0.32, 0.48, 0.005),
                 color="grey") +
  geom_density(aes(x = poll)) +
  geom_hline(yintercept = 0) +
  labs(x = "Probability", y = "Frequency", title = "Poll sample") +
  theme_bw() 

percentile_2.5 <- quantile(sims$poll, 0.025)
percentile_97.5 <- quantile(sims$poll, 0.975)
moe <- (percentile_97.5 - percentile_2.5) / 2

#####################
# testing twice the sample size
sample_2 <- rbinom(n = 10000, size = samp.size * 2, prob = 0.39)
sims_2 <- tibble(poll = sample_2/2008)

sample.plot2 <- ggplot(data = sims_2) +
  geom_histogram(aes(x = poll, y = after_stat(density)),
                 breaks = seq(0.34, 0.44, 0.005),
                 color = "grey") +
  geom_density(aes(x = poll)) +
  geom_hline(yintercept = 0) +
  labs(x = "Probability", y = "Frequency", title = "Poll sample") +
  theme_bw() 

percentile_2.5 <- quantile(sims_2$poll, 0.025)
percentile_97.5 <- quantile(sims_2$poll, 0.975)
moe2 <- (percentile_97.5 - percentile_2.5) / 2


two.samples = sample.plot + sample.plot2

##########################################
# TASK 2
##########################################                  # creating the Gallup data
gallup.data <- tibble(poll = c(rep(1,(round(0.39*1004))),   # where 1 = 'satisfied'
                               rep(0,(round(0.59*1004))),   # 0 = 'dissatisfied'
                               rep(NA,(round(0.02*1004))))) # NA = 'no opinion'

R <- 10000
gallup.resamples <- tibble(p.hat = numeric(R))
for (i in 1:R){
  resample <- sample(x = gallup.data$poll,
                     size = 1004,
                     replace = T)
  gallup.resamples$p.hat[i] = mean(resample, na.rm = T)
}


(resample.plot <- ggplot(data = gallup.resamples) +
  geom_histogram(aes(x = p.hat, y = after_stat(density)),
                 breaks = seq(0.325, 0.475, 0.005), 
                 color = "grey") +
  geom_density(aes(x = p.hat)) +
  geom_hline(yintercept = 0) +
  labs(x = "Probability", y = "Density", title = "Resampling") +
  theme_bw()
  )

percentile_2.5 <- quantile(gallup.resamples$p.hat, 0.025)
percentile_97.5 <- quantile(gallup.resamples$p.hat, 0.975)
moe3 <- (percentile_97.5 - percentile_2.5) / 2

##########################################
# TASK 3
##########################################
N <- seq(from = 100, to = 3000, by = 10)
P <- seq(from = 0.01, to = 0.99, by = 0.01)

simulations <- tibble(
  n = numeric(),
  p = numeric(),
  margin.of.error = numeric())

for (i in N){
  for (j in P){
    curr.sim <- rbinom(n = 10000, size = i, prob = j) / i 
    percentile_2.5 <- quantile(curr.sim, 0.025)
    percentile_97.5 <- quantile(curr.sim, 0.975)
    num <- (percentile_97.5 - percentile_2.5) / 2
    simulations <- bind_rows(simulations,
                             tibble(n = i, p = j, margin.of.error = num))
  }
}

result.p <- ggplot(data = simulations) +
  geom_raster(aes(x = n, y = p, fill = margin.of.error)) +
  scale_fill_viridis_c(option = "inferno") +
  geom_hline(yintercept = 0) +
  labs(x = "Sample size", 
       y = "Percent satisfied", 
       title = "Estimated margin of error")

###########################################
# TASK 4
###########################################
N <- seq(100, 2000, by = 10)
P <- seq(0.01, 0.99, by=0.01)
z <- qnorm(0.975)  

wilson.results <- tibble(n = numeric(), 
                         p = numeric(), 
                         margin.of.error = numeric())

for (n in N) {
  for (p in P) {
    error <- z * (sqrt((n * p * (1-p)) + (z^2 / 4))) / (n + z^2)
    
    wilson.results <- bind_rows(wilson.results,
                                tibble(n = n, p = p, margin.of.error = error))
  }
}

wilson.plot <- ggplot(data = wilson.results) +
  geom_raster(aes(x = n, y = p, fill = margin.of.error)) +
  geom_hline(yintercept = 0) +
  scale_fill_viridis_c(option = "inferno") +
  labs(x = "Sample size", y = "Percent Satisfied", title = "Wilson MOE") +
  theme_bw()
  