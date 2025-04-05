#########################################
# LAB 10:
#########################################
# load libraries
#########################################
library(tidyverse)
#########################################
# TASK 1
#########################################
samp.size <- 1004

sample <- rbinom(n = 10000, size = samp.size, prob = 0.39)
sims <- tibble(poll = sample)

sample.plot <- ggplot(data = sims) +
  geom_histogram(aes(x = poll, y = after_stat(density))) +
  geom_density(aes(x = poll))

#####################
# testing 20000
sample_2 <- rbinom(n = 20000, size = 1004, prob = 0.39)
sims_20k <- tibble(poll = sample_2)

sample.plot2 <- ggplot(data = sims_20k) +
  geom_histogram(aes(x = poll, y = after_stat(density))) +
  geom_density(aes(x = poll)) 

#############################
# TASK 2
#############################
gallup.data <- tibble(poll = c(rep(1,(round(0.39*1004))), 
                               rep(0,(round(0.59*1004))), 
                               rep(NA,(round(0.02*1004)))))


resample <- sample(x = gallup.data$poll,
                   size = 1004,
                   replace = T)

resample.plot <- ggplot(data = data.frame(resample)) +
  geom_histogram(aes(x = data, y = after_stat(density)))

########################################
# TASK 3
########################################

N <- seq(from = 100, to = 300, by = 10)
P <- seq(from = 0.01, to = 0.99, by = 0.01)

simulations <- tibble(half.of.range = numeric(length(N) * length(P)))
i = 1
for (n in N){
  for (p in P){
    curr.sim <- rbinom(n = 10000, size = n, prob = p)
    percentile_2.5 <- quantile(curr.sim, 0.025)
    percentile_97.5 <- quantile(curr.sim, 0.975)
    num <- (percentile_97.5 - percentile_2.5) / 2
    simulations$half.of.range[i] = num
    i = i + 1
  }
}

result.p <- ggplot(data = simulations) +
  geom_raster(aes(x = half.of.range, y = after_stat(density)))

