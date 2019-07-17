#### To do: 
#  Double check sample_post functions with old

# Environment preparation -------------------------------------------------
rm(list = ls())

# Packages
## Data handling
require(osfr)
require(tidyverse) #should have also readxl

## Other stats
require(pwr)

## Graphs
require(ggplot2)
require(ggpubr)
require(viridis)
require(grid)
library(gtable)


# Graphics ----------------------------------------------------------------
my_theme <- theme_classic() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t = 20, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(hjust = 0.95),
        axis.title.y = element_text(hjust = 0.95),
        axis.line.x = element_line(arrow = arrow(ends = "last", length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(ends = "last", length = unit(0.1, "inches"))),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 


std_fill <- scale_fill_viridis(discrete = TRUE, alpha = 0.2)
std_fill_dark <- scale_fill_viridis(discrete = TRUE)
my_purple <- viridis(5, alpha = 0.8)[1]
my_watergreen <- viridis(5, alpha = 0.8)[3]
my_yellow <- viridis(5, alpha = 0.8)[5]
my_purple_light <- viridis(5, alpha = 0.01)[1]
my_watergreen_light <- viridis(5, alpha = 0.2)[3]
my_yellow_light <- viridis(5, alpha = 0.2)[5]
my_bad <- "#f0d9d0"
my_good <- "#d0f0da"
# colours from package viridis
# organization plots with ggpubr


gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}
gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}


# Power calculation-related -----------------------------------------------
# outputs n smallest group (control)
whats_nC <- function(delta, sd_ratio, n_ratio = 1, alt = "two.sided") {
  
  n_nec <- MESS::power_t_test(power = 0.8,
                              delta = delta,
                              ratio = n_ratio,
                              sd = 1,
                              sd.ratio = sd_ratio,
                              alternative = alt,
                              sig.level = .05,
                              df.method = "welch")
  n_nec$n[1]
  
}

# Bayesian analysis ----------------------------------------------------------------
## Prior parmeters
find_prior_par <- function(pilot_name = 01, n_pilot = 0, mean_pilot = 0, s2_pilot = 0, 
                           belief = 1, n_sampled = 10000) {
  
  # Prior: parameters
  n0_cor <- n_pilot * belief
  mu0 <- 0 + mean_pilot
  k0 <- 0 + n0_cor
  v0 <- 0 + n0_cor
  
  if (n_pilot > 0) {
    sigma0_2 <- 0 + (n0_cor - 1)/n0_cor * s2_pilot
  } else {
    sigma0_2 <- 0
  }
  
  # Organize
  #  experiment <- as.character(pilot_name)
  prior_par <- as.data.frame(cbind(mu0, k0, v0, sigma0_2, n0_cor))
  
  prior_par
  
}


## Posterior parameters
find_post_par <- function(n_group, mean_group, s2_group, belief_group = 1,
                          mu0 = 0, k0 = 0, v0 = 0, sigma0_2 = 0, n0_cor = 0) {
  
  # Posterior: parameters 
  n_group_cor <- n_group * belief_group
  
  mu1 <- ((k0/(k0 + n_group_cor)) * mu0) + 
    ((n_group_cor/(k0 + n_group_cor)) * mean_group)
  k1 <- k0 + n_group_cor
  v1 <- v0 + n_group_cor
  
  sigma1_2 <- 
    (
    
      v0*sigma0_2 + 
      (n_group_cor - 1) * s2_group + 
      ((k0 * n_group_cor/(k0 + n_group_cor)) * ((mean_group - mu0)^2))
      
    ) / v1
  
  n1_cor <- n0_cor + n_group_cor
  
  # Organize
  paramenters <- as.data.frame(cbind(mu1, k1, v1, sigma1_2, n1_cor))
  
  return(paramenters)
  
}


## Sampling
sample_post_cor <- function(n_group, mean_group, s2_group, belief = 1, n_sampled = 10000, 
                            mu0 = 0, k0 = 0, v0 = 0, sigma0_2 = 0) {
  
  # Posterior: parameters 
  mu1 <- ((k0/(k0 + n_group)) * mu0) + ((n_group/(k0 + n_group)) * mean_group)
  k1 <- k0 + n_group
  v1 <- v0 + n_group
  
  sigma1_2 <- (v0*sigma0_2 + (n_group - 1)*s2_group + 
                 ((k0*n_group/(k0+n_group)) * ((mean_group - mu0)^2))) / v1
  
  # Sample from posterior
  
  sigma_post <- asbio::rinvchisq(n = n_sampled, # amount values to draw 
                                 df = v1,  
                                 scale = sigma1_2)
  
  mu_post    <- rep(NA, n_sampled)
  
  for (i in 1:n_sampled) {   
    
    mu_post[i]    <- rnorm(n = 1, 
                           mean = mu1, 
                           sd = sqrt(sigma_post[i] / k1)) 
    
  } 
  
  return(mu_post)
}


