## The How Does RePAIR work file

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")


# N for power 0.8 ----------------------------------------------------
# dataset - all combinations
dat <- expand.grid(
  "hedges"      = c(.2, .5, .9),
  "sd_ec_ratio" = c(1, 2),
  "n_prior"     = c(0, 10, 20, 50, 100, 200)
) 

dat$n_e <- pmap_dbl(list(dat$hedges, dat$sd_ec_ratio), whats_nC, n_ratio = 1)
dat$n_c <- dat$n_e - dat$n_prior
dat[dat$n_c <= 1,]$n_c <- 2 # min n_c must be 2
dat$n_tot <- dat$n_c + dat$n_e

# Bayesian power -------------------------------------------------------
# Parameters chosen to keep power at least at 80%.

# Parameters population
dat$p_mean_c <- 0
dat$p_sd_c <- 1
dat$p_sd_e <- dat$p_sd_c * dat$sd_ec_ratio
dat$p_sd_pooled <- sqrt(((dat$p_sd_e)^2 + (dat$p_sd_c)^2)/2) ## explanation from formula paper
dat$p_mean_e <- dat$p_mean_c + (dat$p_sd_pooled * dat$hedges) # from hedges G definition

# Parameters experiment
dat$mean_c <- NA
dat$mean_e <- NA
dat$s2_c <- NA
dat$s2_e <- NA


#simulation
dat$bae_pow <- NA
n_sim <- 10000
for (i in c(1:nrow(dat))) {
  print(i)
  
  # Create variable for output
  contains_zero <- FALSE #how often does the 95%CI contain zero? expected 20%
  
  # Simulation loop
  for (sim in c(1:n_sim)) {
    # From population to sample (from N distribution)
    ## Control
    d_c <- rnorm(n = dat[i,]$n_c, mean = dat[i,]$p_mean_c, sd = dat[i,]$p_sd_c)
    dat[i,]$mean_c <- mean(d_c)
    dat[i,]$s2_c <- var(d_c)
    
    ## Experimental
    d_e <- rnorm(n = dat[i,]$n_e, mean = dat[i,]$p_mean_e, sd = dat[i,]$p_sd_e)
    dat[i,]$mean_e <- mean(d_e)
    dat[i,]$s2_e <- var(d_e)
    
    
    # Sampling from posterior
    ## Control
    par_c <- find_prior_par(n_pilot = dat[i,]$n_prior, 
                            mean_pilot = 0, s2_pilot = 1) # prior parameters
    
    mu_post_c <- sample_post_cor(n_group = dat[i,]$n_c, mean_group = dat[i,]$mean_c,
                                 s2_group = dat[i,]$s2_c,  
                                 mu0 = par_c$mu0, 
                                 k0 = par_c$k0, v0 = par_c$v0, 
                                 sigma0_2 = par_c$sigma0_2) 
    
    
    mu_post_e <- sample_post_cor(n_group = dat[i,]$n_e, mean_group = dat[i,]$mean_e,
                             s2_group = dat[i,]$s2_e)
    
    
    # Credibility interval
    cred_int <- mu_post_e - mu_post_c
    
    # Power
    
    quantile(cred_int, probs = c(.025, .975))
    #print(quantile(cred_int, probs = c(.025, .975)))
    contains_zero[sim] <- 0 >= quantile(cred_int, probs = .025) &
      0 <= quantile(cred_int, probs = .975)
    
  }
  
  dat[i,]$bae_pow <- n_sim - sum(contains_zero)
  
}

#write.csv(dat, file = "sim_prior.csv")
dat <- read.csv("sim_prior.csv")

# Visualization RePAIR ----------------------------------------------------
#dat <- read.csv("/Users/vbonape2/surfdrive/Work/PhD/nStat/n_stat_git/sim_prior_power.csv")
dat$my_cat <- ifelse(dat$n_prior == 0, "No prior", "With RePAIR")

top_left <- 
  ggplot(
    dat[dat$sd_ec_ratio == 1 & dat$hedges == "0.2" & dat$n_prior == 0,], 
    aes(x = n_prior, y = n_tot, alpha = 0.5)
  ) +
  geom_point(size = 4, colour = my_purple, shape = 15) +
  geom_point(size = 4, colour = "black", shape = 22, alpha = 1, fill = NA) +
  facet_grid(~ my_cat) + 
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0),
                     limits = c(0,0)) +  
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(600,800),
                     limits = c(550,850)) + 
  ylab("Total N (exp + control)") +
  my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none") 


top_right <-  ggplot(dat[dat$sd_ec_ratio == 1 & dat$hedges == "0.2" & dat$n_prior != 0,], 
                 aes(x = n_prior, y = n_tot, 
                     alpha = bae_pow/10000)) +
  facet_grid(~my_cat) +
  geom_line(size = 4, colour = my_purple) + 
  geom_point(size = 4, colour = my_purple, shape = 15) +
  geom_point(size = 4, colour = "black", shape = 22, alpha = 1, fill = NA) +
  
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(10,20,50,100,200),
                     limits = c(10,200)) +  
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(600,800),
                     limits = c(550,850)) + 
  my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none") +
  scale_alpha(range = c(0.5, 1))


bottom_left <- ggplot(dat[dat$sd_ec_ratio == 1 & dat$hedges != "0.2" & dat$n_prior == 0,],
                 aes(x = n_prior, y = n_tot, 
                     color = as.factor(hedges)), alpha = 0.5) + 
  geom_point(size = 4, colour = c(my_watergreen, my_yellow), shape = c(19,17)) +
  geom_point(size = 4, colour = "black", shape = c(21,24), alpha = 1, fill = NA) +
  
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0),
                     limits = c(0,0)) +  
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(10,25,50,100),
                     limits = c(10,130)) + 
  my_theme +
  theme(axis.line.x = element_line(arrow = arrow(0)),
        axis.line.y = element_line(arrow = arrow(0)),
        axis.title.x=element_text(colour = "white"), 
        axis.title.y=element_text(colour = "white"), 
        legend.position = "none") +
  geom_point(aes(x = 0, y = 20), colour = "red", size = 4, shape = 18)


bottom_right <- 
  ggplot(dat[dat$sd_ec_ratio == 1 & dat$hedges != "0.2"  & dat$n_prior != 0,], 
                 aes(x = n_prior, y = n_tot, colour = as.factor(hedges), 
                     fill = as.factor(hedges), shape = as.factor(hedges), alpha = bae_pow/10000)) +
    geom_line(size = 4) + 
    geom_point(size = 4) + 
    scale_fill_manual(values = c(my_watergreen, my_yellow)) + 
    scale_colour_manual(values = c(my_watergreen, my_yellow)) + 
    scale_shape_manual(values = c(21,24)) +
    geom_point(size = 4, colour = "black", alpha = 1, fill = NA) +
    scale_x_continuous(trans = "pseudo_log",
                       breaks = c(10,20,50,100,200),
                       limits = c(10,200)) +  
    scale_y_continuous(trans = "pseudo_log",
                       breaks = c(10,25,50,100),
                       limits = c(10,130)) +   
    xlab("N prior") +
    my_theme +
    theme(axis.title.y=element_blank(),        
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          legend.position = "none") +
    scale_alpha(range = c(0.5, 1))

images <- list(top_left, top_right, bottom_left, bottom_right)

RePAIR_sim <- 
  ggarrange(
    
    top_left, 
    top_right,
    bottom_left,
    bottom_right,
    
    ncol = 2, nrow = 2, 
    heights = c(1,2), widths = c(1,4)
    
  )

saveRDS(RePAIR_sim, file = "figures/RePAIR_sim.rds")

