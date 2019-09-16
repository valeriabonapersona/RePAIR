## The RePAIR sensitivity
# Environment -------------------------------------------------------------
source("RePAIR_functions.R")
set.seed(8709)

# Datasets ----------------------------------------------------------------
limits <- read.csv("limits_means_02.csv")
sens <- read.csv("sim_prior.csv")


# Sensitivity -------------------------------------------------------------
# Defined from limits dataset
n_prior <- c(10,20,50,100,200)
mean_prior <- c(0, .05, 0.1, 0.3, 0.5, -.05, -0.1, -0.3, -0.5) # population mean = 0, values after normalization and 10x sampling from population papers
##add 0.9?

# dataset preparation
sens <- sens %>% 
  # select(-X) %>% 
  filter(hedges == 0.5, sd_ec_ratio == 1) %>%
  mutate(mean_c = NA, s2_c = NA, mean_e = NA, s2_e = NA) %>%
  tidyr::crossing(mean_prior) 


#simulation
sens$bae_pow_bad <- NA

n_sim <- 10000
n_sam <- 10000 # sampled from posterior

for (i in c(1:nrow(sens))) {
  print(i)
  
  # Create variable for output
  contains_zero <- FALSE #how often does the 95%CI contain zero? expected 20%
  
  # Simulation loop
  for (sim in c(1:n_sim)) {
    
    # From population to sample (from N distribution)
    ## Control
    d_c <- rnorm(n = sens[i,]$n_c, mean = sens[i,]$p_mean_c, sd = sens[i,]$p_sd_c)
    sens[i,]$mean_c <- mean(d_c)
    sens[i,]$s2_c <- var(d_c)
    
    ## Experimental
    d_e <- rnorm(n = sens[i,]$n_e, mean = sens[i,]$p_mean_e, sd = sens[i,]$p_sd_e)
    sens[i,]$mean_e <- mean(d_e)
    sens[i,]$s2_e <- var(d_e)
    
    
    # Sampling from posterior
    ## Control
    c_prior_par <- find_prior_par(n_exp = sens[i,]$n_prior, 
                                  mean_exp = sens[i,]$mean_prior, 
                                  s2_exp = 1, 
                                  belief = 1)
    
    c_post_par <- find_post_par(n_exp = sens[i,]$n_c,
                                mean_exp = sens[i,]$mean_c,
                                s2_exp = sens[i,]$s2_c, 
                                belief = 1,
                                data_par = c_prior_par)
    
    c_sampled <- sample_post(data_par = c_post_par, 
                             n_sampled = n_sam)
    
    
    ## Experimental
    e_post_par <- find_post_par(n_exp = sens[i,]$n_e,
                                mean_exp = sens[i,]$mean_e,
                                s2_exp = sens[i,]$s2_e, 
                                belief = 1)
    
    e_sampled <- sample_post(data_par = e_post_par, 
                             n_sampled = n_sam)

    
    # Credibility interval
    cred_int <- e_sampled - c_sampled
    
    # Power
    
    quantile(cred_int, probs = c(.025, .975))
    #print(quantile(cred_int, probs = c(.025, .975)))
    contains_zero[sim] <- 0 >= quantile(cred_int, probs = .025) &
      0 <= quantile(cred_int, probs = .975)
    
  }
  
  sens[i,]$bae_pow_bad <- n_sim - sum(contains_zero)
  
}


#write.csv(sens, file = "sim_sensitivity.csv")
write.csv(sens, file = "sim_sensitivity_cor.csv")

#sens <- read.csv("sim_sensitivity.csv") ##old






# Visualization sensitivity -----------------------------------------------

limits[nrow(limits) + 1,
      c("X0.25", "X0.75", "number_anim")]<- c(-0.1,0.1,200)
limits[nrow(limits) + 1,
       c("X0.25", "X0.75", "number_anim")]<- c(0,0,0)

sens <- data.frame(sens,
                   limits[match(sens$n_prior, limits$number_anim),
                                c("X0.25","X0.75")]) 

sens$yes <- ifelse(sens$mean_prior >= sens$X0.25 & sens$mean_prior <= sens$X0.75, "yes", "no")  ## do this up and then run simulation

sens %>%
  filter(yes != "no", abs(mean_prior)!= 0.05 ) %>%
  ggplot(aes(x = as.factor(n_prior), y = bae_pow_bad/10000)) +
  ylim(0,1.1) + 
  # scale_x_continuous(trans = "pseudo_log",
  #                    breaks = c(10,20,50,100,200),
  #                    limits = c(10,200)) +  
  geom_line(aes(group = n_prior,size = 0.3)) +
  geom_point(aes(colour = factor(mean_prior == 0.0), 
                 #shape = factor(abs(mean_prior)), 
                 alpha = -abs(mean_prior), size = 1)) +
  geom_hline(yintercept = 0.8) +
  #geom_hline(yintercept = 0.5, size = 0.5, linetype = "dotted") + 
  geom_hline(yintercept = 0.2, size = 0.5, linetype = "dotted", colour = "red") + 
  
  my_theme + 
  scale_color_viridis(discrete = TRUE) + theme(legend.position = "none") ->sensit
# svg(filename = "figures/sensit_02.svg")
# sensit
# dev.off()

saveRDS(sensit, file = "figures/sensitivity.rds")
