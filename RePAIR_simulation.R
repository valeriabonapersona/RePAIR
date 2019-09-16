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
dat[, c("mean_c", "mean_e", "s2_c", "s2_e", "bae_pow")] <- NA

#simulation
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
dat$my_y_cat <- ifelse(dat$n_tot <= 200, "small", "large")
dat$n_tot_02 <- ifelse(dat$n_tot > 200, dat$n_tot / 100, dat$n_tot)
current <- data.frame(hedges = 0.9, 
                      n_prior = 0, 
                      n_tot = 20, 
                      my_cat = "No prior", 
                      my_y_cat = "small", 
                      bae_pow = 1)
my_text <- data.frame(hedges = c(0.2,0.5,0.9),
                      n_prior = rep(160,3), 
                      n_tot = c(605, 72, 25), 
                      my_cat = rep("With RePAIR",3), 
                      my_y_cat = c("large","small", "small"), 
                      bae_pow = rep(1,3), 
                      lab = c("Hedge's G = 0.2", "Hedge's G = 0.5", "Hedge's G = 0.9"))

ggplot(
  dat[dat$sd_ec_ratio != 2,], 
  aes(x = n_prior, y = n_tot, 
      colour = factor(hedges), fill = factor(hedges), shape = factor(hedges), 
      alpha = bae_pow/10000)
) +
  
 # geom_point(size = 4, colour = factor(hedges), shape = 15) +
 # geom_point(size = 4, colour = c(my_purple, my_watergreen, my_yellow), shape = c(15,19,17)) +
  
  geom_line(size = 7, linejoin = "round") + 
  geom_point(size = 8, fill = "white", alpha = 1, show.legend = FALSE) +
  
  geom_point(size = 8, show.legend = FALSE) + 
  scale_fill_manual(values = c(my_yellow, my_watergreen, my_purple), guide = FALSE) + 
  scale_colour_manual(values = c(my_yellow, my_watergreen, my_purple), guide = FALSE) + 
  scale_shape_manual(values = c(22,21,24), guide = FALSE) +
  scale_alpha(range = c(0.3, 0.9), name = "Power:") +
  geom_point(size = 8, colour = "black", alpha = 1, fill = NA) +
  facet_grid(my_y_cat ~ my_cat, scales = "free", space = "free") + 
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0,10,20,50,100,200)) + 
  scale_y_continuous(trans = "pseudo_log", 
                     breaks = c(0,20,40,80,120,600, 750)) + 
  geom_point(data = current, aes(x = n_prior, y = n_tot), colour = "red", size = 8, shape = 18, alpha = 1) +
  geom_text(data = my_text, aes(x = n_prior, y = n_tot, label = lab), colour = "black", alpha = 1) +
  xlab(expression(paste(N[prior], italic(" (log scale)")))) +
  ylab(expression(paste(N[total], " = ", N[exp], "+", N[con], italic(" (log scale)")))) +
   my_theme + 
  theme(legend.position = c(0.97,0.9),
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
     #   legend.text = element_blank(),
        legend.spacing.x = unit(0,"cm")) -> repair_plot


## layout with grid
gt_repair <- ggplotGrob(repair_plot)

gt_repair$heights[7] <- unit(30, "pt") 
# remove right facet
panels <- grep("panel", gt_repair$layout$name)
right_facet <- unique(gt_repair$layout$b[panels])
gt_repair <- gt_repair[, -(right_facet)]

#gt_repair$heights[1] <- unit(1.5, "cm")
#gt_repair$heights[4] <- unit(3, "cm")

# change size facets
gt_repair$widths[5] <- unit(0.4,"null")
gt_repair$heights[8] <- unit(1.2, "null")

# change size breaks axes
gt_repair$widths[6] <- unit(30,"pt")
gt_repair$heights[9] <- unit(30, "pt")

grid.draw(gt_repair)

#saveRDS(RePAIR_sim, file = "figures/RePAIR_sim.rds")
saveRDS(gt_repair, file = "figures/gt_repair.rds")

