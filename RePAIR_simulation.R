## Script: "RePAIR simulation study: relationship between prior, sample size and power"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")

# N for power 0.8 ----------------------------------------------------
# factors varying in the simulation are explained in Table S2 of the manuscript
# _c = control, _e = experimental
# n_ = sample size, sd_ = standard deviation, s2 = variance, pow = power

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

# Population parameters (p_)
dat$p_mean_c <- 0 # µ
dat$p_sd_c <- 1 # standard deviation (sd)
dat$p_sd_e <- dat$p_sd_c * dat$sd_ec_ratio
dat$p_sd_pooled <- sqrt(((dat$p_sd_e)^2 + (dat$p_sd_c)^2)/2) 
dat$p_mean_e <- dat$p_mean_c + (dat$p_sd_pooled * dat$hedges) 

# Parameters experiment 
dat[, c("mean_c", "mean_e", "s2_c", "s2_e", "repair_pow")] <- NA 

#simulation
n_sim <- 10000 # datasets simulated
n_sam <- 10000 # samples drawn

for (i in c(1:nrow(dat))) {
  print(i)
  
  # Create variable for output
  contains_zero <- FALSE # how often does the 95%CI contain zero? expected 20%
  
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
    c_prior_par <- find_prior_par(n_exp = dat[i,]$n_prior, 
                                  mean_exp = 0, 
                                  s2_exp = 1, 
                                  belief = 1)
    if (dat[i,]$n_prior == 0) {
      c_prior_par[1,] <- c(0,0,0,1,0)
    }
    
    
    c_post_par <- find_post_par(n_exp = dat[i,]$n_c,
                                mean_exp = dat[i,]$mean_c,
                                s2_exp = dat[i,]$s2_c, 
                                belief = 1,
                                data_par = c_prior_par)
    
    c_sampled <- sample_post(data_par = c_post_par, 
                             n_sampled = n_sam)

    
    ## Experimental
    e_post_par <- find_post_par(n_exp = dat[i,]$n_e,
                                mean_exp = dat[i,]$mean_e,
                                s2_exp = dat[i,]$s2_e, 
                                belief = 1)
    
    e_sampled <- sample_post(data_par = e_post_par, 
                             n_sampled = n_sam)
    
    
    # Credibility interval
    cred_int <- e_sampled - c_sampled
    
    
    # Power
    quantile(cred_int, probs = c(.025, .975))
    
    contains_zero[sim] <- 0 >= quantile(cred_int, probs = .025) &
      0 <= quantile(cred_int, probs = .975)
    
  }
  
  dat[i,]$repair_pow <- n_sim - sum(contains_zero)
  
}

#write.csv(dat, file = "sim_prior.csv")
dat <- read.csv("sim_prior.csv")

# Visualization RePAIR ----------------------------------------------------
#dat <- read.csv("/Users/vbonape2/surfdrive/Work/PhD/nStat/n_stat_git/sim_prior_power.csv")
dat$my_x_cat <- ifelse(dat$n_prior == 0, "No prior", "With RePAIR")
dat$my_y_cat <- ifelse(dat$n_tot <= 200, "small", "large")
dat$n_tot_02 <- ifelse(dat$n_tot > 200, dat$n_tot / 100, dat$n_tot)
current <- data.frame(hedges = 0.9, 
                      n_prior = 0, 
                      n_tot = 20, 
                      my_x_cat = "No prior", 
                      my_y_cat = "small", 
                      repair_pow = 1)
my_text <- data.frame(hedges = c(0.2,0.5,0.9),
                      n_prior = rep(160,3), 
                      n_tot = c(605, 72, 25), 
                      my_x_cat = rep("With RePAIR",3), 
                      my_y_cat = c("large","small", "small"), 
                      repair_pow = rep(1,3), 
                      lab = c("Hedge's G = 0.2", "Hedge's G = 0.5", "Hedge's G = 0.9"))

ggplot(
  dat[dat$sd_ec_ratio != 2,], 
  aes(x = n_prior, y = n_tot, 
      colour = factor(hedges), fill = factor(hedges), shape = factor(hedges), 
      alpha = repair_pow/10000) 
  ) +
  
  geom_line(size = 7, linejoin = "round") + 
  geom_point(size = 8, fill = "white", alpha = 1, show.legend = FALSE) +
  geom_point(size = 8, show.legend = FALSE) + 
  
  scale_fill_manual(values = c(my_yellow, my_watergreen, my_purple), guide = FALSE) + 
  scale_colour_manual(values = c(my_yellow, my_watergreen, my_purple), guide = FALSE) + 
  scale_shape_manual(values = c(22,21,24), guide = FALSE) +
  scale_alpha(range = c(0.3, 0.9), name = "Power:") +
  
  geom_point(size = 8, colour = "black", alpha = 1, fill = NA) +
  
  facet_grid(my_y_cat ~ my_x_cat, scales = "free", space = "free") + 
  
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0,10,20,50,100,200)) + 
  scale_y_continuous(trans = "pseudo_log", 
                     breaks = c(0,20,40,80,120,600, 750)) + 
  
  geom_point(data = current, aes(x = n_prior, y = n_tot), 
             colour = "red", size = 8, shape = 18, alpha = 1) +
  geom_text(data = my_text, aes(x = n_prior, y = n_tot, label = lab), 
            colour = "black", alpha = 1) +
  
  xlab(expression(paste(N[prior], italic(" (log scale)")))) +
  ylab(expression(paste(N[total], " = ", N[exp], "+", N[con], italic(" (log scale)")))) +
  
  my_theme + 
  theme(legend.position = c(0.97,0.9),
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
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

saveRDS(gt_repair, file = "figures/gt_repair.rds")

