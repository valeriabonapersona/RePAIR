## Script: "Sensitivity simulation study"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019


# Environment -------------------------------------------------------------
source("RePAIR_functions.R")
set.seed(8709)

# Datasets ----------------------------------------------------------------
limits <- read.csv("limits_means.csv") # from RePAIR_prior.R
sens <- read.csv("sim_prior.csv") # from RePAIR_simulation.R


# Sensitivity -------------------------------------------------------------
# Limits dataset: variation in relacs and lit controls
# population mean = 0, values after normalization and 10x sampling from population papers

## add n_prior 0 and 200 for similarity with simulation study (RePAIR_simulation.R)
limits[nrow(limits) + 1,
       c("X0.25", "X0.75", "number_anim")]<- c(0,0,0)
limits[nrow(limits) + 1,
       c("X0.25", "X0.75", "number_anim")]<- c(-0.1,0.1,200)


mean_prior <- abs(c(limits$X0.25, limits$X0.75))
mean_prior <- unique(c(mean_prior * -1, mean_prior))

# dataset preparation
sens <- sens %>% 
  # select(-X) %>% 
  filter(hedges == 0.5, sd_ec_ratio == 1, n_prior != 10) %>%
  mutate(mean_c = NA, s2_c = NA, mean_e = NA, s2_e = NA) %>%
  tidyr::crossing(mean_prior) 

# select biologically relevant
sens <- data.frame(sens,
                   limits[match(sens$n_prior, limits$number_anim),
                          c("X0.25","X0.75")]) 

sens$relevant <- ifelse(sens$mean_prior >= sens$X0.25 & 
                          sens$mean_prior <= sens$X0.75, 
                        "yes", "no")

sens <- sens[sens$relevant == "yes",]

# simulation
## ATTENTION: it takes a few hours (best overnight)
## for a quick look, change n_sim to 10 and n_sam to 1000
sens$repair_pow_sens <- NA

n_sim <- 10000 # simulated datasets
n_sam <- 10000 # sampled from posterior

for (i in c(1:nrow(sens))) {
  print(i)
  
  # Create variable for output
  contains_zero <- FALSE 
  
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
    
    if (sens[i,]$n_prior == 0) {
      c_prior_par[1,] <- c(0,0,0,1,0)
    }
    
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

    
    # Confidence interval
    conf_int <- e_sampled - c_sampled
    
    # Power
    
    quantile(conf_int, probs = c(.025, .975))

    contains_zero[sim] <- 0 >= quantile(conf_int, probs = .025) &
      0 <= quantile(conf_int, probs = .975)
    
  }
  
  sens[i,]$repair_pow_sens <- n_sim - sum(contains_zero)
  x <- sens$repair_pow_sens
  write.csv(x, file = "check_sens.csv")
  
}


#write.csv(sens, file = "sim_sensitivity.csv")
sens <- read.csv("sim_sensitivity.csv")





# Visualization sensitivity -----------------------------------------------
sens$my_x_cat <- ifelse(sens$n_prior == 0, "No prior", "With RePAIR")
sens$my_alpha_cat <- ifelse(sens$mean_prior == 0, "Perfect estimation",
                       ifelse(sens$mean_prior == sens$X0.25 | 
                                sens$mean_prior == sens$X0.75, "Sensitivity variation (0.25-0.75 quantile)", NA))
current <- data.frame(n_prior = 100, 
                      my_x_cat = "With RePAIR", 
                      repair_pow_sens = 0.23, 
                      lab = "Current median prospective power")

sens %>%
  filter(my_alpha_cat != "NA", n_prior != 10) %>%
  ggplot(aes(x = n_prior, y = repair_pow_sens/10000)) +
  xlab(expression(paste(N[prior], italic(" (log scale)")))) +
  ylab(expression(paste(italic("Prospective"), " power"))) +
  facet_grid(~ my_x_cat, scales = "free", space = "free") + 
  
  scale_y_continuous(breaks = c(0, 0.2, 0.5, 0.8, 1),
                     labels = c("0","20%","50%","80%","100%"), 
                     limits = c(0,1)) +
  scale_x_continuous(trans = "pseudo_log", ## do as factors?
                     breaks = c(0, 20, 50, 100, 200)) +
  geom_hline(yintercept = 0.8, colour = "darkgreen") +
  geom_hline(yintercept = 0.2, size = 0.5, linetype = "dotted", colour = "red") + 
  geom_line(aes(group = n_prior, size = 0.3)) +
  geom_point(aes(colour = my_alpha_cat, size = 1)) +
  geom_text(data = current, aes(x = n_prior, y = repair_pow_sens, label = lab), 
            colour = "red", alpha = 1) +
  my_theme + 
  scale_color_manual(values = c(my_watergreen, my_watergreen_light), 
                     name = "Estimation population mean") + 
  theme(legend.position = "none",#c(0.5,0.1),
        legend.title.align = 0.5,
      #  legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.spacing.x = unit(0,"cm")) +
  guides(size = FALSE, 
         colour = guide_legend(direction = "horizontal")) -> sensit


## layout with grid
gt_sensit <- ggplotGrob(sensit)

# remove right facet
# change size facets
gt_sensit$widths[5] <- unit(0.4,"null")

# change size breaks axes
gt_sensit$widths[6] <- unit(30,"pt")

grid.draw(gt_sensit)

#saveRDS(gt_sensit, file = "figures/sensitivity.rds")
