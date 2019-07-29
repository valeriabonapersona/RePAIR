## The RePAIR on relacs dataset

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")
set.seed(8709)



# Import datasets ---------------------------------------------------------
dat <- read.csv("n_stat_data.csv") # criteria: adult males OL task, >1h retention (both short and long), controls >50%, novel_exp >0
dat <- dat %>% select(-contains("X"))

prior <- read_excel("prior_control_literature.xlsx")
prior$sd <- prior$sem * sqrt(prior$N)
prior$var <- prior$sd^2


# From MaB, hedges G = 0.57 for males/OIL/short&long/mice&rats/acrossELA models
# 0.57 is likely to be overestimated (problems with lit_meta + also rats)
# Considered 0.4 as "real effect size"
dat %>%
  select(control_or_ELS_animal, di) %>%
  group_by(control_or_ELS_animal) %>%
  summarize(mean(di), sd(di), length(di)) %>%
  gather(key = out, value = amount, `mean(di)`, `sd(di)`, `length(di)`) %>%
  unite(var, control_or_ELS_animal, out, sep="_") %>%
  spread(key = var, value = amount) -> dat_sum

names(dat_sum) <- c("Ln", "Lmean", "Lsd", "Pn", "Pmean", "Psd")
dat_sum <- metafor::escalc(m1i = Pmean, sd1i = Psd, n1i = Pn,
                           m2i = Lmean, sd2i = Lsd, n2i = Ln,
                           measure = "SMD", data = dat_sum)

# Effect size estimated here is -.36


# Power calculation
# run whats_nC from sim_with_prior.Rmd
pow_cal <- expand.grid(
  "hedges"  = c(.36, .4, .57),
  "tail"     = c("two.sided", "one.sided")
) #all combinations
pow_cal$tail <- as.character(pow_cal$tail)
pow_cal$n_c <- ceiling(purrr::pmap_dbl(list(pow_cal$hedges, as.character(pow_cal$tail)), whats_nC, n_ratio = 1, sd_ratio = 1))
pow_cal$n_tot <- pow_cal$n_c*2


# is RELACS powered enough? YES

# overall
dat %>%
  group_by(control_or_ELS_animal) %>%
  count() %>%
  spread(key = control_or_ELS_animal, value = n) %>%
  mutate(n_ratio = P / L) %>%
  mutate(pow = whats_pow(n_low = L, n_ratio = n_ratio))


# per paper
dat %>%
  group_by(contact,control_or_ELS_animal) %>%
  count() %>%
  spread(key = control_or_ELS_animal, value = n) %>%
  mutate(min_n = min(c(L,P))) %>%
  mutate(max_n = max(c(L,P))) %>%
  mutate(n_ratio = max_n / min_n) %>%
  mutate(pow = whats_pow(n_low = min_n, n_ratio = n_ratio)) %>% 
  ungroup() %>% summarize(median(pow))
## average power achieved is (again...) .2 . the 20% of 7 is 1.4, so if I have between 1 and 2
## papers significant, then I kind of validate the calculations

# Does the amount of achieved power agree with the amount of sig studies? YES
dat %>%
  group_by(contact) %>%
  # spread(key = control_or_ELS_animal, value = di) %>%
  summarize(get_p(di, control_or_ELS_animal)) %>%
  count(`get_p(di, control_or_ELS_animal)` <= 0.05)


# RePAIR on relacs --------------------------------------------------------
# Run sample_post function from sim_with_prior.Rmd
# Prepare dataset
dat_an <- cbind(dat_sum, 0, 0, 1)
names(dat_an) <- c("c_n", "c_mean", "c_sd", 
                   "e_n", "e_mean", "e_sd", 
                   "p_n", "p_mean", "p_var")
dat_an$c_var <- dat_an$c_sd ^ 2
dat_an$e_var <- dat_an$e_sd ^ 2

# Without prior
## Frequentist t-test
t.test(di ~ control_or_ELS_animal, data = dat)

## Bayesian
post_c_np <- sample_post(n_group = dat_an$c_n[1], mean_group = dat_an$c_mean[1], 
                         s2_group = dat_an$c_var[1])
post_e <- sample_post(n_group = dat_an$e_n[1], mean_group = dat_an$e_mean[1], 
                      s2_group = dat_an$e_var[1])

cred_int_fun_np <- post_e - post_c_np
quantile(cred_int_fun_np, probs = c(0.025, .5, 0.975))


# With prior from relacs
dat_an$c_n[1] - (dat_an$c_n[1] / 100 * 30) # I can remove up to 92 animals according to my rule of thumb

## Calculate prior from relacs
## I randomly pick 92 animals from relacs dataset and remove them
dat$id_uni <- as.factor(paste(dat$id, dat$lab_lit, sep = "_"))
identical(length(unique(dat$id_uni)), nrow(dat))

#set.seed(7459)
rem <- sample.int(dat_an$c_n[1], 92)
dat %>%
  filter(control_or_ELS_animal == "L") %>%
  slice(rem) %>%
  select(id_uni) -> r_prior_sel

dat$r_p_sel <- ifelse(dat$id_uni %in% r_prior_sel$id_uni, TRUE, FALSE)

dat %>%
  filter(control_or_ELS_animal == "L") %>%
  group_by(r_p_sel) %>%
  summarize(length(di), mean(di), var(di)) -> r_dat_p
r_dat_p <- as.data.frame(r_dat_p)

post_c_rp <- sample_post(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                         mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                         s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4], 
                         n_pilot = r_dat_p[r_dat_p$r_p_sel == TRUE,2], 
                         mean_pilot = r_dat_p[r_dat_p$r_p_sel == TRUE,3], 
                         s2_pilot = r_dat_p[r_dat_p$r_p_sel == TRUE,4]) 

cred_int_fun_rp <- post_e - post_c_rp
quantile(cred_int_fun_np, probs = c(0.025, .5, 0.975))


# Prior from literature ---------------------------------------------------
## meta-analysis
mod <- metafor::rma(mean, sd^2, method = "FE", data = prior)

## bayesian
### run functions from repair below

for (i in 1:nrow(prior)) {
  
  print(prior$author[i])
  
  if (i == 1) {
    
    p_par <- find_prior_par(n_pilot    = prior$N[i],
                            mean_pilot = prior$mean[i],
                            s2_pilot   = prior$var[i],
                            belief     = 1)
    
  } else {
    
    p_par[i,] <- NA
    prev <- i - 1
    
    post_par <- find_post_par(n_group = prior$N[i],
                              mean_group = prior$mean[i],
                              s2_group   = prior$var[i],
                              belief_group = 1,
                              mu0 = as.numeric(p_par$mu0[prev]),
                              k0 = as.numeric(p_par$k0[prev]),
                              v0 = as.numeric(p_par$v0[prev]),
                              sigma0_2 = as.numeric(p_par$sigma0_2[prev]),
                              n0_cor = as.numeric(p_par$n0_cor[prev])
    )
    p_par[i, "mu0"] <- post_par$mu1
    p_par[i, "k0"] <- post_par$k1
    p_par[i, "v0"] <- post_par$v1
    p_par[i, "sigma0_2"] <- post_par$sigma1_2
    p_par[i, "n0_cor"] <- post_par$n1_cor
    
  }
  
  print(nrow(p_par))
  
}


#with my parameters I now calculate the posterior of the control
post_c_lp_app <- sample_post_cor(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                                 mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                                 s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4],
                                 mu0 = p_par$mu0[nrow(p_par)], k0 = p_par$k0[nrow(p_par)], 
                                 v0 = p_par$v0[nrow(p_par)], sigma0_2 = p_par$sigma0_2[nrow(p_par)])


cred_int_fun_lp_app <- post_e - post_c_lp_app
quantile(cred_int_fun_lp_app, probs = c(0.025, .5, 0.975))


## first meta-analyse data ## Bayesian way better than meta-analysing?
post_c_lp_meta <- sample_post(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                              mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                              s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4], 
                              n_pilot = sum(prior$N), 
                              mean_pilot = round(mod$beta[1],2), 
                              s2_pilot = (sqrt(sum(prior$N))*round(mod$se,2))^2) #transform se from metafor to var 

cred_int_fun_lp_meta <- post_e - post_c_lp_meta
quantile(cred_int_fun_lp_meta, probs = c(0.025, .5, 0.975))

p_21 <- ggdensity(post_e) +
  xlim(0.5,0.7) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  geom_density(fill = viridis(option = "C", n = 7)[7], alpha = 0.3) +
  my_theme

p_12 <- ggdensity(post_c_np) +
  xlim(0.5,0.7) + ylim(0,50) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  geom_density(fill = viridis(option = "C", n = 7)[1], alpha = 0.3) +
  my_theme

p_22 <- ggdensity(post_c_rp) +
  xlim(0.5,0.7) + ylim(0,50) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  geom_density(fill = viridis(option = "C", n = 7)[1], alpha = 0.3) +
  my_theme

p_32 <- ggdensity(post_c_lp_app) +
  xlim(0.5,0.7) + ylim(0,50) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  geom_density(fill = viridis(option = "C", n = 7)[1], alpha = 0.3) +
  my_theme

p_13 <- 
  ggdensity(cred_int_fun_np) +
  xlim(-0.3,0.3) + ylim(0,30) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  annotate("rect", 
           xmin = quantile(cred_int_fun_np, probs = 0.025), 
           xmax = quantile(cred_int_fun_np, probs = 0.975), 
           ymin = -Inf, ymax = Inf, 
           alpha = .1) +
  geom_vline(xintercept = 0, colour = "red") +
  geom_density(fill = viridis(option = "D", n = 7)[6], alpha = 0.3) +
  my_theme

p_23 <- 
  ggdensity(cred_int_fun_rp) +
  xlim(-0.3,0.3) + ylim(0,30) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  annotate("rect", 
           xmin = quantile(cred_int_fun_rp, probs = 0.025), 
           xmax = quantile(cred_int_fun_rp, probs = 0.975), 
           ymin = -Inf, ymax = Inf, 
           alpha = .1) +
  geom_vline(xintercept = 0, colour = "red") +
  geom_density(fill = viridis(option = "D", n = 7)[6], alpha = 0.3) +
  my_theme

p_33 <- 
  ggdensity(cred_int_fun_lp_app) +
  xlim(-0.3,0.3) + ylim(0,30) +
  xlab("Discrimination index") +
  ylab("Density distribution") +
  annotate("rect", 
           xmin = quantile(cred_int_fun_lp_app, probs = 0.025), 
           xmax = quantile(cred_int_fun_lp_app, probs = 0.975), 
           ymin = -Inf, ymax = Inf, 
           alpha = .1) +
  geom_vline(xintercept = 0, colour = "red") +
  geom_density(fill = viridis(option = "D", n = 7)[6], alpha = 0.3) +
  
  my_theme



# Move to a new page
svg(filename = "figures/analysis_relacs.svg")
grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(p_21, vp = define_region(row = 2, col = 1)) 

print(p_12, vp = define_region(row = 1, col = 2))
print(p_22, vp = define_region(row = 2, col = 2))   
print(p_32, vp = define_region(row = 3, col = 2))   

print(p_13, vp = define_region(row = 1, col = 3))
print(p_23, vp = define_region(row = 2, col = 3))   
print(p_33, vp = define_region(row = 3, col = 3))   

dev.off()

