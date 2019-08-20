## The RePAIR on relacs dataset
## To do: better names for functions of RePAIR 
# Environment -------------------------------------------------------------
source("RePAIR_functions.R")


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

post_c_np <- sample_post_cor(n_group = dat_an$c_n[1], mean_group = dat_an$c_mean[1], 
                         s2_group = dat_an$c_var[1])
post_e <- sample_post_cor(n_group = dat_an$e_n[1], mean_group = dat_an$e_mean[1], 
                      s2_group = dat_an$e_var[1])

exp_res <- data.frame(post_e = post_e, 
                       post_c = post_c_np, 
                       diff_post = post_e - post_c_np)
quantile(exp_res$diff_post, probs = c(0.025, .5, 0.975))


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

par_c_rp <- find_prior_par(n_group = r_dat_p[r_dat_p$r_p_sel == TRUE,2], 
                           mean_group = r_dat_p[r_dat_p$r_p_sel == TRUE,3], 
                           s2_group = r_dat_p[r_dat_p$r_p_sel == TRUE,4], 
                           belief = 1,
                           mu0 = 0, k0 = 0, v0 = 0, sigma0_2 = 0, n0_cor = 0,
                           n_sampled = 10000) 
post_c_rp <- sample_post_cor(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                         mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                         s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4], 
                         mu0 = par_c_rp$mu0, 
                         k0 = par_c_rp$k0, 
                         v0 = par_c_rp$v0, sigma0_2 = par_c_rp$sigma0_2) 


prel_res <- data.frame(post_e = post_e, 
                      post_c = post_c_rp, 
                      diff_post = post_e - post_c_rp)
quantile(prel_res$diff_post, probs = c(0.025, .5, 0.975))


# Prior from literature ---------------------------------------------------
## meta-analysis
mod <- metafor::rma(mean, sd^2, method = "FE", data = prior)

## bayesian
### run functions from repair below

for (i in 1:nrow(prior)) {
  
  print(prior$author[i])
  
  if (i == 1) {
    
    p_par <- find_prior_par(n_group    = prior$N[i],
                            mean_group = prior$mean[i],
                            s2_group   = prior$var[i],
                            belief     = 1)
    
  } else {
    
    p_par[i,] <- NA
    prev <- i - 1
    
    post_par <- find_prior_par(n_group = prior$N[i],
                              mean_group = prior$mean[i],
                              s2_group   = prior$var[i],
                              belief = 0.3,
                              mu0 = as.numeric(p_par$mu0[prev]),
                              k0 = as.numeric(p_par$k0[prev]),
                              v0 = as.numeric(p_par$v0[prev]),
                              sigma0_2 = as.numeric(p_par$sigma0_2[prev]),
                              n0_cor = as.numeric(p_par$n0_cor[prev])
    )
    p_par[i, "mu0"] <- post_par$mu0
    p_par[i, "k0"] <- post_par$k0
    p_par[i, "v0"] <- post_par$v0
    p_par[i, "sigma0_2"] <- post_par$sigma0_2
    p_par[i, "n0_cor"] <- post_par$n0_cor
    
  }
  
  print(nrow(p_par))
  
}


#with my parameters I now calculate the posterior of the control
post_c_lp_app <- sample_post_cor(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                                 mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                                 s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4],
                                 mu0 = p_par$mu0[nrow(p_par)], k0 = p_par$k0[nrow(p_par)], 
                                 v0 = p_par$v0[nrow(p_par)], sigma0_2 = p_par$sigma0_2[nrow(p_par)])

lit_res <- data.frame(post_e = post_e, 
                      post_c = post_c_lp_app, 
                      diff_post = post_e - post_c_lp_app)
quantile(lit_res$diff_post, probs = c(0.025, .5, 0.975))


## first meta-analyse data ## Bayesian way better than meta-analysing?
meta_par_c <- find_prior_par(n_group = sum(prior$N), 
                           mean_group = round(mod$beta[1],2), 
                           s2_group = (sqrt(sum(prior$N))*round(mod$se,2))^2, 
                           belief = 0.3) 
post_c_lp_meta <- sample_post_cor(n_group = r_dat_p[r_dat_p$r_p_sel == FALSE,2], 
                              mean_group = r_dat_p[r_dat_p$r_p_sel == FALSE,3], 
                              s2_group = r_dat_p[r_dat_p$r_p_sel == FALSE,4], 
                              mu0 = meta_par_c$mu0, 
                              k0 = meta_par_c$k0, 
                              v0 = meta_par_c$v0, sigma0_2 = meta_par_c$sigma0_2) #transform se from metafor to var 
met_res <- data.frame(post_e = post_e, 
                      post_c = post_c_lp_meta, 
                      diff_post = post_e - post_c_lp_meta)
quantile(met_res$diff_post, probs = c(0.025, .5, 0.975))


####generalize this graph!!
my_res <- data.frame(cbind(prior = rep(c("exp_res","prel_res", "lit_res"), each =10000),
                           rbind(exp_res, prel_res, lit_res)))

my_res %>% 
  gather(key = "post", value = "val", -prior) %>%
  mutate(fac = ifelse(post == "diff_post", "Difference", "Groups")) %>%
    ggplot(aes(x = val, fill = post)) +
    geom_density() +
    facet_grid(prior~fct_rev(fac), scales = "free")+
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = 0, colour = my_bad)) + 
    my_theme + 
    std_fill_dark +
    theme(legend.position = "none") -> res_rep_rel

saveRDS(res_rep_rel, "figures/repair_results.rds")
