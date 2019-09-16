## The RePAIR on relacs dataset
## To do: better names for functions of RePAIR 
## fix with new functions

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")
set.seed(1234)


# Import datasets ---------------------------------------------------------
dat <- read.csv("n_stat_data.csv") # criteria: adult males OL task, >1h retention (both short and long), controls >50%, novel_exp >0
dat <- dat %>% select(-contains("X"))
dat$id_uni <- as.factor(paste(dat$id, dat$lab_lit, sep = "_")) # check unique animals
identical(length(unique(dat$id_uni)), nrow(dat))


prior <- read_excel("prior_control_literature.xlsx")
prior$sd <- prior$sem * sqrt(prior$N)
prior$var <- prior$sd^2


# From MaB, hedges G = 0.57 for males/OIL/short&long/mice&rats/acrossELA models
# 0.57 is likely to be overestimated (problems with lit_meta + also rats)
# Considered 0.4 as "real effect size"
dat %>%
  select(control_or_ELS_animal, di) %>%
  group_by(control_or_ELS_animal) %>%
  summarize(m = mean(di), sd = sd(di), n = length(di)) %>%
  gather(key = out, value = amount, m, sd, n) %>%
  unite(var, control_or_ELS_animal, out, sep="_") %>%
  spread(key = var, value = amount) -> dat_sum

dat_sum <- metafor::escalc(m1i = P_m, sd1i = P_sd, n1i = P_n,
                           m2i = L_m, sd2i = L_sd, n2i = L_n,
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

# RePAIR on relacs --------------------------------------------------------
# Run sample_post function from sim_with_prior.Rmd
# Prepare dataset
dat_an <- dat_sum
names(dat_an) <- c("c_mean", "c_n", "c_sd",
                   "e_mean", "e_n", "e_sd") ##put at the beginning when files unblinded
dat_an$c_var <- dat_an$c_sd ^ 2
dat_an$e_var <- dat_an$e_sd ^ 2



# randomly remove control animals so that n_c = n_tot / 100 * 30 
n_rem <- ceiling((dat_an$c_n[1]+dat_an$e_n[1]) * 0.3)
rem <- sample(dat[dat$control_or_ELS_animal == "L",]$id_uni, n_rem)
dat$sel <- ifelse(dat$id_uni %in% rem, TRUE, FALSE)

# Summary stats relacs pilot and prior
dat %>%
  filter(control_or_ELS_animal == "L") %>%
  group_by(sel) %>%
  summarize(n = length(di), m = mean(di), v = var(di)) -> dat_sel
dat_sel <- as.data.frame(dat_sel)



# Prior parameters --------------------------------------------------------
## From relacs
par_c_rp <- find_prior_par(pilot_name = "relacs",
                           n_group = dat_sel[dat_sel$sel == TRUE,]$n, 
                           mean_group = dat_sel[dat_sel$sel == TRUE,]$m, 
                           s2_group = dat_sel[dat_sel$sel == TRUE,]$v) 

## From literature
lit_p_par <- data.frame(rbind(rep(0,6)))
names(lit_p_par) <- c("pilot_name", "mu0", "k0", "v0", "sigma0_2", "n0_cor")
  
for (i in 1:nrow(prior)) {
  
  post_par <- find_prior_par(pilot_name = "lit",
                             n_group    = prior$N[i],
                             mean_group = prior$mean[i],
                             s2_group   = prior$var[i],
                             belief     = prior$belief_02[i],
                             mu0        = as.numeric(lit_p_par$mu0[i]),
                             k0         = as.numeric(lit_p_par$k0[i]),
                             v0         = as.numeric(lit_p_par$v0[i]),
                             sigma0_2   = as.numeric(lit_p_par$sigma0_2[i]),
                             n0_cor     = as.numeric(lit_p_par$n0_cor[i])
    )
  
    lit_p_par <- rbind(lit_p_par, post_par)  

}

## From meta-analysis
mod <- metafor::rma(mean, sd^2, method = "FE", data = prior)

meta_par_c <- find_prior_par(pilot_name = "meta",
                             n_group = sum(prior$N * prior$belief_02), 
                             mean_group = round(mod$beta[1],2), 
                             s2_group = (sqrt(sum(prior$N * prior$belief_02))*round(mod$se,2))^2) 

prior_par <- data.frame(rbind(par_c_rp, lit_p_par[c(1,nrow(lit_p_par)),], meta_par_c))

# Relacs analysis ---------------------------------------------------
# Frequentist t-test
t.test(di ~ control_or_ELS_animal, data = dat)
t.test(di ~ control_or_ELS_animal, data = dat[dat$sel == FALSE,])

# Bayesian
## Experimental group
post_e <- sample_post_cor(n_group = dat_an$e_n[1], mean_group = dat_an$e_mean[1], 
                          s2_group = dat_an$e_var[1])

## put here control validation t-test
post_c_check <- sample_post_cor(n_group = dat_an$c_n[1], mean_group = dat_an$c_mean[1], 
                                s2_group = dat_an$c_var[1])

## Control group
post_c_all <- data.frame(matrix(nrow = 10000, ncol = nrow(prior_par)))
names(post_c_all) <- prior_par$pilot_name

for (my_col in levels(as.factor(prior_par$pilot_name))) {

  post_c_all[, my_col] <- sample_post_cor(n_group    = dat_sel[dat_sel$sel == FALSE,]$n, 
                                mean_group = dat_sel[dat_sel$sel == FALSE,]$m, 
                                s2_group   = dat_sel[dat_sel$sel == FALSE,]$v, 
                                mu0  = as.numeric(as.character(prior_par[prior_par$pilot_name == my_col,]$mu0)),
                                k0   = as.numeric(as.character(prior_par[prior_par$pilot_name == my_col,]$k0)),
                                v0   = as.numeric(as.character(prior_par[prior_par$pilot_name == my_col,]$v0)),
                                sigma0_2   = as.numeric(as.character(prior_par[prior_par$pilot_name == my_col,]$sigma0_2)))
  
}

post_c_all <- data.frame(cbind(post_c_check, post_c_all))
## bayesian
### run functions from repair below
post_diff <- post_e - post_c_all

t(apply(post_diff,2, quantile, probs = c(0.975)))



# Visualization results ---------------------------------------------------
post_c_all %>% 
  gather(key = prior, value = post_c) %>%
  bind_cols(post_e = rep(post_e, ncol(post_c_all))) %>%
  mutate(diff = post_e - post_c) %>%
  gather(key = group, value = post, -prior) %>%
  mutate(fac = ifelse(group == "diff", "Difference", "Groups")) -> post_graph
 
post_graph$prior <- as.factor(post_graph$prior)
post_graph$prior <- factor(post_graph$prior, levels = c("post_c_check", "X0", "relacs", "lit", "meta"))
post_graph %>%
  filter(fac == "Difference") %>%
  group_by(prior) %>%
  summarize(high = quantile(post, probs = 0.975), 
            low  = quantile(post, probs = 0.025)) -> diff_quant

post_graph %>%
   ggplot(aes(x = post, fill = group)) +
    geom_density() +
    facet_grid(prior~fct_rev(fac), scales = "free") +
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = diff_quant$high), linetype = 2, alpha = 0.5) +
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = diff_quant$low), linetype = 2, alpha = 0.5) +
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = 0, colour = my_bad)) +
    my_theme + 
    std_fill_dark +
    theme(legend.position = "none") -> res_rep_rel

saveRDS(res_rep_rel, "figures/repair_results.rds")



# Manual calculation p-value ----------------------------------------------

## from https://www.medcalc.org/calc/comparison_of_means.php
#I will consider
post_e
post_c_check





# Priors and prospective power --------------------------------------------
dat %>%
  group_by(contact, control_or_ELS_animal) %>%
  summarize(n = length(di)) %>%
  spread(key = control_or_ELS_animal, value = n) -> dat_group_sum

# Conventional prospective power according to our estimation Hedge's G = 0.4
power <- pwr.t2n.test(n1 = dat_group_sum$L, n2 = dat_group_sum$P, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power <- round(power$power,3)
median(dat_group_sum$power)

con_relacs <- sum(dat_group_sum$L)
con_lit <- sum(prior$N * prior$belief_02)
con_both <- con_relacs * 0.8 + con_lit

exp_relacs <- sum(dat_group_sum$P)


power <- pwr.t2n.test(n1 = con_relacs * 0.8, n2 = dat_group_sum$P, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power_p08 <- round(power$power,3)
power <- pwr.t2n.test(n1 = con_relacs, n2 = dat_group_sum$P, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power_p1 <- round(power$power,3)

power <- pwr.t2n.test(n1 = con_both, n2 = dat_group_sum$P, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power_rel_lit <- round(power$power,3)

power <- pwr.t2n.test(n1 = con_both, n2 = exp_relacs * 0.8, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power_all_08 <- round(power$power,3)
power <- pwr.t2n.test(n1 = con_both, n2 = exp_relacs, 
                      d = 0.4, sig.level = .05)
dat_group_sum$power_all_1 <- round(power$power,3)

write.csv(dat_group_sum, file = "power_relacs.csv")
