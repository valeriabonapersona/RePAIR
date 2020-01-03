## Script: "RePAIR validation in a real-life dataset: RELACS"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019

# Environment -------------------------------------------------------------
source("src/RePAIR_functions.R")
set.seed(1238)

# Import datasets ---------------------------------------------------------
dat <- read.csv(paste0(path_data, "RELACS_anonymized_blinded.csv")) # for criteria, see Table S3
dat <- dat %>% select(-contains("X"))

prior <- read.csv(paste0(path_data, "RELACS_prior_control_literature.csv"))
prior$sd <- prior$sem * sqrt(prior$N)
prior$var <- prior$sd^2

# From MaBapp (https://osf.io/ra947/): 
# hedges G = 0.57 for males/OIL/short&long/mice&rats/acrossELA models
# since -0.57 is likely to be overestimated (problems with lit_meta + also rats),
# we considered -0.4 the correct effect size estimate

# remove blinding - analysis was conducted blinded to experimental condition
## e = early life adversity (experimental group), c = control
dat$control_or_ELA_animal <- recode(dat$control_or_ELA_animal, 
                                    P = "e", L = "c")


# di = discrimination, m = mean, sd = standard deviation, n = sample size
dat %>%
  select(control_or_ELA_animal, di) %>%
  group_by(control_or_ELA_animal) %>%
  summarize(m = mean(di), sd = sd(di), n = length(di)) %>%
  gather(key = out, value = amount, m, sd, n) %>%
  unite(var, control_or_ELA_animal, out, sep="_") %>%
  spread(key = var, value = amount) -> dat_sum

dat_sum <- metafor::escalc(m1i = e_m, sd1i = e_sd, n1i = e_n,
                           m2i = c_m, sd2i = c_sd, n2i = c_n,
                           measure = "SMD", data = dat_sum)

# Effect size estimated here is -.36


# Power calculation
# sample size calculation with estimated effect sizes
pow_cal <- expand.grid(
  "hedges"  = c(.36, .4, .57), #estimated Hedge's G from literature (.57 and .4) and RELACS (.36)
  "tail"     = c("two.sided", "one.sided")
) #all combinations
pow_cal$tail <- as.character(pow_cal$tail)
pow_cal$n_c <- ceiling(purrr::pmap_dbl(list(pow_cal$hedges, as.character(pow_cal$tail)), whats_nC, n_ratio = 1, sd_ratio = 1))
pow_cal$n_tot <- pow_cal$n_c*2

# is RELACS powere enough? 
## when aggregated: YES
dat %>%
  group_by(control_or_ELA_animal) %>%
  count() %>%
  spread(key = control_or_ELA_animal, value = n) %>%
  mutate(n_ratio = e / c) %>%
  mutate(pow = whats_pow(n_low = c, n_ratio = n_ratio))

# per single study: NO
dat %>%
  group_by(contact, control_or_ELA_animal) %>%
  count() %>%
  spread(key = control_or_ELA_animal, value = n) %>%
  mutate(min_n = min(c(c,e))) %>%
  mutate(max_n = max(c(c,e))) %>%
  mutate(n_ratio = max_n / min_n) %>%
  mutate(pow = whats_pow(n_low = min_n, n_ratio = n_ratio)) %>% 
  ungroup() %>% summarize(median(pow))


# RePAIR on RELACS --------------------------------------------------------
# Dataset preparation
dat_an <- dat_sum
dat_an$c_var <- dat_an$c_sd ^ 2 #_var = variance
dat_an$e_var <- dat_an$e_sd ^ 2

# randomly remove control animals so that n_c = n_tot / 100 * 30 
n_rem <- ceiling((dat_an$c_n[1]+dat_an$e_n[1]) * 0.3)
rem <- sample(dat[dat$control_or_ELA_animal == "c",]$unique_id, n_rem)
dat$sel <- ifelse(dat$unique_id %in% rem, TRUE, FALSE)

# Summary stats relacs pilot and prior
dat %>%
  filter(control_or_ELA_animal == "c") %>%
  group_by(sel) %>%
  summarize(n = length(di), m = mean(di), v = var(di)) -> dat_sel
dat_sel <- as.data.frame(dat_sel)


# Prior parameters --------------------------------------------------------
## From relacs
par_c_rp <- find_prior_par(n_exp = dat_sel[dat_sel$sel == TRUE,]$n, 
                           mean_exp = dat_sel[dat_sel$sel == TRUE,]$m, 
                           s2_exp = dat_sel[dat_sel$sel == TRUE,]$v, 
                           belief = 1) 
par_c_rp$pilot_name <- "relacs"

## From literature
lit_p_par <- find_multiple_prior_par(data_exp = prior, 
                        n_exp = "N", 
                        mean_exp = "mean", 
                        s2_exp = "var", 
                        belief = "index")

lit_p_par$pilot_name <- "literature_RePAIR"


no_prior <- data.frame(mu0 = 0, k0 = 0, 
                       v0 = 0, sigma0_2 = 0, 
                       n0_cor = 0, pilot_name = "no_prior")
prior_par <- data.frame(rbind(no_prior, par_c_rp, lit_p_par[nrow(lit_p_par),]))

# Relacs analysis ---------------------------------------------------
# Frequentist t-test
t.test(di ~ control_or_ELA_animal, data = dat)
t.test(di ~ control_or_ELA_animal, data = dat[dat$sel == FALSE,])


# Bayesian
## Experimental group
post_par_e <- find_post_par(n_exp = dat_an$e_n[1], mean_exp = dat_an$e_m[1], 
                            s2_exp= dat_an$e_var[1], belief = 1)
post_e <- sample_post(data_par = post_par_e)

## put here control validation t-test
post_par_c <- find_post_par(n_exp = dat_an$c_n[1], mean_exp = dat_an$c_m[1], 
                            s2_exp= dat_an$c_var[1], belief = 1)
post_c_check <- sample_post(data_par = post_par_c)


## Control group
post_c_all <- data.frame(matrix(nrow = 10000, ncol = nrow(prior_par)))
names(post_c_all) <- prior_par$pilot_name

for (my_col in levels(as.factor(prior_par$pilot_name))) {
  
  post_par_c_all <- find_post_par(n_exp = dat_sel[dat_sel$sel == FALSE,]$n,
                                  mean_exp = dat_sel[dat_sel$sel == FALSE,]$m, 
                                  s2_exp = dat_sel[dat_sel$sel == FALSE,]$v, 
                                  belief = 1,
                                  data_par = prior_par[prior_par$pilot_name == my_col,])
  post_c_all[,my_col] <- sample_post(post_par_c_all)
  
}

post_c_all <- data.frame(cbind(post_c_check, post_c_all))

## bayesian
### run functions from repair below
post_diff <- post_e - post_c_all

t(apply(post_diff,2, quantile, probs = c(0.975)))



# Visualization results - Fig. 2-B ---------------------------------------------------
post_c_all %>% 
  gather(key = prior, value = post_c) %>%
  bind_cols(post_e = rep(post_e, ncol(post_c_all))) %>%
  mutate(diff = post_e - post_c) %>%
  gather(key = group, value = post, -prior) %>%
  mutate(fac = ifelse(group == "diff", "Difference", "Groups")) -> post_graph
 
post_graph$prior <- as.factor(post_graph$prior)
post_graph$prior <- factor(post_graph$prior, 
                           levels = c("post_c_check", "no_prior", 
                                      "relacs", "literature_RePAIR"))
levels(post_graph$prior) <- c("t-test", "no prior", "prior from relacs", 
                              "literature prior RePAIR")

my_num_exp <- paste0("Nexp=", dat_an$e_n)
saveRDS(my_num_exp, paste0(path_fig_inter, "my_num_exp.RDS"))

my_num_con <- c(paste0("Ncon=", dat_an$c_n),
                paste0("Ncon=", dat_sel[dat_sel$sel == FALSE,]$n),
                paste0("Ncon=", dat_sel[dat_sel$sel == FALSE,]$n),
                paste0("Ncon=", dat_sel[dat_sel$sel == FALSE,]$n)
                )
saveRDS(my_num_con, paste0(path_fig_inter, "my_num_con.RDS"))

my_num_prior <- c(rep("Nprior=0", 2), 
                  paste0("Nprior=", dat_sel[dat_sel$sel == TRUE,]$n),
                  paste0("Nprior=", sum(prior$N * prior$belief_02))
                  )
saveRDS(my_num_prior, paste0(path_fig_inter, "my_num_prior.RDS"))

 
post_graph %>%
  filter(fac == "Difference") %>%
  group_by(prior) %>%
  summarize(high = quantile(post, probs = 0.975), 
            low  = quantile(post, probs = 0.025)) -> diff_quant

saveRDS(diff_quant, paste0(path_fig_inter, "diff_quant.RDS"))


post_graph %>%
   ggplot(aes(x = post, fill = group)) +
    geom_density() +
    ylab("Density") + xlab("Discrimination Index") +
    facet_grid(prior~fct_rev(fac), scales = "free", labeller = label_wrap_gen(30)) +
    #geom_text() +
  geom_text(aes(x, y, label=my_num_exp), 
            colour = "black", size=2.4,
            data = data.frame(x = 0.64, y = 30, 
                             fac = "Groups",
                             group = "diff"),vjust=0, hjust = 0) +
  geom_text(aes(x, y, label=my_num_con), 
            colour = "black", size=2.4,
            data = data.frame(x = 0.64, y = 25, 
                              fac = "Groups",
                              group = "diff"),vjust=0, hjust = 0) +
  geom_text(aes(x, y, label=my_num_prior), 
            colour = "black", size=2.4,
            data = data.frame(x = 0.64, y = 20, 
                              fac = "Groups",
                              group = "diff"),vjust=0, hjust = 0) +

    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = diff_quant$high), linetype = 2, alpha = 0.5) +
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = diff_quant$low), linetype = 2, alpha = 0.5) +
    geom_vline(data = data.frame(fac="Difference", x=0),
               mapping = aes(xintercept = 0, colour = my_bad)) +
    my_theme +  
    std_fill_dark +
    theme(legend.position  = c(0.1,0.1),
          legend.title.align = 0.5,
          legend.background = element_rect(color = "black"),
          strip.background = element_rect(fill = NA),
          strip.text = element_text(colour = "black")) +
  guides(colour = FALSE, 
         fill = guide_legend(direction = "vertical")) -> res_rep_rel # Fig. 2-B

saveRDS(res_rep_rel, paste0(path_fig_inter, "repair_results.rds"))


# Priors and prospective power - Table S5 --------------------------------------------
# Sample size summary per RELACS experiment
dat %>%
  group_by(contact, control_or_ELA_animal) %>%
  summarize(n = length(di)) %>%
  spread(key = control_or_ELA_animal, value = n) -> dat_group_sum

# Change in prospective power for each of the RELACS experiments
# when considering different types of prior
## calculate effective N in various priors
con_relacs <- NULL # control group
exp_relacs <- NULL # experimental group

## for each experiment in RELACS
  for (exp in c(1:nrow(dat_group_sum))) {
    print(exp)
    con_relacs[exp] <- sum(dat_group_sum$c) - dat_group_sum$c[exp]
    exp_relacs[exp] <- sum(dat_group_sum$e) - dat_group_sum$e[exp]
    
    } # other experiments as prior

con_lit <- sum(prior$N * prior$index) # literature as prior
con_both <- con_relacs * 0.8 + con_lit # other experiments and literature as prior


prior_type <- data.frame(
  
  type  = as.factor(c(rep(c("no_prior","RELACS_0.8","RELACS_1", "RELACS_0.8_lit", 
            "RELACS_0.8_lit_exp_0.8"), each = nrow(dat_group_sum)))),
  n_c_p = c(rep(0, nrow(dat_group_sum)), con_relacs * 0.8, con_relacs, rep(con_both,2)),
  n_e_p = c(rep(0,4*nrow(dat_group_sum)), exp_relacs * 0.8)
  
)

# calculate prospective power for all conditions
for (lev in levels(prior_type$type)) {
  print(lev)
  power <- pwr.t2n.test(n1 = dat_group_sum$c + prior_type[prior_type$type == lev,]$n_c_p, 
                        n2 = dat_group_sum$e + prior_type[prior_type$type == lev,]$n_e_p,
                        d = 0.4, sig.level = .05)
  dat_group_sum[lev] <- paste0(round(power$power,3) * 100, "%")
  
  
}

print(paste("The median prospective power for each experiment in the RELACS dataset is", median(dat_group_sum$no_prior)))

write.csv(dat_group_sum, file = paste0(path_other_output, "power_relacs.csv"))
