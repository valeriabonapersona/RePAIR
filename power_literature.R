## The power file

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")


# Download datasets from osf -----------------------------------------------------------------
# OSF connection
#OSF_PAT <- "my_token_name" ##CHANGE THIS BEFORE IT BECOMES PUBLIC
# OSF_PAT <- "BAcJNLrL90DqkfNJUBMOZuKieUmg3WTVhltI6xE2Pk3BnCl6ALSq1kvVSK7y7kWsSmaBAy"
# 
# osf_auth(OSF_PAT)
# project <- osf_retrieve_node("wvs7m")
# 
# datasets <- c("meta_n.csv", "meta_effectsize.csv")
# 
# # Download
# for (d in datasets) {
#   print(d)
#   project %>%
#     osf_ls_files(n_max = 20) %>%
#     filter(name == d) %>%
#     osf_download(overwrite = TRUE)
#   
# }

# Import datasets ---------------------------------------------------------
dat <- read.csv("meta_n.csv")
meta <- read.csv("meta_effectsize.csv")


# Achieved power ----------------------------------------------------------
# Calculate power achieved for each effect size
power <- pwr.t2n.test(n1 = meta$n_1, n2 = meta$n_2, 
                      d = meta$yi, sig.level = .05) ## ask Herbert
meta$power <- power$power
median(meta$power)

# Visualization
pow_ach <- 
  ggplot(meta, aes(x = power, fill = TRUE)) + 
  geom_histogram(colour = "black", bins = 50) +
  ylim(0,630) +
  xlab("Power achieved") + ylab("Number of experiments") +
  geom_vline(xintercept = median(meta$power), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill + 
  theme(legend.position = "none")

saveRDS(pow_ach, "figures/power_achieved.rds")

pow_ach_fields <- ggplot(meta, aes(x = power, fill = TRUE)) + 
  geom_histogram(colour = "black", bins = 50) +
  ylim(0,360) +
  xlab("Power achieved") + ylab("Number of experiments") +
  facet_grid(~study) +
  ggtitle("Achieved power") +
  geom_vline(xintercept = median(meta$power), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill +  
  theme(legend.position = "none")

# svg(filename = "figures/supp_power_achieved_fields.svg")
# pow_ach_fields
# dev.off()


# Animals used -----------------------------------------------------------
median(dat$n_t)

total_n <- ggplot(dat, aes(x = n_t, fill = TRUE)) +
  geom_histogram(colour = "black", bins = 50) +
  ylim(0,310) +
  xlab("Animals used (log scale)") + ylab("Number of publications") +
  scale_x_continuous(trans = "log10",
                     breaks = c(0,10,20,50,150,500)) +
  geom_vline(xintercept = median(dat$n_t), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + std_fill + 
  theme(legend.position = "none")

saveRDS(total_n, file = "figures/total_n.rds")

# Estimation effect size range --------------------------------------------
# Effect size achieved
all_es <- (floor(quantile(abs(meta$yi), probs = c(0.25,0.5,0.75))*10))/10
## see other file for other method to calculate it

# Visualization
effect_sizes <- 
  ggplot(meta, aes(x = abs(yi), fill = TRUE)) + 
  geom_histogram(colour = "black", bins = 50) +
  xlab("Hedge's G") + ylab("Number of publications") +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,0.2,0.5,0.9,1.5,3,7, 20)) +
  geom_vline(xintercept = as.numeric(all_es), color = c(my_purple, my_watergreen, my_yellow), 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill +
  theme(legend.position = "none")

# svg(filename = "figures/supp_effectsizes_achieved.svg")
#   effect_sizes
# dev.off()

# Theoretical power -------------------------------------------------------
# Calculation
dat_theor <- dat %>% 
  bind_rows(dat,dat) %>%
  mutate(eff_size = rep(all_es, each = nrow(dat)))

pow <- pwr.t2n.test(n1 = dat_theor$n_1,
                    n2 = dat_theor$n_2,
                    d  = dat_theor$eff_size,
                    sig.level = .05)
dat_theor$theor_pow <- pow$power

## Theoretical power visualization
# Find maximum value of density
densMax <- dat_theor %>% 
  group_by(eff_size) %>%
  summarise(dens = max(density(theor_pow)[["y"]])) %>%
  filter(dens == max(dens))

# Find maximum value of bin count
countMax <- dat_theor %>% 
  group_by(eff_size, 
           bins=cut(theor_pow, seq(floor(min(theor_pow)),
                                      ceiling(max(theor_pow)), 
                                      0.01), right=FALSE)) %>%
  summarise(count=n()) %>% 
  ungroup() %>% filter(count==max(count))


theor_pow <- 
  ggplot(dat_theor, aes(x = theor_pow, fill = as.factor(eff_size), sf = countMax$count/densMax$dens)) + 
  geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=0, ymax=Inf),
            fill= my_bad) +
  geom_rect(mapping=aes(xmin=0.8, xmax=1, ymin=0, ymax=Inf),
            fill= my_good) +
 # geom_histogram(colour = "black", bins = 100) +
  geom_density(aes(y=..density.. * sf), alpha = 0.5) +
    
  geom_histogram(colour="black", binwidth = 0.01) +
  ylab("Number of papers") + 
  xlab("Estimated power before experiment") + 
  facet_grid(eff_size ~., scales = "free") +
  geom_text(aes(x, y, label=lab, colour = "red"),
            data = data.frame(x = 0.9, y = Inf,
                              lab = paste0(round(per_power$high_per,1),"%"),
                              eff_size = all_es),vjust=1) +
  geom_text(aes(x, y, label=lab, colour = "green"),
    data=data.frame(x=0.4, y=Inf,
                    lab = paste0(round(per_power$low_per,1),"%"),
                    eff_size = all_es),vjust=1) +
  my_theme + 
  std_fill_dark +
  labs(tag = "Hedge's G") +
  theme(legend.position  = "none",
        strip.background = element_rect(fill = NA),
        strip.text = element_text(colour = "black", face = "bold"),
        plot.tag.position = c(1.02, 0.535), 
        plot.tag = element_text(angle = -90, size = 10),
        plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
  coord_cartesian(clip = "off")
theor_pow

saveRDS(theor_pow, file = "figures/theoretical_power.rds")


# Theoretical power with prior --------------------------------------------
## Preparation dataset
# for each effect size
dat_theor <- dat %>% 
 # filter(!study %in% c("metabolism", "neuroscience")) %>% ##keep or remove?
  bind_rows(dat,dat) %>%
  mutate(eff_size = rep(all_es, each = nrow(dat)))

# for each prior effect size
priors <- c("no", "rean_1", "rean_03", "redi_1", "redi_03")
dat_theor <- dat_theor %>% 
  bind_rows(dat_theor, dat_theor, dat_theor, dat_theor) %>%
  mutate(prior_type = rep(priors, each = nrow(dat_theor))) %>%
  mutate(n_2 = ifelse(prior_type %in% c("redi_1", "redi_03"), 
                      n_t / 3 * 2, n_2)) %>%
  mutate(n_1 = ifelse(prior_type %in% c("redi_1", "redi_03"), 
                      n_t / 3, n_1)) %>%
 group_by(study, prior_type, eff_size) %>%
  mutate(
    prior_n = case_when(
      prior_type == "no" ~ 0,
      prior_type == "rean_1" ~ sum(n_1) - n_1,
      prior_type == "rean_03" ~ sum(n_1) * 0.3 - n_1,
      prior_type == "redi_1" ~ sum(n_t),
      prior_type == "redi_03" ~ sum(n_t) * 0.3,
    )
  ) %>% 
  ungroup() %>%
  mutate(n_1_tot = n_1 + prior_n)
  

pow <- pwr.t2n.test(n1 = dat_theor$n_1_tot,
                    n2 = dat_theor$n_2,
                    d  = dat_theor$eff_size,
                    sig.level = .05)
dat_theor$theor_pow <- pow$power

## Visualization
# Find maximum value of density
densMax <- dat_theor %>% 
  filter(prior_type == "no") %>%
  group_by(eff_size) %>%
  summarise(dens = max(density(theor_pow)[["y"]])) %>%
  filter(dens == max(dens))

# Find maximum value of bin count
countMax <- dat_theor %>% 
  filter(prior_type == "no") %>%
  group_by(eff_size, 
           bins=cut(theor_pow, seq(floor(min(theor_pow)),
                                   ceiling(max(theor_pow)), 
                                   0.01), right=FALSE)) %>%
  summarise(count=n()) %>% 
  ungroup() %>% filter(count==max(count))


for (each in levels(factor(dat_theor$prior_type))) {
  print(each)
  # Percentages distributions
  dat_theor %>%
    filter(prior_type == each) %>%
    group_by(eff_size) %>%
    summarize(low_per = sum((theor_pow <= 0.5)) /nrow(dat) * 100,
              high_per = sum((theor_pow >= 0.8))/ nrow(dat) * 100) -> per_power
  
my_graph <- dat_theor %>%
    filter(prior_type == each) %>%
  ggplot(aes(x = theor_pow, fill = factor(eff_size), sf = countMax$count/densMax$dens)) + 
    geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=0, ymax=Inf),
              fill= my_bad) +
    geom_rect(mapping=aes(xmin=0.8, xmax=1, ymin=0, ymax=Inf),
              fill= my_good) +
    geom_density(data = dat_theor[dat_theor$prior_type == "no",],aes(y = ..density.. * sf), fill = "grey", alpha = 0.5) +  
    geom_histogram(colour = "black", binwidth = 0.01, position = "identity") +
    
    facet_grid(eff_size ~., scales = "free") +
    scale_y_continuous() +
    ylab("Number of papers") + 
    xlab("Prospective power") + 
    
    geom_text(aes(x, y, label=lab, colour = "red"),
              data = data.frame(x = 0.9, y = Inf,
                                lab = paste0(round(per_power$high_per,1),"%"),
                                eff_size = all_es),vjust=1) +
    geom_text(aes(x, y, label=lab, colour = "green"),
              data=data.frame(x=0.4, y=Inf,
                              lab = paste0(round(per_power$low_per,1),"%"),
                              eff_size = all_es),vjust=1) +
    
    my_theme + 
    std_fill_dark +
    labs(tag = "Hedge's G") +
    theme(legend.position  = "none",
          strip.background = element_rect(fill = NA),
          strip.text = element_text(colour = "black", face = "bold"),
          plot.tag.position = c(1.02, 0.535), 
          plot.tag = element_text(angle = -90, size = 10),
          plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
    scale_alpha_discrete(range = c(0.2,0.8)) + 
    coord_cartesian(clip = "off")
  

# Save
 saveRDS(my_graph, file = paste0("figures/theor_pow_", each,".rds"))
  

}
#  readRDS("figures/theor_pow_rean_03.rds") -> a
# 
# svg(filename = paste0("figures/supp_theor_pow_rean_03.svg"))
# a
# dev.off()

