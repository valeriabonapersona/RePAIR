## The RePAIR prior

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")
set.seed(8709)


# # Download datasets from osf -------------------------------------------------------
# OSF connection
#OSF_PAT <- "my_token_name" ##CHANGE THIS BEFORE IT BECOMES PUBLIC
#OSF_PAT <- "BAcJNLrL90DqkfNJUBMOZuKieUmg3WTVhltI6xE2Pk3BnCl6ALSq1kvVSK7y7kWsSmaBAy"

# osf_auth(OSF_PAT)
# project <- osf_retrieve_node("xj6yb")
# 
# # Download
# project %>%
#   osf_ls_files(n_max = 20) %>%
#   filter(name == "n_stat_data.csv") %>%
#   osf_download(overwrite = TRUE)
# 
# project %>%
#   osf_ls_files(n_max = 20) %>%
#   filter(name == "prior_control_literature.xlsx") %>%
#   osf_download(overwrite = TRUE)




# Import datasets ---------------------------------------------------------
relacs <- read.csv("n_stat_data.csv") # criteria: adult males OL task, >1h retention (both short and long), controls >50%, novel_exp >0
relacs <- relacs %>% select(-contains("X"))
prior <- read_excel("prior_control_literature.xlsx")

# Preparation datasets
prior$sd <- prior$sem * sqrt(prior$N)
prior$var <- prior$sd^2
prior$prior <- "lit"

## Merge
prior$contact <- as.factor(paste(prior$author, prior$year, sep = "_"))
relacs %>% filter(control_or_ELS_animal == "L") %>%
  group_by(contact) %>%
  summarize(mean = mean(di), 
            N = length(di)) %>%
  mutate(prior = "relacs") -> all
all <- rbind(all, prior %>% select("contact", "mean", "N", "prior"))
all %>% 
   mutate(belief = ifelse(!contact %in% 
                            all[duplicated(all$contact),]$contact, 1, 0.5),
          N_cor = belief * N) -> all
# Variation control groups ------------------------------------------------
est_mean <- 
  all %>% 
  group_by(prior) %>% 
  summarize(est_mean = mean(mean),
            N_cor = sum(N_cor), 
            N = sum(N)) 
                 
pop_control <-
  all %>% 
  ggplot(aes(x = mean, fill = TRUE)) +
  xlim(0,1)+ 
  xlab("Discrimination Index") + ylab("Density") +
  geom_vline(xintercept = mean(all$mean), colour = "red") +
  facet_grid(prior ~.) +
  geom_text(aes(x, y, label=lab, colour = "red"),
            data = data.frame(x = 0.7, y = 12,
                              lab = round(est_mean$est_mean,3),
                              prior = est_mean$prior),vjust=1) +
  ggtitle("Mean population estimation") +
  geom_density() + 
  my_theme + 
  std_fill + 
  theme(legend.position = "none")
  

# svg(filename = "figures/supp_pop_control.svg")
# pop_control
# dev.off()


# Simulation estimation pop mean ------------------------------------------
## How many experiments would I need to approximate the population mean?
all$mean <- ((all$mean - mean(all$mean)) / sd(all$mean)) #tryout standardization


## How many papers to I need to have the mean between .61 and .63?
sim_means <- as.data.frame(cbind(rep(2:14, each = 10000), NA, NA, NA))
names(sim_means) <- c("number", "ave", "N", "N_cor")
for (many in c(1:nrow(sim_means))) {
  selected <- sample.int(14, sim_means$number[many])
  sim_means$ave[many] <- mean(all$mean[selected])
  sim_means$N[many] <- sum(all$N[selected])
  sim_means$N_cor[many] <- sum(all$N_cor[selected])
  
}

#write.csv(sim_means, "sim_means_controls.csv")
sim_means <- read.csv("sim_means_controls.csv")
plot(sim_means$N ~ sim_means$number)
plot(sim_means$N_cor ~ sim_means$number)

## variation mean by number of papers selected
sim_means %>%
  filter(number != 14) %>%
  group_by(number) %>%
  summarize("0.025" = quantile(ave, probs = 0.025), 
            "0.25"  = quantile(ave, probs = 0.25), 
            "0.5"   = quantile(ave, probs = 0.5), 
            "0.75"  = quantile(ave, probs = 0.75), 
            "0.975" = quantile(ave, probs = 0.975)) %>%
  gather(key = type, value = quant, -number) -> sim_means_quant


sim_means_quant$type_cor <- ifelse(sim_means_quant$type %in% c("0.025", "0.975"),
                                   "quantile", 
                                   ifelse(sim_means_quant$type == "0.5", "median", "quartile"))


sim_means_quant %>%
  filter(type_cor == "quartile") %>%
  arrange(number) %>%
  arrange(-quant) %>%
  ggplot(aes(x = quant, y = number)) +
  geom_point() +
  geom_vline(xintercept = 0, colour = "grey") + 
  geom_vline(xintercept = c(-0.5, 0.5), colour = my_purple, linetype="dashed") + 
  geom_vline(xintercept = c(-0.3, 0.3), colour = my_watergreen, linetype="dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), colour = my_yellow, linetype="dashed") +
  # geom_text(label = "0.5", aes(y = 14, x = 0.55), colour = a) + 
  # geom_text(label = "-0.5", aes(y = 14, x = -0.55), colour = a) + 
  # geom_text(label = "0.3", aes(y = 14, x = 0.35), colour = b) + 
  # geom_text(label = "-0.3", aes(y = 14, x = -0.35), colour = b) +     
  # geom_text(label = "0.1", aes(y = 14, x = 0.15), colour = c) + 
  # geom_text(label = "-0.1", aes(y = 14, x = -0.15), colour = c) + 
  geom_polygon(fill = viridis(option = "C",n=1), alpha = 0.2) +
  my_theme + 
  scale_x_continuous(breaks = c(-0.5,-0.3,-0.1,0,0.1,0.3,0.5)) +
  xlab("Variation from real control population mean") +
  ylab("Number of studies") +
  geom_segment(aes(y= 10, yend=10, x=0, xend=-0.5), 
               size = 0.5, colour = viridis(option = "A", n=10)[7],
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_segment(aes(y=10, yend=10, x=0, xend=0.5), 
               size = 0.5, colour = viridis(5)[4],
               arrow = arrow(length = unit(0.1, "inches"))) -> sensitivity

# svg(filename = "figures/supp_sensitivity.svg")
# sensitivity
# dev.off()


## variation mean by N selected (by selecting papers)
sim_means$N_group <- ifelse(sim_means$N %in% c(9:11), "10", 
                            ifelse(sim_means$N %in% c(17:25), "20",
                                   ifelse(sim_means$N %in% c(47:53), "50",
                                          ifelse(sim_means$N %in% c(97:103), "100",
                                                 ifelse(sim_means$N %in% c(196:204), "200",
                                                        "NA")))))

sim_means %>%
  filter(N_group != "NA") %>%
  mutate(N_group = as.numeric(N_group)) %>%
  group_by(N_group) %>%
  summarize("how_many" = length(ave),
            "0.025"    = quantile(ave, probs = 0.025),
            "0.25"     = quantile(ave, probs = 0.25),
            "0.5"      = quantile(ave, probs = 0.5),
            "0.75"     = quantile(ave, probs = 0.75),
            "0.975"    = quantile(ave, probs = 0.975)) %>%
  gather(key = type, value = quant, -c(N_group, how_many)) -> sens_based_N



## for sensitivity analysis
sim_means_quant %>%
  filter(type %in% c("0.25", "0.75")) %>%
  mutate(quant_round = round(quant,1)) %>%
  select(number, type, quant_round) %>%
  spread(key = type, value = quant_round) %>%
  mutate(number_anim = number * 10) -> limits_means

# write.csv(limits_means, "limits_means.csv")
