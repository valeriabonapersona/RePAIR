## Script: "Variation in prior selection due to random sampling"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019

# Environment -------------------------------------------------------------
source("src/RePAIR_functions.R")
set.seed(8709)

## first download datasets with download_data.R

# Import datasets ---------------------------------------------------------
relacs <- read.csv(paste0(path_data, "RELACS_anonymized_blinded.csv")) # criteria: adult males OL task, >1h retention (both short and long), controls >50%, novel_exp >0
relacs <- relacs %>% select(-contains("X"))
prior <- read.csv(paste0(path_data, "RELACS_prior_control_literature.csv"))

# Preparation datasets
prior$sd <- prior$sem * sqrt(prior$N)
prior$var <- prior$sd^2
prior$prior <- "lit"

# remove blinding - analysis was conducted blinded to experimental condition
## e = early life adversity (experimental group), c = control
relacs$control_or_ELA_animal <- recode(relacs$control_or_ELA_animal, 
                                       P = "e", L = "c")

## Merge
prior$contact <- as.factor(paste(prior$author, prior$year, sep = "_"))
relacs %>% 
  filter(control_or_ELA_animal == "c") %>%
  group_by(contact) %>%
  summarize(mean = mean(di), 
            N = length(di)) %>%
  mutate(prior = "relacs") -> all

all <- rbind(all, prior %>% select("contact", "mean", "N", "prior"))

# Variation control groups - Fig. S4-A------------------------------------------------
## estimation mean of population mean from literature and relacs
est_mean <- 
  all %>% 
  group_by(prior) %>% 
  summarize(est_mean = mean(mean),
            N = sum(N)) 

pop_control <-
  all %>% 
  ggplot(aes(x = mean, fill = TRUE)) +
  xlim(0.4,.8)+ 
  xlab("Discrimination") + ylab("Population mean distribution") +
  geom_vline(xintercept = mean(all$mean), colour = "red") +
  facet_grid(prior ~.) +
  geom_text(aes(x, y, label=lab, colour = "red"),
            data = data.frame(x = 0.7, y = 12,
                              lab = round(est_mean$est_mean,3),
                              prior = est_mean$prior),vjust=1) +
  geom_density() + 
  my_theme + 
  std_fill + 
  theme(legend.position = "none")
  

saveRDS(pop_control, file = paste0(path_fig_inter, "pop_control.rds"))


# Simulation estimation pop mean ------------------------------------------
## How many experiments would I need to sufficidently approximate the population mean?
all$mean <- ((all$mean - mean(all$mean)) / sd(all$mean)) # standardization

sim_means <- as.data.frame(cbind(rep(2:17, each = 10000), NA, NA))
names(sim_means) <- c("number", "ave", "N")

for (many in c(1:nrow(sim_means))) {
  selected <- sample.int(17, sim_means$number[many]) # sampled on the index, the lower the index the least likely you are to select this experiment
  sim_means$ave[many] <- mean(all$mean[selected])
  sim_means$N[many] <- sum(all$N[selected])

}

# Save
#write.csv(sim_means, paste0(path_data_temp, "sim_means_controls.csv"))

# Reupload results (if for later session)
#sim_means <- read.csv(paste0(path_data_temp,"sim_means_controls.csv"))


## variation mean by number of papers selected - Fig. S4-B
sim_means %>%
  filter(number != 17) %>%
  group_by(number) %>%
  summarize("0.25"  = quantile(ave, probs = 0.25), 
            "0.75"  = quantile(ave, probs = 0.75)) %>%
  gather(key = type, value = quant, -number) -> sim_means_quant


sim_means_quant %>%
  arrange(number) %>%
  arrange(-quant) %>%
  ggplot(aes(x = quant, y = number)) +
  geom_point() +
  geom_vline(xintercept = 0, colour = "grey") + 
  geom_polygon(fill = viridis(option = "C",n=1), alpha = 0.2) +
  my_theme + 
  scale_x_continuous(breaks = c(-0.5,-0.3,-0.1,0,0.1,0.3,0.5)) +
  scale_y_continuous(breaks = c(2,5,10,15)) +
  xlab(expression(paste("Variation from ", mu["con"], italic(" (Hedge's G)")))) +
  ylab("Number of studies sampled") -> sensitivity

saveRDS(sensitivity, file = paste0(path_fig_inter, "pop_means_variation.rds")) # Fig S4-B


## variation mean by N selected (by selecting papers)
## ranges selected by keeping length similar
sim_means$N_group <- ifelse(sim_means$N %in% c(16:27), "20",
                            ifelse(sim_means$N %in% c(47:53), "50",
                                   ifelse(sim_means$N %in% c(97:103), "100",
                                          ifelse(sim_means$N %in% c(196:204), "200",
                                                 "NA"))))

sim_means %>%
  filter(N_group != "NA") %>%
  mutate(N_group = as.numeric(as.character(N_group))) %>%
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

write.csv(limits_means, paste0(path_data_temp, "limits_means.csv"))


# Fig. S4-C
ex_dist <- 
  ggplot(NULL, aes(c(-5,10))) +
    xlab("mean value outcome") +
    geom_area(stat = "function", fun = dnorm, args = list(mean = -1, sd = 1), fill = my_yellow, xlim = c(-4, 10)) +
    geom_area(stat = "function", fun = dnorm, args = list(mean =  3, sd = 1), fill = my_watergreen_light, xlim = c(-4, 10)) +
    geom_area(stat = "function", fun = dnorm, args = list(mean =  5, sd = 1), fill = my_watergreen, xlim = c(-4, 10)) +
    geom_area(stat = "function", fun = dnorm, args = list(mean =  7, sd = 1), fill = my_watergreen_light, xlim = c(-4, 10)) +
    geom_segment(aes(y= 0.4, yend=0.4, x=5, xend=3), 
                 size = 0.5, colour = viridis(option = "A", n=10)[7],
                 arrow = arrow(length = unit(0.1, "inches"))) +
    geom_segment(aes(y=0.4, yend=0.4, x=5, xend=7), 
                 size = 0.5, colour = viridis(5)[4],
                 arrow = arrow(length = unit(0.1, "inches"))) +
    my_theme + 
    theme(#axis.title.x = element_blank(),
          axis.title.y = element_blank(),
         # axis.text.x = element_blank(),
          axis.text.y=element_blank(),
        #  axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())
saveRDS(ex_dist, file = paste0(path_fig_inter, "ex_distr.rds"))
