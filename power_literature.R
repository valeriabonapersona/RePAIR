## Script: "Statistical power in preclinical (rodent) literature"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")

# make sure you have downloaded relevant datasets!
# see download_data.R 

# Import datasets ---------------------------------------------------------
dat <- read.csv("meta_n.csv") # corresponds to "Data A" in manuscript (Fig. S1)
meta <- read.csv("meta_effectsize.csv") # corresponds to "Data B" in manuscript (Fig. S2)


# Achieved power (Fig. 1-A, Fig. S2) ----------------------------------------------------------
# Calculate power achieved for each effect size
power <- pwr.t2n.test(n1 = meta$n_1, n2 = meta$n_2, 
                      d = meta$yi, sig.level = .05)
meta$power <- power$power
pow_ach_text <- paste0("Median: ", round(median(meta$power) * 100),"%")
saveRDS(pow_ach_text, "figures/power_achieved_text.rds")

# Visualization
pow_ach <- 
  ggplot(meta, aes(x = power, fill = TRUE)) + 
  geom_histogram(colour = "black", bins = my_bin) +
  xlab("Power achieved") + ylab("Number of experiments") +
  geom_vline(xintercept = median(meta$power), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill +
  scale_x_continuous(breaks = c(0,0.2,0.5,0.8,1.0), 
                     labels = c("0%", "20%", "50%", "80%", "100%")) +
  theme(legend.position = "none")

## Main text (Fig. 1-A)
pow_ach_main <- pow_ach +  
  annotate("text", x=0.25, y=500, label=pow_ach_text, color = my_watergreen, fontface = "bold", hjust = 0)
saveRDS(pow_ach_main, "figures/power_achieved.rds")

## Supplementary (Fig. S2)
pow_ach_fields <- pow_ach + 
  facet_grid(~study)
saveRDS(pow_ach_fields, "figures/supp_power_achieved_fields.rds")

# Animals used (Fig. 1-B) -----------------------------------------------------------
median(dat$n_t)
total_n_text <- paste0("Median ", median(dat$n_t)/2,"/group")
saveRDS(total_n_text, "figures/total_n_text.rds")

total_n <- ggplot(dat, aes(x = n_t, fill = TRUE)) +
  geom_histogram(colour = "black", bins = my_bin) +
 # ylim(0,310) +
  xlab(expression(paste("Animals used ", italic("(log scale)")))) + ylab("Number of publications") +
  scale_x_continuous(trans = "log10",
                     breaks = c(0,10,20,50,150,500)) +
  geom_vline(xintercept = median(dat$n_t), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  annotate("text", x=25, y=249, label=total_n_text, color = my_watergreen, fontface = "bold", hjust = 0) +
  my_theme + std_fill + 
  theme(legend.position = "none")

saveRDS(total_n, file = "figures/total_n.rds")


# Estimation effect size range --------------------------------------------
# Range effect sizes
all_es <- (floor(quantile(abs(meta$yi), probs = c(0.25,0.5,0.75))*10))/10

# Visualization (Fig. S3)
effect_sizes <- 
  ggplot(meta, aes(x = abs(yi), fill = TRUE)) + 
  geom_histogram(colour = "black", bins = my_bin) +
  xlab(expression(paste("Hedge's G ", italic("(log scale)")))) + ylab("Number of publications") +
  ylab("Number of effect sizes") +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,0.2,0.5,0.9,1.5,3,7, 20)) +
  geom_vline(xintercept = as.numeric(all_es), color = c(my_purple, my_watergreen, my_yellow), 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill +
  theme(legend.position = "none")

saveRDS(effect_sizes, "figures/supp_effect_size_range.rds")


# Theoretical power with prior --------------------------------------------
## Preparation dataset
# for each effect size
dat_theor <- dat %>% 
  bind_rows(dat,dat) %>%
  mutate(eff_size = rep(all_es, each = nrow(dat)))

# for each prior effect size
priors <- c("no", # no prior
            "redi_1", # redistributed resources, index = 1
            "redi_03" # redistributed resources, index = 0l3
            )
dat_theor <- dat_theor %>% 
  bind_rows(dat_theor, dat_theor) %>%
  mutate(prior_type = rep(priors, each = nrow(dat_theor))) %>%
  mutate(n_2 = ifelse(prior_type %in% c("redi_1", "redi_03"), 
                      n_t / 3 * 2, n_2)) %>%
  mutate(n_1 = ifelse(prior_type %in% c("redi_1", "redi_03"), 
                      n_t / 3, n_1)) %>%
 group_by(study, prior_type, eff_size) %>%
  mutate(
    prior_n = case_when(
      prior_type == "no" ~ 0,
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

## Visualization (Fig. 1-C, Fig. 2-C, Fig. S5)
# Find maximum value of density
densMax <- dat_theor %>% 
  filter(prior_type == "no") %>%
  group_by(eff_size) %>%
  summarise(dens = max(density(theor_pow)[["y"]])) %>%
  filter(dens == max(dens))
saveRDS(densMax, file = "figures/Fig01_densMax.rds")

# Find maximum value of bin count
countMax <- dat_theor %>% 
  filter(prior_type == "no") %>%
  group_by(eff_size, 
           bins=cut(theor_pow, seq(floor(min(theor_pow)),
                                   ceiling(max(theor_pow)), 
                                   my_bin_width), right = TRUE)) %>%
  summarise(count=n()) %>% 
  ungroup() %>% filter(count==max(count))

saveRDS(countMax, file = "figures/Fig01_countMax.rds")

for (each in levels(factor(dat_theor$prior_type))) {
  
  # Percentages distributions
  dat_theor %>%
    filter(prior_type == each) %>%
    group_by(eff_size) %>%
    summarize(low_per = sum((theor_pow <= 0.5)) /nrow(dat) * 100,
              high_per = sum((theor_pow >= 0.8))/ nrow(dat) * 100) -> per_power
  
  my_graph <- dat_theor %>%
      filter(prior_type == each) %>%
      ggplot(aes(x = theor_pow, fill = factor(eff_size), 
                 sf = countMax$count/densMax$dens)) + 
        geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=0, ymax=Inf),
                  fill= my_bad) +
        geom_rect(mapping=aes(xmin=0.8, xmax=1, ymin=0, ymax=Inf),
                  fill= my_good) +
        geom_density(data = dat_theor[dat_theor$prior_type == "no",],
                     aes(y = ..density.. * sf), fill = "grey", alpha = 0.5) +  
        geom_histogram(colour = "black", binwidth = my_bin_width, position = "identity") +
        
        facet_grid(eff_size ~., scales = "free") +
    
        scale_x_continuous(breaks = c(0,0.2,0.5,0.8,1.0), 
                           labels = c("0%", "20%", "50%", "80%", "100%")) +
        ylab("Number of publications") + 
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
        scale_fill_manual(values = c(my_yellow, my_watergreen, my_purple)) + 
        labs(tag = "Hedge's G") +
        theme(legend.position  = "none",
              strip.background = element_rect(fill = NA),
              strip.text = element_text(colour = "black", face = "bold"),
              plot.tag.position = c(1.02, 0.535), 
              plot.tag = element_text(angle = -90, size = 10),
              plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
        coord_cartesian(clip = "off")
      

# Save
 saveRDS(my_graph, file = paste0("figures/theor_pow_", each,".rds"))
  

}

