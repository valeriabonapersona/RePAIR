## The power file

# Environment -------------------------------------------------------------
rm(list = ls())
source("RePAIR_functions.R")


# Download datasets from osf -----------------------------------------------------------------
# OSF connection
#OSF_PAT <- "my_token_name" ##CHANGE THIS BEFORE IT BECOMES PUBLIC
# OSF_PAT <- "BAcJNLrL90DqkfNJUBMOZuKieUmg3WTVhltI6xE2Pk3BnCl6ALSq1kvVSK7y7kWsSmaBAy"
# 
# osf_auth(OSF_PAT)
# project <- osf_retrieve_node("wvs7m")
# 
# datasets <- c("meta_n.csv", "meta_effectsie.csv")
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
                      d = meta$yi, sig.level = .05)
meta$power <- power$power
median(meta$power)

# Visualization
pow_ach <- 
  ggplot(meta, aes(x = power, fill = TRUE)) + 
  geom_histogram(colour = "black", bins = 50) +
  ylim(0,610) +
  xlab("Power achieved") + ylab("Number of experiments") +
  geom_vline(xintercept = median(meta$power), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill + 
  theme(legend.position = "none")


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

svg(filename = "figures/supp_power_achieved_fields.svg")
pow_ach_fields
dev.off()


# Animals used -----------------------------------------------------------
median(dat$n_t)

total_n <- ggplot(dat, aes(x = n_t, fill = TRUE)) +
  geom_histogram(colour = "black", bins = 50) +
  xlab("Animals used (log scale)") + ylab("Number of publications") +
  scale_x_continuous(trans = "log10",
                     breaks = c(0,10,20,50,150,500)) +
  geom_vline(xintercept = median(dat$n_t), color = my_watergreen, 
             linetype = "dashed", size = 1.5) +
  my_theme + std_fill + 
  theme(legend.position = "none")


# Estimation effect size range --------------------------------------------
# Effect size achieved
all_es <- (floor(quantile(abs(meta$yi), probs = c(0.25,0.5,0.75))*10))/10

effect_sizes <- 
  ggplot(meta, aes(x = abs(yi), fill = TRUE)) + 
  geom_histogram(colour = "black", bins = 50) +
  xlab("Animals used (log scale)") + ylab("Number of publications") +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,0.2,0.5,0.9,1.5,3,7, 20)) +
  geom_vline(xintercept = as.numeric(all_es), color = c(my_purple, my_watergreen, my_yellow), 
             linetype = "dashed", size = 1.5) +
  my_theme + 
  std_fill +
  theme(legend.position = "none")


svg(filename = "figures/supp_effectsizes_achieved.svg")
  effect_sizes
dev.off()


# Theoretical power -------------------------------------------------------


