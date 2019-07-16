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

# svg(filename = "figures/supp_power_achieved_fields.svg")
# pow_ach_fields
# dev.off()


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
  mutate(eff_size = c(rep(0.2, nrow(dat)),
                      rep(0.5, nrow(dat)),
                      rep(0.9, nrow(dat))))

pow <- pwr.t2n.test(n1 = dat_theor$n_1,
                    n2 = dat_theor$n_2,
                    d  = dat_theor$eff_size,
                    sig.level = .05)
dat_theor$theor_pow <- pow$power



# theoretical power visualization
theor_pow <- 
  ggplot(dat_theor, aes(x = theor_pow, fill = as.factor(eff_size))) + 
  geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=0, ymax=900),
            fill= my_bad) +
  geom_rect(mapping=aes(xmin=0.8, xmax=1, ymin=0, ymax=900),
            fill= my_good) +
  geom_histogram(colour = "black", bins = 100) +
  ylab("Number of papers") + 
  xlab("Estimated power before experiment") + 
  facet_grid(eff_size ~.) +
  # geom_text(aes(x, y, label=lab, colour = "red"),
  #           data=data.frame(x=0.9, y=750, 
  #                           lab=c("0%", "1.65%", "12.5%"),
  #                           eff_size=c("0.2 small", "0.5 medium", 
  #                                      "0.9 large")), 
  #           vjust=1) +
  # geom_text(aes(x, y, label=lab, colour = "green"),
  #           data=data.frame(x=0.4, y=750,
  #                           lab=c("99.7%", "93.5%", "61.9%"),
  #                           eff_size=c("0.2 small", "0.5 medium", 
  #                                      "0.9 large")), vjust=1) +
  my_theme + 
  std_fill_dark +
  theme(legend.position  = "none",
        strip.background = element_rect(fill = NA),
        strip.text = element_text(colour = "white", face = "bold"))
theor_pow



## Add in some colors based on the data
dat_theor$facet_fill_color <- c("my_purple_light", "my_watergreen_light", "my_yellow_light")[as.factor(dat_theor$eff_size)]

## Create main plot
theor_pow

dummy <- theor_pow
dummy$layers <- NULL
dummy <- dummy + 
  # geom_rect(data=dat_theor, xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
  #                          aes(fill = eff_size))
  geom_rect(data = dat_theor, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, aes(fill = facet_fill_color))

dummy

library(gtable)

g1 <- ggplotGrob(theor_pow)
g2 <- ggplotGrob(dummy)


panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip-right", g2$layout$name)
g2$grobs[strips] <- replicate(sum(strips), nullGrob(), simplify = FALSE)
g2$layout$l[panels] <- g2$layout$l[panels] + 1
g2$layout$r[panels] <- g2$layout$r[panels] + 2

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)


## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.draw(new_plot)
