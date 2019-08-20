## The pretty figures file

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")


# Figure 1 ----------------------------------------------------------------
# import individual images
pow_ach <- readRDS("figures/power_achieved.rds")
theor_pow <- readRDS("figures/theoretical_power.rds")
total_n <- readRDS("figures/total_n.rds")

svg(filename = "figures/Figure_01.svg")
ggarrange(
  
  ggarrange(pow_ach, total_n, ncol = 2, labels = c("A", "B")),
  
  theor_pow,
  
  nrow = 2, heights = c(3,5),
  labels = c(NA, "C")
)

dev.off()



# Figure 2 ----------------------------------------------------------------
RePAIR_sim <- readRDS("figures/RePAIR_sim.rds")
theor_pow_prior <- readRDS("figures/theor_pow_prior.rds") ##countMax needs to be saved
RePAIR_res <- readRDS("figures/repair_results.rds")

svg(filename = "figures/Figure_02.svg")

ggarrange(
  RePAIR_sim,
  
  ggarrange(theor_pow_prior, RePAIR_res, ncol = 2, labels = c("B", "C"), widths = c(1.5,1)),
  
  nrow = 2, 
  labels = c("A", NA)
)

dev.off()

