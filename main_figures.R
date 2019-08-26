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
theor_pow_prior <- readRDS("figures/theor_pow_redi_03.rds") ##countMax needs to be saved
RePAIR_res <- readRDS("figures/repair_results.rds")

svg(filename = "figures/Figure_02.svg")

ggarrange(
  RePAIR_sim,
  
  ggarrange(RePAIR_res,theor_pow_prior, ncol = 2, labels = c("B", "C"), widths = c(1,1.5)),
  
  nrow = 2, 
  labels = c("A", NA)
) 

dev.off()


# Figures S5 --------------------------------------------------------------
# import images
pop_control <- readRDS("figures/pop_control.rds") # fig A
pop_means <- readRDS("pop_means_variation.rds") #fig B
ex_distr <- readRDS("figures/ex_distr.rds") #fig C
sensit <- readRDS("figures/sensitivity.rds") #fig D


svg(filename = "figures/Fig_S5.svg")
ggarrange(
  pop_control,
  ex_distr, 
  pop_means, 
  sensit, 
  #nrow = 2, 
  heights = c(1, 1.5), widths = c(1,1.5),
  labels = c("A", "C", "B", "D")
)
dev.off()

# Figure S6 ---------------------------------------------------------------
# import individual images
t_pow_1 <- readRDS("figures/theor_pow_rean_1.rds")
t_pow_03 <- readRDS("figures/theor_pow_rean_03.rds")

svg(filename = "figures/Fig_S6.svg")

ggarrange(
  t_pow_03,
  t_pow_1,
  nrow = 2, 
  labels = c("A", "B")
)

dev.off()