## The pretty figures file

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")


# Figure 1 ----------------------------------------------------------------

svg(filename = "figures/Figure_01.svg")

ggarrange(
  
  ggarrange(pow_ach, total_n, ncol = 2, labels = c("A", "B")),
  
  theor_pow,
  
  nrow = 2, heights = c(3,5),
  labels = c(NA, "C")
)

dev.off()



