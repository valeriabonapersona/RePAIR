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



# Figure 2 ----------------------------------------------------------------
svg(filename = "figures/RePAIR_sim.svg")

  ggarrange(
    
    top_left, 
    top_right,
    bottom_left,
    bottom_right,
    
    ncol = 2, nrow = 2, 
    heights = c(1,2), widths = c(1,4)
    
  )

dev.off()


