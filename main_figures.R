## The pretty figures file

# Environment -------------------------------------------------------------
source("RePAIR_functions.R")

# Figure 1 ----------------------------------------------------------------
# import individual images
pow_ach <- readRDS("figures/power_achieved.rds")
theor_pow <- readRDS("figures/theor_pow_no.rds")
total_n <- readRDS("figures/total_n.rds")

# other
densMax <- readRDS("figures/Fig01_densMax.rds")
countMax <- readRDS("figures/Fig01_countMax.rds")

# various texts
theor_pow_text <- readRDS("figures/power_achieved_text.rds")
total_n_text <- readRDS("figures/total_n_text.rds")


Fig_01 <-
  ggarrange(
    ggarrange(pow_ach, total_n, ncol = 2, labels = c("A", "B")),
    
    theor_pow,
    
    nrow = 2, heights = c(3,5),
    labels = c(NA, "C")
  )


# Figure 2 ----------------------------------------------------------------
## Fig. 2-A
RePAIR_sim <- readRDS("figures/gt_RePAIR.rds")
Fig_2A <- ggarrange(RePAIR_sim)

## Fig. 2-B
my_num_exp <- readRDS("figures/my_num_exp.RDS") # various text
my_num_con <- readRDS("figures/my_num_con.RDS")
my_num_prior <- readRDS("figures/my_num_prior.RDS")
diff_quant <- readRDS("figures/diff_quant.RDS") 

Fig_2B <- readRDS("figures/repair_results.rds") # graph

## Fig. 2-C 
Fig_2C <- readRDS("figures/theor_pow_redi_03.rds")

pdf(file = "figures/fig_2.pdf", width = 15, height = 13)
ggarrange(
  ggarrange(Fig_2A, Fig_2B, ncol = 2, labels = c("A", "B")),
  
  Fig_2C,
  
  nrow = 2,
  labels = c(NA, "C")
)
dev.off()


# Figure S2 ---------------------------------------------------------------
Fig_S2 <- readRDS("figures/supp_power_achieved_fields.rds")

# Figure S3 ---------------------------------------------------------------
Fig_S3 <- readRDS("figures/supp_effect_size_range.rds")

# Figures S4 --------------------------------------------------------------
# import images
pop_control <- readRDS("figures/pop_control.rds") # fig A
pop_means <- readRDS("pop_means_variation.rds") #fig B
ex_distr <- readRDS("figures/ex_distr.rds") #fig C
sensit <- readRDS("figures/sensitivity.rds") #fig D

Fig_S4 <- 
  ggarrange(
    ggarrange(pop_control, pop_means, ex_distr, ncol = 3, labels = c("A", "B", "C"), widths = c(0.8,1.2,0.8)), 
    sensit,
    nrow = 2, heights = c(3,5),
    labels = c(NA, "D")
  )

# Figure S5 ---------------------------------------------------------------
Fig_S5 <- readRDS("figures/theor_pow_redi_1.rds")


# Save --------------------------------------------------------------------
figures_list <- list(fig_01 = Fig_01, fig_S2 = Fig_S2,# fig_S3 = Fig_S3, 
                     fig_S4 = Fig_S4, fig_S5 = Fig_S5)

for (fig in names(figures_list)) {
 print(fig) 
  pdf(file = paste0("figures/", fig, ".pdf"))
  print(figures_list[[fig]])
  dev.off()
}
