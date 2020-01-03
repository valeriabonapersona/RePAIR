## The pretty figures file

# Environment -------------------------------------------------------------
source("src/RePAIR_functions.R")

# Figure 1 ----------------------------------------------------------------
# import individual images
pow_ach <- readRDS(paste0(path_fig_inter, "power_achieved.rds"))
theor_pow <- readRDS(paste0(path_fig_inter, "theor_pow_no.rds"))
total_n <- readRDS(paste0(path_fig_inter, "total_n.rds"))

# other
densMax <- readRDS(paste0(path_fig_inter, "Fig01_densMax.rds"))
countMax <- readRDS(paste0(path_fig_inter, "Fig01_countMax.rds"))

# various texts
theor_pow_text <- readRDS(paste0(path_fig_inter, "power_achieved_text.rds"))
total_n_text <- readRDS(paste0(path_fig_inter, "total_n_text.rds"))

# save figure 1 - attention: heavy on Rstudio
Fig_01 <- 
  ggarrange(
    ggarrange(pow_ach, total_n, ncol = 2, labels = c("A", "B")),
    
    theor_pow,
    
    nrow = 2, heights = c(3,5),
    labels = c(NA, "C")
  )


# Figure 2 ----------------------------------------------------------------
## Fig. 2-A
RePAIR_sim <- readRDS(paste0(path_fig_inter, "gt_RePAIR.rds"))
Fig_2A <- ggarrange(RePAIR_sim)

## Fig. 2-B
# various text
my_num_exp <- readRDS(paste0(path_fig_inter, "my_num_exp.RDS")) 
my_num_con <- readRDS(paste0(path_fig_inter, "my_num_con.RDS"))
my_num_prior <- readRDS(paste0(path_fig_inter, "my_num_prior.RDS"))
diff_quant <- readRDS(paste0(path_fig_inter, "diff_quant.RDS"))
# graph
Fig_2B <- readRDS(paste0(path_fig_inter, "repair_results.rds"))

## Fig. 2-C 
Fig_2C <- readRDS(paste0(path_fig_inter, "theor_pow_redi_03.rds"))

Fig_02 <- 
  ggarrange(
    ggarrange(Fig_2A, Fig_2B, ncol = 2, labels = c("A", "B")),
    
    Fig_2C,
    
    nrow = 2,
    labels = c(NA, "C")
  )


# Figure S2 ---------------------------------------------------------------
Fig_S2 <- readRDS(paste0(path_fig_inter,"supp_power_achieved_fields.rds"))


# Figure S3 ---------------------------------------------------------------
Fig_S3 <- readRDS(paste0(path_fig_inter,"supp_effect_size_range.rds"))

# Figures S4 --------------------------------------------------------------
# import images
pop_control <- readRDS(paste0(path_fig_inter, "pop_control.rds")) # fig A
pop_means <- readRDS(paste0(path_fig_inter, "pop_means_variation.rds")) #fig B
ex_distr <- readRDS(paste0(path_fig_inter, "ex_distr.rds")) #fig C
sensit <- readRDS(paste0(path_fig_inter, "sensitivity.rds")) #fig D

Fig_S4 <-  
  ggarrange(
    ggarrange(pop_control, pop_means, ex_distr, ncol = 3, labels = c("A", "B", "C"), widths = c(0.8,1.2,0.8)), 
    sensit,
    nrow = 2, heights = c(3,5),
    labels = c(NA, "D")
  )


# Figure S5 ---------------------------------------------------------------
Fig_S5 <- readRDS(paste0(path_fig_inter, "theor_pow_redi_1.rds"))

# Save --------------------------------------------------------------------
figures_list <- list(fig_01 = Fig_01, fig_02 = Fig_02, 
                     
                     fig_S2 = Fig_S2, fig_S3 = Fig_S3, 
                     fig_S4 = Fig_S4, fig_S5 = Fig_S5)

for (fig in names(figures_list)) {
 print(fig) 
  pdf(file = paste0(path_fig_final, fig, ".pdf"))
  print(figures_list[[fig]])
  dev.off()
}
