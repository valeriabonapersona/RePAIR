## Script: "Functions"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019



# Environment preparation -------------------------------------------------
rm(list = ls())

# Library
my_library <- c(
  "osfr", "readxl", # data handling
  "assertive", "docstring", # functions
  "pwr", "metafor", # for statistics
  "ggpubr", "viridis", "grid", "gtable" # graphs,
  )

library(tidyverse)
lapply(my_library, library, character.only = TRUE)


# Paths -------------------------------------------------------------------
path_data <- "data/raw/"


# Power calculation-related functions -----------------------------------------------
# outputs n smallest group (control)
whats_nC <- function(delta, sd_ratio, n_ratio = 1, alt = "two.sided") { ##you can substitute the args of other functions as ...
  
  #' @title Calculate sample size control group
  #' @description Wrapper function to power_t_test() function from MESS package. 
  #' This function extracts the calculated sample size by power_t_test() when considering 
  #' a power of 0.8 and an alpha value of 0.05. The method of estimation is "welch".
  #' 
  #' 
  #' @param delta Measure of effect size, corresponds to *delta* in power_t_test().
  #' @param sd_ratio Ratio of standard deviation between control and experimental group.
  #' It corresponds to sd.ratio in power_t_test().
  #' @param n_ratio Ratio of sample size between the control and the experimental group. 
  #' It corresponds to ratio in power_t_test(). Default value is 1, meaning equal sample sizes.
  #' @param alt Type of hypothesis to test. Corresponds to alternative in power_t_test(). 
  #' Default value is two sided test ("two.sided")
  
  n_nec <- MESS::power_t_test(power = 0.8,
                              delta = delta,
                              ratio = n_ratio,
                              sd = 1,
                              sd.ratio = sd_ratio,
                              alternative = alt,
                              sig.level = .05,
                              df.method = "welch")
  n_nec$n[1]
  
}

whats_pow <- function(n_low, n_ratio) {
  
  #' @title Calculate prospective power
  #' @description Wrapper function to power_t_test() function from MESS package. 
  #' This function extracts the power achieved given the characteristics of choice.
  #' As default, the test in two-sided with Welch correction. The effect size considered is
  #' 0.4, with equal standard deviation across groups. 
  #' 
  #' @param n_low Sample size of one of the two groups. Corresponds to n in power_t_test().
  #' @param n_ratio Ratio of sample size between the control and the experimental group. 

  x <- MESS::power_t_test(n = n_low,
                          delta = 0.4,
                          ratio = n_ratio,
                          sd = 1,
                          sd.ratio = 1,
                          alternative = "two.sided",
                          sig.level = .05,
                          df.method = "welch")
  return(x$power)
}




# RePAIR functions ----------------------------------------------------------------
## Of note: in this script, we name variables as Gelman (1995) section 3.3 and 3.4.
## For simplicity, in the related manuscript, k and v are simplified as n_prior
## since in this particular application k = n_prior and v = n_prior

## Prior parmeters

find_post_par <- function(n_exp, mean_exp, s2_exp, belief, data_par = NULL) {
  
  #' @title Find posterior parameters
  #' @description This function takes summary statistics information of an 
  #' experimental group, and the degree of similarity (belief, number [0,1]) 
  #' that this experiment has with the one to be conducted. 
  #' With this information, the function outputs
  #' posterior parameters according to Gelman's equations (1995, section 3.3 and 3.4).
  #' 
  #' 
  #' @param n_exp Sample size of the group of interest.
  #' @param mean_exp Mean of the group of interest.
  #' @param s2_exp Variance of the group of interest.
  #' @param belief Degree of similarity with the currently conducted experiment. 
  #' It must be a number between 0 and 1.
  #' @param data_par Parameters to be used instead of a uninformative prior. 
  #' data_par must have the same structure as find_post_par output. 
  #' @return Table with posterior paramenters. mu1 = posterior mu, 
  #' k1 and v1 = degrees of freedom, sigma1_2 = posterior variance, 
  #' n1_cor = weight of the experiment based on sample size and degree of similarity).

  
  # Checks 
  ## New variables
  assert_is_a_number(n_exp)
  assert_is_a_number(mean_exp)
  assert_is_a_number(s2_exp)
  assert_is_a_number(belief)
  
  ## Prior parameters
  my_prior_par <- c("mu0", "k0", "v0", "sigma0_2", "n0_cor")
  
  if (is.null(data_par)) {

    mu0 <- 0
    k0 <- 0
    v0 <- 0
    sigma0_2 <- 0
    n0_cor <- 0
    
  } else if (!is.data.frame(data_par)) {
    
    stop("Data_par is not a dataframe")
    
  } else if (any(!my_prior_par %in% names(data_par))) {
    
    stop("The variable names of data_par are not consistent with outputs of find_prior_par() /n
         For manual specification of prior parameters, create data_par dataframe with parameters: /n
         mu0, k0, v0, sigma0_2, n0_cor")
    
  } else {
    ##write function
    mu0      <- data_par[nrow(data_par), "mu0"]
    k0       <- data_par[nrow(data_par), "k0"]
    v0       <- data_par[nrow(data_par), "v0"]
    sigma0_2 <- data_par[nrow(data_par), "sigma0_2"]
    n0_cor   <- data_par[nrow(data_par), "n0_cor"]
    
  }
  
  # Calculate parameters
  ## exp indicates current experiment, 0 prior, 1 posterior
  ## equations from Gelman (195) Ch. 3
  n_exp_cor <- n_exp * belief
  
  mu1 <- ((k0/(k0 + n_exp_cor)) * mu0) + 
    ((n_exp_cor/(k0 + n_exp_cor)) * mean_exp)
  k1 <- k0 + n_exp_cor
  v1 <- v0 + n_exp_cor
  
  sigma1_2 <- 
    (v0*sigma0_2 + (n_exp_cor - 1) * s2_exp + 
       ((k0 * n_exp_cor/(k0 + n_exp_cor)) * ((mean_exp - mu0)^2))
    ) / v1
  
  n1_cor <- n0_cor + n_exp_cor
  
  # Organize
  data.frame(mu1, k1, v1, sigma1_2, n1_cor)
  
}

find_prior_par <- function(n_exp, mean_exp, s2_exp, belief, data_par = NULL) {
  
  #' @title Find prior parameters
  #' @description Wrapper function to find_post_par(). 
  #' This function takes summary statistics information of an 
  #' experimental group, the degree of similarity (belief, number [0,1]) 
  #' that this experiment has with the one to be conducted.
  #' With this information, the function outputs
  #' prior parameters that can be used for analysis of future experiments.
  #' 
  #' 
  #' @param n_exp Sample size of the group of interest.
  #' @param mean_exp Mean of the group of interest.
  #' @param s2_exp Variance of the group of interest.
  #' @param belief Degree of similarity with the currently conducted experiment. 
  #' It must be a number between 0 and 1.
  #' @param data_par Parameters to be used instead of a uninformative prior. 
  #' data_par must have the same structure as find_post_par output. 
  #' @return Table with posterior paramenters. mu0 = prior mu, 
  #' k0 and v0 = degrees of freedom, sigma0_2 = prior variance, 
  #' n0_cor = weight of the experiment based on prior sample size and 
  #' degree of similarity).
  
  
  
  out_par <- find_post_par(n_exp = n_exp, 
                           mean_exp = mean_exp, 
                           s2_exp = s2_exp, belief = belief, data_par = data_par)
  names(out_par) <- c("mu0", "k0", "v0", "sigma0_2", "n0_cor")
  return(out_par)
}

find_multiple_prior_par <- function(data_exp, n_exp, mean_exp, s2_exp, belief, data_par = NULL) {
  
  #' @title Find prior parameters with multiple experiments
  #' @description Wrapper function to find_prior_par(). 
  #' This function iterates the action of find_prior_par() by performing sequentially
  #' the same operation for all added experiments. Specifically, the posterior parameters
  #' calculated after experiment 1 will become the prior parameters of experiment 2.
  #' The posterior parameters calculated after experiment 2 will become the prior parameters
  #' of experiment 3 - and so forth. The results are independent of the order in which the
  #' experiments are added. 
  #' It takes data organized in a dataset with the following information: sample size,
  #' mean, variance, degree of similarity.
  #' 
  #' 
  #' @param data_exp Name of the dataset.
  #' @param n_exp Variable of dataset containing sample size of the group of interest.
  #' @param mean_exp Variable of dataset containing mean of the group of interest.
  #' @param s2_exp Variable of dataset containing variance of the group of interest.
  #' @param belief Variable of dataset containing degree of similarity with the currently conducted experiment. 
  #' It must be a number between 0 and 1.
  #' @param data_par Parameters to be used instead of a uninformative prior. 
  #' data_par must have the same structure as find_post_par output. 
  #' @return Table with posterior paramenters. mu0 = prior mu, 
  #' k0 and v0 = degrees of freedom, sigma0_2 = prior variance, 
  #' n0_cor = weight of the experiment based on prior sample size and 
  #' degree of similarity). Each row corresponds to variation in prior parameters
  #' as a new experiment is added in the iterative process. The last row corresponds
  #' to the prior parameters when all provided experiments are added.
  
  
  
  
  # Checks
  ##
  assert_is_data.frame(data_exp)
  
  my_data <- data.frame(data_exp[,c(n_exp, mean_exp, s2_exp, belief)])
  names(my_data) <- c("n_exp", "mean_exp", "s2_exp", "belief")
  
  assert_all_are_not_na(my_data)
  
  out_par <- data.frame(t(rep(0,5)))
  names(out_par) <- c("mu0", "k0", "v0", "sigma0_2", "n0_cor")
  for (i in c(1:nrow(my_data))) {
    inter_par <- find_prior_par(n_exp = my_data[i, "n_exp"],
             mean_exp = my_data[i, "mean_exp"],
             s2_exp = my_data[i, "s2_exp"], 
             belief = my_data[i, "belief"],
             data_par = out_par[nrow(out_par),])
    
    out_par <- rbind(out_par, inter_par)
  }
   return(out_par[-1,])
}



sample_post <- function(data_par, n_sampled = 10000) {
  
  
  #' @title Sample values from posterior distribution
  #' @description This function can be used to sample values from the distribution
  #' provided. The distribution is defined by parameters, which can be calculated
  #' with the function find_post_par(). It follows Gelman's equations (1995, section 3.3 and 3.4).
  #' 
  #' 
  #' @param data_exp Output of find_post_par() function. It contains the parameters of
  #' the distribution from which to sample.
  #' @param n_sampled Number of values to sample. Default is 10000.
  #' @return String with n values (specified by n_sampled) following the given distribution.
  
  ## Prior parameters
  my_post_par <- c("mu1", "k1", "v1", "sigma1_2")
  
  if (!is.data.frame(data_par)) {
    
    stop("Data_par is not a dataframe")
    
  } else if (any(!my_post_par %in% names(data_par))) {
    
    stop("The variable names of data_par are not consistent with outputs of find_post_par() /n
         For manual specification of prior parameters, create data_par dataframe with parameters: /n
         mu1, k1, v1, sigma1_2, n1_cor")
    
  } else {
    ##write function
    mu1      <- data_par[nrow(data_par), "mu1"]
    k1       <- data_par[nrow(data_par), "k1"]
    v1       <- data_par[nrow(data_par), "v1"]
    sigma1_2 <- data_par[nrow(data_par), "sigma1_2"]
    n1_cor   <- data_par[nrow(data_par), "n1_cor"]
    
  }
  
 
  # Sampling
  sigma_post <- asbio::rinvchisq(n     = n_sampled, # amount values to draw 
                                 df    = v1,  
                                 scale = sigma1_2)
  
  mu_post    <- rep(NA, n_sampled)
  
  for (i in 1:n_sampled) {   
    
    mu_post[i]    <- rnorm(n = 1, 
                           mean = mu1, 
                           sd = sqrt(sigma_post[i] / k1)) 
    
  } 
  
  return(mu_post)
}
  


# Graphics ----------------------------------------------------------------
my_theme <- theme_classic() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t = 20, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(hjust = 0.95),
        axis.title.y = element_text(hjust = 0.95),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 


std_fill <- scale_fill_viridis(discrete = TRUE, alpha = 0.2)
std_fill_dark <- scale_fill_viridis(discrete = TRUE)
my_purple <- viridis(5, alpha = 0.8)[1]
my_watergreen <- viridis(5, alpha = 0.8)[3]
my_yellow <- viridis(5, alpha = 0.8)[5]
my_purple_light <- viridis(5, alpha = 0.01)[1]
my_watergreen_light <- viridis(5, alpha = 0.2)[3]
my_yellow_light <- viridis(5, alpha = 0.2)[5]
my_bad <- "#f0d9d0"
my_good <- "#d0f0da"
my_bin <- 50
my_bin_width <- my_bin / 5000

