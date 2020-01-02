## Script: "Download your dependencies"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Dec. 23rd, 2019


# Environment preparation -------------------------------------------------
rm(list = ls())

# Install packages
## from CRAN
list_cran_packages <- c(
  "tidyverse", "readxl", # data handling
  "assertive", "docstring", # functions
  "pwr", "metafor", # for statistics
  "ggpubr", "viridis", "grid", "gtable" # graphs
)

new_packages <- list_cran_packages[!(list_cran_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

## from Github
if(!require(osfr)) remotes::install_github("centerforopenscience/osfr")





