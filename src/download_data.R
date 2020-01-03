## Script: "Download relevant datasets automatically from osf"

## accompanying manuscript: 
## "RePAIR: a power solution to animal experimentation"

## Author: Valeria Bonapersona
## contact: v.bonapersona-2 (at) umcutrecht.nl

## Last update: Oct. 28th, 2019



# Environment -------------------------------------------------------------
source("src/RePAIR_functions.R")

# Download datasets from osf -----------------------------------------------------------------
# OSF connection
project <- osf_retrieve_node("wvs7m")

# Download
datasets <- c("meta_n.csv", "meta_effectsize.csv", 
              "RELACS_anonymized_blinded.csv", "RELACS_prior_control_literature.csv")

for (d in datasets) {
  print(d)
  project %>%
    osf_ls_files(n_max = 20) %>%
    filter(name == d) %>%
    osf_download(path = path_data, conflicts = TRUE) 
}
