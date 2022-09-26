library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# start from script 2, i.e. after webscraping
# note that we no longer use here() for relative file paths
# but write that in directly with forward slash
# files are located relative to the file _targets.R 
# which has to be in the main working directory outside of the scripts folder

library(targets)
source("scripts/functions.R") 

tar_option_set(packages = c("readr", "dplyr", "ggplot2", "naniar"))

# targets pipeline - eventually modularise this by modelling stage
list(
  # note that we skip create targets for web scraping - retain script 1
  # not sure how targets() interacts with json/web environments, try later
  #tar_target(file, "processed/agimpacts_full.csv", format = "file"),
  #tar_target(CGIAR_data, get_data_from_csv(file)),
  #tar_target(CGIAR_cleaned, remove_na(CGIAR_data)),
  # note that we skip targets for the manual data validation - retain script 2
  # because it is just too hard to rewrite...for now
  tar_target(ag, "processed/agimpacts_final.csv", format = "file"),
  tar_target(agimpacts, get_data_from_csv(ag))
  # note we need to convert some variables from chr to factor later
  # work with Monfreda data FIRST and THEN make all changes to agimpacts in one function
  
)

# note these commands must be commented out or will be recursive
# since file _targets.R runs all code in this script automatically

# check pipeline & dependencies
#tar_manifest()
#tar_visnetwork()

#  make outputs!
# tar_make() 

# inspect output
#tar_read(CGIAR_data)
