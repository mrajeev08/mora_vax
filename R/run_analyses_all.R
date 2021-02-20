# Main script to run all analyses

# helper function to source files with invisible output
run_script <- function(path) {
  source(here::here(path), local = environment())
  return(paste(path, "is done"))
}

# First run models
run_script("R/run_dem_mod.R") # will take a few minutes!
run_script("R/run_vacc_mod.R")

# Then make the figures & tables
run_script("R/figure2.R")
run_script("R/Table1.R")
run_script("R/figure3.R")
run_script("R/figure4.R")

# Make the mdpi manuscript
rmarkdown::render("paper/mdpi_paper.Rmd", output_format = "rticles::mdpi_article")
