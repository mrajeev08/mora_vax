# Lessons learned from pilot rabies dog vaccination campaigns in Moramanga District, Madagascar

Data, code, and paper comparing pilot vaccination campaigns in Moramanga District, Madagascar

## Repository structure

All data are in data/ and data-raw/ although note that not all cost and location data have been shared.

All analyses and functions are in R/. To run all the analyses, see [R/run_analyses_all.R](R/run_analyses_all.R)

All figures and tables are in figs/ and analysis outputs are in out/.

The manuscript is written in R Markdown and knit to an MDPI formatted pdf using the rticles::mdpi_article output.

This project uses `renv` to manage package dependencies. To install these dependencies in a project specific library, clone this repository to your local computer, and then use the following command in R:

```
renv::restore()
```