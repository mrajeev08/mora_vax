# Lessons learned from pilot rabies dog vaccination campaigns in Moramanga District, Madagascar

Data, code, and manuscript files accompanying:

Filla C, Rajeev M, Randriana Z, et al. Lessons learned and paths forward for rabies dog vaccination in Madagascar: a case study of pilot vaccination campaigns in Moramanga District. *Accepted in Tropical Medicine and Infectious Diseases.*

## Abstract
Canine rabies causes an estimated 60,000 human deaths per year, but these 
deaths are preventable through post-exposure prophylaxis of people and 
vaccination of domestic dogs. Dog vaccination campaigns targeting 70% of 
the population are effective at interrupting transmission. Here, we report on 
lessons learned during pilot dog vaccination campaigns in the Moramanga 
District of Madagascar. We compare two different vaccination strategies: 
a volunteer driven effort to vaccinate dogs in two communes using static 
point vaccination, and continuous vaccination as part of routine veterinary 
services. We used dog age data from the campaigns to estimate key demographic 
parameters and to simulate different vaccination strategies. Overall, 
we found that dog vaccination was feasible and that most dogs were accessible to vaccination. 
The static-point campaign achieved higher coverage, but required more 
resources and had a limited geographic scope compared to the continuous 
delivery campaign. Our modeling results suggest that targeting puppies 
through community-based vaccination efforts could improve coverage. We 
found that mass dog vaccination is feasible and can achieve high coverage 
in Madagascar, however context-specific strategies and an investment in dog 
vaccination as a public good will be required to move the country towards 
elimination. 

## Repository structure

All processed data are in [data/](data), and raw geospatial data and the processing script are in[data-raw/](data-raw), although note that not all cost data have been shared. All analyses and functions are in [R/](R). 

All figures and tables are in figs/ and analysis outputs are in out/.

The manuscript is written in R Markdown and knit to an MDPI formatted pdf using the rticles::mdpi_article (currently a fork at [mrajeev08/rticles@c2a0819](https://github.com/mrajeev08/rticles/tree/c2a0819482d11f77d488205ae70b62ea793fe4a1), as the template is out of date) output.

This project uses `renv` to manage package dependencies. To install these dependencies in a project specific library, clone or download this repository, and then use the following command in R:

```
renv::restore()
```

To run all the analyses, from within the cloned repository, run [R/run_analyses_all.R](R/run_analyses_all.R), i.e. in R:
```
source("R/run_analyses_all.R")
```
