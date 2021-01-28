# Get demographic estimates for a set of bootstrapped data
library(foreach)
library(doParallel)
library(iterators)
library(dplyr)
library(readr)
library(here)
library(tidyr)
source("R/dem_models.R")
source("R/vacc_models.R")

dem_ests <- read_csv("out/dem_ests.csv")

# Sim vaccination with parameters where pop is growing -------------------------
dem_ests %>%
  ungroup() %>%
  filter(pop_growth == "Growing") %>%
  select(fert_est, psurv_adult_est, growth, prop_est, age_class, sim) %>%
  pivot_wider(values_from = prop_est, names_from = age_class) %>%
  select(-sim) %>%
  distinct() -> dem_ests_all

# sample 1000 of these par values
set.seed(1222)
dem_ests_sim <- dem_ests_all[sample(nrow(dem_ests_all), 1000), ]

# Average # of pups born to a dog each year = 4.9 pups * 1 / 0.4
set.seed(1224)
litter_size <- c(4, 8) * 1 * 0.4 # from literature (Morters, Czupryna, etc.)
dem_ests_sim <- expand_grid(dem_ests_sim, litter_size)

# parallelize this (otherwise it takes a while...)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

vacc_sims <- 
  foreach(i = iter(dem_ests_sim, "row"), 
          .export = c("simvacc", "bind_rows", "tibble"),
          .combine = 'rbind') %dopar% {
            
            # get the prop
            prop_pup <- as.numeric(i[, "1"])
            prop_adult <- 1 - prop_pup
            
            # take the vals from optim and convert to adult survival
            pup_surv <- (i$fert_est/i$litter_size)^(1/12) # annual to monthly 
            adult_surv <- i$psurv_adult_est^(1/12)
            
            # with campaigns only
            camp <- simvacc(tmax = 12*10, 
                            pup_surv = pup_surv, 
                            adult_surv = adult_surv, 
                            revacc = 1, 
                            pup_cov = 0, 
                            campaign_cov = 0.7, 
                            campaign_times = seq(12, 12*10, by = 12), 
                            pup_month_vacc = 1, 
                            birthrate = 2.4/12, # monthly birth rate
                            S_adult_0 = prop_adult*1000, 
                            S_pup_0 = rep(prop_pup/12, 12)*1000, 
                            V_adult_0 = 0, 
                            V_pup_0 = rep(0, 12))
            
            # with pup vacc only
            pup <- simvacc(tmax = 12*10, 
                           pup_surv = pup_surv, 
                           adult_surv = adult_surv, 
                           revacc = 1, 
                           pup_cov = 0.7, 
                           campaign_cov = 0, campaign_times = 0, 
                           pup_month_vacc = 3, 
                           birthrate = 3/12, # monthly birth rate
                           S_adult_0 = prop_adult*1000, 
                           S_pup_0 = rep(prop_pup/12, 12)*1000, 
                           V_adult_0 = 0, 
                           V_pup_0 = rep(0, 12))
            
            both <- simvacc(tmax = 12*10, 
                            pup_surv = pup_surv, 
                            adult_surv = adult_surv, 
                            revacc = 1, 
                            pup_cov = 0.7, 
                            campaign_cov = 0.7, 
                            campaign_times = seq(12, 12*10, by = 12), 
                            pup_month_vacc = 3, 
                            birthrate = 3/12, # monthly birth rate
                            S_adult_0 = prop_adult*1000, 
                            S_pup_0 = rep(prop_pup/12, 12)*1000, V_adult_0 = 0, 
                            V_pup_0 = rep(0, 12))
            
            vacc_comp <- bind_rows(tibble(month = 1:120, 
                                          cov = camp$V/(camp$S + camp$V), 
                                          type = "Campaign", 
                                          litter_size = i$litter_size, 
                                          pup_surv = pup_surv, 
                                          adult_surv = adult_surv), 
                                   tibble(month = 1:120, 
                                          cov = pup$V/(pup$S + pup$V), 
                                          type = "Puppy vax", 
                                          litter_size = i$litter_size, 
                                          pup_surv = pup_surv, 
                                          adult_surv = adult_surv), 
                                   tibble(month = 1:120,
                                          cov = both$V/(both$S + both$V), 
                                          type = "Both", 
                                          litter_size = i$litter_size, 
                                          pup_surv = pup_surv, 
                                          adult_surv = adult_surv))
            vacc_comp
          }

write_csv(vacc_sims, "out/vacc_sims.csv")

# Close out
stopCluster(cl)
