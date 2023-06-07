# Get demographic estimates for a set of bootstrapped data
library(foreach)
library(doParallel)
library(iterators)
library(dplyr)
library(readr)
library(here)
library(tidyr)
select <- dplyr::select
source("R/dem_models.R")

# animals
animals_2018 <- read_csv("data/animal_2018.csv")
animals_2019 <- read_csv("data/campaign_2019.csv")

# filter to dogs
animals_2018 %>%
  filter(Species == "A") %>%
  mutate(year = 2018, 
         AgeMonths = replace_na(AgeMonths, 0), 
         AgeYears = replace_na(AgeYears, 0), 
         age_mos = AgeYears*12 + AgeMonths) %>%
  mutate(age_mos = ifelse(age_mos == 0, NA, age_mos), 
         Sex = toupper(Sex)) %>%
  select(Fokontany, Commune, Date, age_mos, year, Sex) -> animals_age

animals_2019 %>%
  filter(`Dog or Cat` == "dog") %>%
  select(Date, age_mos = `Age (months)`, Commune, Fokontany = `Owner Location`, 
         Sex) %>%
  mutate(year = 2019, Sex = toupper(Sex)) %>%
  bind_rows(animals_age) -> animals_age

write_csv(animals_age, "out/animals_age.csv")

# Bootstrapped data
set.seed(1334)
n_by_age <- (replicate(100, 
                       ages_to_class(age_yrs = animals_age$age_mos/12, 
                                     years_in_class = c(1, 1, 4, 1),
                                     sample = TRUE)))
n_by_age <- split(n_by_age, rep(1:ncol(n_by_age), each = nrow(n_by_age)))

# original data
original <- ages_to_class(age_yrs = animals_age$age_mos/12, 
                          years_in_class = c(1, 1, 4, 1),
                          sample = FALSE)
n_by_age$`0` <- original
  
# Get expanded grid
set.seed(1335)
par_df <- data.frame(p1 = rnorm(100, mean = 0, sd = 1), 
                     fert = rnorm(100, mean = 1, sd = 1))
ll_grid <- expand_grid(par_df,  
                       age_data = 1:length(n_by_age))

# parallelize this
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

dem_ests <- 
  foreach(i = iter(ll_grid, "row"), .combine = 'rbind', 
          .export = c("les_mat", "exp_trans", "apply_trans")) %dopar% {
    
    # draw initial values
    age_ests <- optim(par = list(p1 = i$p1, 
                                 fert = i$fert), 
                      fn = les_mat, n_per_age = n_by_age[[i$age_data]], 
                      n_classes = 4, years_in_class = c(1, 1, 4, 1),
                      par_trans = list(exp_trans, exp),
                      trans_probs = rep("p1", 4), 
                      type = "pois")
    
    preds <- les_mat(n_per_age = n_by_age[[i$age_data]], 
                     par = age_ests$par, 
                     n_classes = 4, years_in_class = c(1, 1, 4, 1),
                     par_trans = list(exp_trans, exp),
                     trans_probs = rep("p1", 4),
                     type = "pred")
    
    par_ests <- apply_trans(age_ests$par, list(exp_trans, exp))
    
    # output
    data.frame(ll = age_ests$value, 
               sim = names(n_by_age[i$age_data]), 
               psurv_adult_init = i$p1,
               fert_init = i$fert,
               fert_est = par_ests["fert"], 
               psurv_adult_est = par_ests["p1"],
               stable_ages_est = preds$stable_age, 
               growth = preds$growth,
               stable_ages_data = n_by_age[[i$age_data]], 
               age_class = 1:length(preds$stable_age))
               
  }

stopCluster(cl)

# Parameter estimates
dem_ests %>%
  group_by(sim, growth) %>%
  mutate(pop_growth = factor(ifelse(growth >= 1, "Growing", "Declining")), 
         prop_data = stable_ages_data/sum(stable_ages_data), 
         prop_est = stable_ages_est/sum(stable_ages_est)) -> dem_ests

# Output for demography
write_csv(dem_ests, "out/dem_ests.csv")


