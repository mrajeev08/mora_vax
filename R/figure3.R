# Figure 3. Demography and vax

# pkgs
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)
library(bbmle)

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
  mutate(age_mos = ifelse(age_mos == 0, NA, age_mos)) %>%
  select(Fokontany, Commune, Date, age_mos, year, Sex) -> animals_age

animals_2019 %>%
  filter(`Dog or Cat` == "dog") %>%
  select(Date, age_mos = `Age (months)`, Commune, Fokontany = `Owner Location`, 
         Sex) %>%
  mutate(year = 2019) %>%
  bind_rows(animals_age) -> animals_age

# histogram of ages
ggplot(animals_age) +
  geom_histogram(aes(x = age_mos)) +
  facet_wrap(~year)

animals_age %>%
  mutate(Sex = toupper(Sex), age = floor(age_mos/12)) %>%
  group_by(Sex, age) %>%
  summarize(N  = n()) %>%
  mutate(N = ifelse(Sex == "M", -N, N)) -> age_pyramid

ggplot(age_pyramid, aes(x = age, y = N, fill = Sex)) +
  geom_col() +
  scale_y_continuous(breaks = c(seq(-600, -200, 200), seq(200, 600, 200)), 
                     labels = c(seq(600, 200, -200), seq(200, 600, 200)), 
                     limits = c(-600, 600)) +
  coord_flip() +
  labs(x = "Age in years", y = "Number of individuals")

age_pyramid %>%
  filter(!is.na(age) & !is.na(N)) %>%
  mutate(age_class = case_when(age == 0 ~ 0, 
                               age == 1 ~ 1, 
                               age > 1 & age < 5 ~ 2, 
                               age >= 5 ~ 3)) %>%
  group_by(age_class) %>%
  summarize(N = sum(abs(N))) %>%
  mutate(prop = N/sum(N)) -> n_by_age
  
age_ests <- optim(par = list(psurv_pup = -2, psurv_adult = 1),
                  fert = 3,
                  fn = agemod2, n_per_age = n_by_age$N,
                  method = "L-BFGS-B")
exp(age_ests$par[c("psurv_pup", "psurv_adult")])/(1 + exp(age_ests$par[c("psurv_pup", "psurv_adult")]))
agemod2(n_per_age = n_by_age$N, par = age_ests$par, type = "pred")

ll_grid <- expand.grid(psurv_pup = seq(-1, 1, by = 0.1), 
                       psurv_adult = seq(-1, 1, by = 0.1), 
                       fert = seq(2, 5, by = 1))
ll_grid %>%
  rowwise() %>%
  mutate(ss = agemod1(n_per_age = n_by_age$N, 
                      par = c(psurv_pup = psurv_pup, 
                              psurv_adult = psurv_adult),
                      fert = fert, type = "fit"),
         gr = agemod1(n_per_age = n_by_age$N, 
                      par = c(psurv_pup = psurv_pup, 
                              psurv_adult = psurv_adult),
                      fert = fert, type = "pred")$growth) -> ll

ggplot(data = ll) +
  geom_tile(aes(x = psurv_pup, y = psurv_adult, fill = sqrt(ss))) +
  facet_wrap(~ fert) +
  scale_fill_gradient2(midpoint = median(sqrt(ll$ss)))

ggplot(data = ll) +
  geom_tile(aes(x = exp_trans(psurv_pup), 
                y = exp_trans(psurv_adult), fill = gr)) +
  facet_wrap(~ fert) +
  scale_fill_gradient2(midpoint = 1) + 
  labs(x = "Annual survival probability - pups", 
       y = "Annual survival probability - adults")
