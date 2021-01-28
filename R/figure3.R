# Figure 3. Demography and vax
# pkgs
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)

# bring in data
animals_age <- read_csv("out/animals_age.csv")

# A original age pyramid
animals_age %>%
  mutate(Sex = toupper(Sex), age = floor(age_mos/12)) %>%
  group_by(Sex, age) %>%
  summarize(N  = n()) %>%
  mutate(N = ifelse(Sex == "M", -N, N)) -> age_pyramid

pyramid_A <- 
  ggplot(age_pyramid, aes(x = age, y = N, fill = Sex)) +
  geom_col() +
  scale_y_continuous(breaks = c(seq(-600, -200, 200), seq(200, 600, 200)), 
                     labels = c(seq(600, 200, -200), seq(200, 600, 200)), 
                     limits = c(-600, 600)) +
  coord_flip() +
  scale_fill_manual(values = c("#6f50a1", "#0d6d64")) +
  labs(x = "Age in years", y = "Number of individuals", tag = "A") +
  theme_minimal_hgrid(font_size = 12)

ggplot(dem_ests) +
  geom_histogram(aes(x = growth, fill = pop_growth))
ggplot(dem_ests) +
  geom_histogram(aes(x = fert_est, fill = pop_growth)) 
ggplot(dem_ests) + 
  geom_histogram(aes(x = psurv_adult_est, fill = pop_growth)) 

par_est_B <-
  ggplot(filter(dem_ests), 
         aes(x = fert_est, y = psurv_adult_est, 
             shape = pop_growth)) +
  geom_point() +
  scale_shape_manual(values = c(1, 16)) +
  theme_minimal_hgrid(font_size = 12) +
  labs(x = "Pup survival (annual prob)", 
       y = "Adult survival (annual prob)", 
       tag = "B", 
       color = "Fertility", shape = "Estimated pop \n growth")

ggplot(filter(dem_ests, sim == 0), 
       aes(x = 1 + exp(fert_est))) +
  geom_histogram()

ggplot(filter(dem_ests, sim == 0), 
       aes(x = psurv_adult_est)) +
  geom_histogram()

ggplot(filter(dem_ests, sim == 0, 
              pop_growth == "Growing"), 
       aes(x = psurv_pup_est)) +
  geom_histogram()

# summarize
dem_ests %>% 
  group_by(sim, growth) %>%
  mutate(prop_data = stable_ages_data/sum(stable_ages_data), 
         prop_est = stable_ages_est/sum(stable_ages_est)) %>%
  select(prop_data, prop_est, growth, pop_growth, sim, age_class) %>%
  pivot_longer(prop_data:prop_est, names_to = "type", values_to = "prop") -> ests_summ

ggplot(ests_summ) +
  geom_boxplot(aes(x = factor(age_class), y = prop, fill = type)) +
  scale_fill_brewer(palette = "Accent")

ggplot(ests_summ) +
  geom_boxplot(aes(x = factor(age_class), y = prop, fill = type)) +
  scale_fill_brewer(palette = "Accent")

ggplot(ests_summ) +
  geom_density(aes(x = prop, fill = type)) +
  facet_wrap(~age_class) +
  scale_fill_brewer(palette = "Accent")


# Simulate vacc given these parameters and store in data frame
# Plot as D
