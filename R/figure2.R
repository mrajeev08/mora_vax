# Figure 2. Comparing costs and willingness to pay

# pkgs
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)

# data on costs
costs <- read_csv("data/costs_temp.csv")
willing_to_pay <- read_csv("data/owner_2018.csv")

# animals
animals_2018 <- read_csv("data/animal_2018.csv")
animals_2019 <- read_csv("data/campaign_2019.csv")

# Figure 1A costs by category
costs %>%
  tidyr::complete(category, year = 2018:2019, fill = list(prop = 0)) %>%
  mutate(type = case_when(category == "Vaccine" ~ "Vaccine", 
                          TRUE ~ "Implementation")) -> costs

ggplot(costs) +
  geom_col(aes(x = category, y = cost, fill = factor(year)), 
           position = position_dodge()) +
  labs(x = "Category", y = "Approximate cost (USD)", tag = "A") +
  scale_fill_brewer(palette = "Dark2", name = "Year") +
  theme_minimal_hgrid(font_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> fig2A

# Figure 1B Cost per animal vaccinated
costs %>%
  group_by(year, type) %>%
  summarize(cost = sum(cost, na.rm = TRUE)) %>%
  mutate(n_animals = case_when(year == 2018 ~ nrow(animals_2018), 
                                  year == 2019 ~ nrow(animals_2019)), 
          cost_per_animal = cost/n_animals) -> costs_per_animal

costs_per_animal %>%
  group_by(year) %>%
  summarize(total = sum(cost_per_animal)) -> total_per_animal

ggplot() +
  geom_col(data = costs_per_animal, 
           aes(x = factor(year), y = cost_per_animal, alpha = type, 
               fill = factor(year)), width = 0.5) +
  geom_text(data = total_per_animal, 
            aes(x = factor(year), y = total, label = format(total, digits = 3)), 
            hjust = -0.15) +
  scale_alpha_manual(values = c(0.75, 0.5), name = "Category") + 
  scale_fill_brewer(palette = "Dark2", name = "Year", guide = "none") +
  labs(x = "Year", y = "Cost per animal \n vaccinated (USD)", tag = "B") +
  ylim(c(0, 3)) +
  coord_flip() +
  theme_cowplot(font_size = 12) + 
  theme(axis.line.y = element_blank()) -> fig2B

# Figure 1C Willingness to pay
reduction <- function(charge, willing) {
  length(willing[willing < charge])/length(willing)
}

charge <- seq(0, 16000, by = 500)
willing_to_pay %>%
  filter(AdultDogs + Puppies > 0, Commune %in% c("Moramanga", "Andasibe"), 
         !is.na(WillingnessPay)) %>%
  group_by(Commune) %>%
  summarize(reduction = unlist(lapply(charge, reduction, 
                                         willing = WillingnessPay))) %>%
  mutate(charged = charge) -> willing_by_loc


willing_to_pay %>%
  filter(AdultDogs + Puppies > 0, Commune %in% c("Moramanga", "Andasibe"), 
         !is.na(WillingnessPay)) %>%
  summarize(reduction = unlist(lapply(charge, reduction, 
                                         willing = WillingnessPay))) %>%
  mutate(charged = charge, Commune = "Overall") %>%
  bind_rows(willing_by_loc) -> willingness

ggplot(willingness) +
  geom_line(aes(x = charged/3100, y = 0.6 - reduction*0.6, linetype = Commune), 
            size = 1.25) +
  xlim(c(0, 3)) +
  scale_linetype_manual(values = c(4, 2, 1), name = "") +
  labs(x = "Amount charged to owner (USD)", y = "Coverage", tag = "C") +
  theme_minimal_hgrid(font_size = 12) -> fig2C

# Figure 1D cost recovery (given estimate for both years)
cost_tradeoff <- function(charged, base_cost, cost_per_animal, nvacc, 
                          reduction) {
  new_vacc <- nvacc - reduction*nvacc
  total_cost <- base_cost + new_vacc*cost_per_animal - new_vacc*charged
  new_cost_per_animal <- total_cost/new_vacc
  return(new_cost_per_animal)
}

costs_per_animal %>%
  pivot_wider(names_from = type, values_from = c(cost, cost_per_animal)) %>%
  select(year, base_cost = cost_Implementation, 
         cost_per_animal = cost_per_animal_Vaccine, 
         nvacc = n_animals) %>%
  left_join(willingness, by = character()) %>%
  filter(Commune == "Overall") %>%
  mutate(new_cost = cost_tradeoff(charged = charged/3100, base_cost = base_cost, 
                                  cost_per_animal = 0.75, 
                                  nvacc = nvacc, reduction = reduction)) -> cost_compare
  
ggplot() +
  geom_line(data = cost_compare, 
            aes(x = charged/3100, y = ifelse(new_cost > 5, 5, new_cost), 
                                             color = factor(year)), 
            size = 1.5) +
  geom_hline(data = total_per_animal, aes(yintercept = total, 
                                          color = factor(year)), 
             linetype = 2) +
  labs(x = "Amount charged (USD)", y = "Estimated cost per animal", tag = "D") +
  ylim(c(0, 5)) +
  xlim(c(0, 3)) +
  scale_color_brewer(palette = "Dark2", name = "Year") +
  theme_minimal_grid(font_size = 12) -> fig2D


# arrange
fig2 <- fig2A + fig2C + fig2B + fig2D 
ggsave("figs/fig2.jpeg", height = 8, width = 10)
