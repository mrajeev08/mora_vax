# Figure 3. Demography and vax
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)

# bring in data
animals_age <- read_csv("out/animals_age.csv")
dem_ests <- read_csv("out/dem_ests.csv")
vacc_sims <- read_csv("out/vacc_sims.csv")

# A: age pyramid ---------------
animals_age %>%
  mutate(Sex = toupper(Sex), age = floor(age_mos/12)) %>%
  group_by(Sex, age) %>%
  summarize(N  = n()) %>%
  mutate(N = ifelse(Sex == "M", -N, N)) -> age_pyramid

pyramid_A <- 
  ggplot(age_pyramid, aes(x = age, y = N, fill = Sex)) +
  geom_col() +
  scale_y_continuous(breaks = c(seq(-600, -200, 200), 0, seq(200, 600, 200)), 
                     labels = c(seq(600, 200, -200), "", seq(200, 600, 200)), 
                     limits = c(-600, 600)) +
  coord_flip() +
  scale_fill_manual(values = c("#6f50a1", "#0d6d64")) +
  labs(x = "Age in years", y = "Number of individuals", tag = "A") +
  theme_minimal_hgrid(font_size = 12)

# B: stable age distributions ---------------
dem_ests %>%
  select(sim, growth, age_class, prop_data, prop_est) %>%
  pivot_longer(prop_data:prop_est, values_to = "prop") -> dem_props


stable_age_B <- 
  ggplot(dem_props) +
  geom_line(aes(x = age_class, y = prop, group = interaction(sim, name), 
                color = name), 
            alpha = 0.25) +
  scale_x_continuous(breaks = 1:4, 
                     labels = c("0 - 1", "1 - 2", "2 - 6", "6+")) +
  scale_color_manual(values = c("navy", "lightblue"), 
                     labels = c("Bootstrapped data", "Model estimates"), 
                     name = "") +
  labs(x = "Age class", y = "Proportion of \n population", tag = "B") +
  theme_minimal_hgrid(font_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size = 1.1, alpha = 1)))

  
# C: par ests ---------------
dem_ests %>%
  select(fert_est, psurv_adult_est,
         growth, pop_growth) %>%
  distinct() %>%
  pivot_longer(fert_est:psurv_adult_est, values_to = "estimate", 
               names_to = "par") -> dem_pars

par_labs <- c("fert_est" = "Fertility", 
              "psurv_adult_est" = "Adult survival")

par_ests_C <- 
  ggplot(dem_pars) +
  geom_histogram(aes(x = estimate, fill = pop_growth)) +
  facet_wrap(~par, 
             labeller = as_labeller(par_labs), scales = "free_x", 
             nrow = 1) +
  scale_fill_manual(values = c("grey", "#d95f02")) +
  labs(x = "Parameter estimate", y = "Frequency", fill = "Pop growth", 
       tag = "C") +
  theme_minimal_hgrid(font_size = 12)
  
# D: vacc ests ---------------
vacc_sims %>%
  group_by(type, month) %>%
  summarize(mean = mean(cov), max = max(cov), min = min(cov)) -> vacc_summs
  
vacc_plot_D <- 
  ggplot(vacc_summs) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max, fill = type), 
              alpha = 0.5) +
  geom_line(aes(x = month, y = mean, color = type)) +
  scale_x_continuous(breaks = seq(0, 12*10, 12), labels = 0:10) +
  scale_fill_brewer(aesthetics = c("color", "fill"), palette = "Accent", 
                    labels = c("Combined", "Annual campaigns", 
                               "Continuous \n puppy vax"),
                    name = "Campaign type") +
  geom_hline(yintercept = 0.7, linetype = 2) +
  ylim(c(0, 1)) +
  labs(x = "Year", y = "Vaccination coverage", tag = "D") +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = guide_legend(override.aes = list(size = 1.1)))
  
# Pull plot together ----------
fig3_top <- pyramid_A  + stable_age_B + plot_layout(nrow = 1, guides = "collect") 
fig3_bottom <- par_ests_C + vacc_plot_D + plot_layout(nrow = 1, guides = "collect")
fig3 <- fig3_top / fig3_bottom
ggsave("figs/fig3.jpeg", fig3, height = 7, width = 8)
