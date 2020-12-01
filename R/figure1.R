# pkgs
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)

# Function for getting coverage
chapman <- function(marked, sighted, resighted){
  pop_est <- ((marked + 1)*(sighted + 1)/(resighted + 1)) - 1
  var <- ((marked + 1)*(sighted + 1)*(marked - resighted)*(sighted - resighted)) / 
    (((resighted + 1)^2)*(resighted + 2))
  pop_CI <- 1.965*sqrt(var)
  return(data.frame(pop_est, pop_CI))
}

# Match vacc #s to gis
mora_communes <- st_read("data/shapefile/mora_communes.shp")
vacc_2018 <- read_csv("data/animal_2018.csv")
vacc_2019 <- read_csv("data/campaign_2019.csv")

# Coverage estimates based on transects
transects <- read_csv("data/transects_2018.csv")

vacc_2018 %>%
  filter(Species == "A") %>%
  group_by(Location) %>%
  summarize(vacc = n()) -> vacc_by_loc

transects %>% 
  filter(Species == "A") %>%
  count(Mark, Location) %>%
  tidyr::pivot_wider(names_from = Mark, values_from = n) %>%
  rowwise() %>%
  mutate(sighted_total = sum(N + Y, na.rm = TRUE), 
         resighted = Y) %>%
  select(Location, sighted_total, resighted) %>%
  left_join(vacc_by_loc) %>%
  mutate(commune = ifelse(Location == "Andasibe", "Andasibe", 
                          "Moramanga")) -> cov

# @ commune level
cov %>%
  group_by(commune) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(chapman(marked = vacc, 
                           sighted = sighted_total, 
                           resighted = resighted),
         year = 2018, 
         cov_est = vacc/pop_est, 
         cov_est_lower = vacc/(pop_est + pop_CI), 
         cov_est_upper = vacc/(pop_est - pop_CI)) -> cov_commune_2018

# 2019 coverage estimates
vacc_2019 %>%
  filter(`Dog or Cat` == "dog") %>%
  group_by(Commune) %>%
  summarize(vacc = n()) %>%
  left_join(mora_communes, by = c("Commune" = "commune")) %>%
  mutate(cov_est = vacc/(pop/22.5), 
         cov_est_lower = vacc/(pop/15), 
         cov_est_upper = vacc/(pop/30), 
         year = 2019) %>%
  select(commune = Commune, starts_with("cov"), vacc, year) %>%
  bind_rows(select(cov_commune_2018, starts_with("cov"), 
                   vacc, commune, year)) -> cov_commune

cov_commune %>%
  complete(year = 2018:2019, commune = mora_communes$commune, 
           fill = list(vacc = 0, cov = 0)) %>%
  right_join(mora_communes) %>%
  filter(!is.na(year)) -> map_cov
st_geometry(map_cov) <- map_cov$geometry


# Map with midpoint
fig1A <-
  ggplot(map_cov) +
  geom_sf(fill = NA) + 
  stat_sf_coordinates(data = filter(map_cov, vacc != 0), 
                      aes(size = vacc, fill = cov_est, shape = factor(year)), 
                      color = "#3D4849") +
  facet_wrap(~year, ncol = 2) +
  theme_map() +
  theme(strip.text = element_text(face = "bold", hjust = 0.45)) +
  scale_fill_distiller(name = "Cov.\n estimate", direction = 1) +
  scale_shape_manual(values = c(21, 23), guide = "none") +
  scale_size(name = "No. dogs\n vaccinated", 
             breaks = c(50, 150, 400, 1000)) +
  labs(tag = "A")
  

# Plot with range
fig1B <- 
  ggplot(cov_commune) +
  geom_pointrange(aes(x = commune, y = cov_est, ymin = cov_est_upper, 
                      ymax = cov_est_lower, 
                      shape = factor(year)), color = "#3D4849") +
  scale_shape_manual(values = c(16, 18), name = "Year") +
  labs(x = "Commune", y = "Coverage", tag = "B") +
  ylim(c(0, 1)) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1 <- (fig1A / fig1B) + plot_layout(heights = c(2, 1), guides = "collect")

ggsave("figs/fig1.jpeg", height = 6, width = 7)
                  