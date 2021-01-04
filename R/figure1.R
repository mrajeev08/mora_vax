# pkgs
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggsn)

# From Claire's MSC
range_people_ph <- c(4.13, 5.654)
range_dogs_ph <- c(0.24, 0.26)

# Range of hdrs then
mora_hdrs <- range_people_ph / range_dogs_ph
mora_mid_hdr <- median(mora_hdrs)
hdr_high <- 25
hdr_low <- 8

# function to get upper & lower conf intervals from binomial into a data.frame
bn_ci <- function(resighted, sighted_total) {
  out <- binom.test(resighted, sighted_total, resighted/sighted_total,
                    alternative = "two.sided", 
                    conf.level = 0.95)
  return(data.frame(cov_est = resighted/sighted_total, 
                    cov_est_lower = out$conf.int[1], 
                    cov_est_upper = out$conf.int[2]))
}

# Match vacc #s to gis
mora_communes <- st_read("data/shapefile/mora_communes.shp")
vacc_2018 <- read_csv("data/animal_2018.csv")
vacc_2019 <- read_csv("data/campaign_2019.csv")
vacc_pts_2018 <- read_csv("data/campaign_pts_2018.csv")

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
  mutate(sighted_total = sum(N, Y, na.rm = TRUE), 
         resighted = Y) %>%
  select(Location, sighted_total, resighted) %>%
  left_join(vacc_by_loc) %>%
  mutate(commune = ifelse(Location == "Andasibe", "Andasibe", 
                          "Moramanga"), 
         bn_ci(resighted, sighted_total)) -> cov

cov %>%
  left_join(vacc_pts_2018) %>%
  filter(commune == "Moramanga") -> cov_pts

# @ commune level
cov %>%
  group_by(commune) %>%
  summarise(across(where(is.numeric), sum)) %>%
  rowwise() %>%
  mutate(year = 2018, 
         bn_ci(resighted, sighted_total), 
         type = "transect") %>%
  left_join(mora_communes) -> cov_commune_2018

vacc_2018 %>%
  filter(Species == "A") %>%
  group_by(Commune) %>%
  summarize(vacc = n()) %>%
  filter(!(Commune %in% "Ambohibary")) %>%
  left_join(mora_communes, by = c("Commune" = "commune")) %>%
  mutate(cov_est = vacc/(pop/mora_mid_hdr), 
         cov_est_lower = vacc/(pop/hdr_low), 
         cov_est_upper = vacc/(pop/hdr_high), 
         year = 2018, 
         type = "hdr") %>%
  bind_rows(rename(cov_commune_2018, Commune = commune)) -> cov_commune_2018

# 2019 coverage estimates
vacc_2019 %>%
  filter(`Dog or Cat` == "dog") %>%
  group_by(Commune) %>%
  summarize(vacc = n()) %>%
  left_join(mora_communes, by = c("Commune" = "commune")) %>%
  mutate(cov_est = vacc/(pop/mora_mid_hdr), 
         cov_est_lower = vacc/(pop/hdr_low), 
         cov_est_upper = vacc/(pop/hdr_high), 
         year = 2019, 
         type = "hdr") %>%
  bind_rows(cov_commune_2018) -> cov_commune

cov_commune %>%
  select(commune = Commune, year, vacc, cov_est, cov_est_lower, 
         cov_est_upper, type) %>%
  filter(!(interaction(year, type) %in% "2018.hdr")) %>%
  complete(year = 2018:2019, commune = mora_communes$commune, 
           fill = list(vacc = 0, cov_est = 0)) %>%
  right_join(mora_communes) %>%
  filter(!is.na(year)) -> map_cov
st_geometry(map_cov) <- map_cov$geometry

# Map with midpoint
fig1A <-
  ggplot(map_cov) +
  geom_sf(fill = NA) + 
  stat_sf_coordinates(data = filter(map_cov, vacc != 0), 
                      aes(size = vacc, fill = cov_est, shape = type), 
                      color = "#3D4849") +
  facet_wrap(~year, ncol = 2) +
  theme_map() +
  theme(strip.text = element_text(face = "bold", hjust = 0.45)) +
  scale_fill_distiller(name = "Cov.\n estimate", direction = 1) +
  scale_shape_manual(values = c(21, 23), guide = "none") +
  scale_size(name = "No. dogs\n vaccinated", 
             breaks = c(50, 150, 400, 1000)) +
  labs(tag = "A") +
  north(data = map_cov, anchor = c(x = 48.5, y = -18.2), symbol = 9) +
  scalebar(
    data = map_cov, dist = 10, dist_unit = "km",
    transform = TRUE, model = "WGS84", anchor = c(x = 48.5, y = -19.4),
    height = 0.01, angle = 45, hjust = 1, st.size = 3, facet.var = "year", 
    facet.lev = 2019
  ) 

# fig1A inset
mada_out <- st_read("data-raw/mada_communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mada_out %>%
  mutate(dist = ifelse(ADM2_EN == "Moramanga", TRUE, FALSE)) %>%
  group_by(dist) %>%
  summarize() %>%
  st_simplify() -> mada_out

map_inset <- ggplot() +
  geom_sf(data = mada_out, aes(fill = factor(dist)), color = NA) +
  scale_fill_manual(values = c("darkgrey", "white"), guide = "none") + 
  theme_map() 

# fig1A outset
map_outset <- ggplot() +
  ggplot(filter(map_cov, commune == "Moramanga")) +
  geom_sf(fill = NA) + 
  geom_point(data = cov_pts, 
             aes(x = Long, y = Lat, size = vacc, fill = cov_est), 
             color = "#3D4849", shape = 21) +
  scale_fill_distiller(name = "Cov.\n estimate", direction = 1) +
  theme_map()

# patchwork them together

# Plot with range
fig1B <- 
  ggplot(cov_commune) +
  geom_pointrange(aes(x = Commune, y = cov_est, 
                      ymin = scales::squish(cov_est_lower, c(0, 1)), 
                      ymax = scales::squish(cov_est_upper, c(0, 1)), 
                      shape = type, color = factor(year)), 
                  position = position_dodge(width = 0.2)) +
  scale_shape_manual(values = c(16, 18), name = "Year") +
  scale_color_brewer(palette = "Dark2", name = "Year") +
  labs(x = "Commune", y = "Coverage", tag = "B") +
  ylim(c(0, 1)) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1 <- (fig1A / fig1B) + plot_layout(heights = c(2, 1), guides = "collect")

ggsave("figs/fig1.jpeg", height = 6, width = 7)
                  