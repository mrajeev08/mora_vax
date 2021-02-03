# Table 1 & other stats

# pkgs
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(lubridate)
library(gt)

# data
vacc_2018 <- read_csv("data/animal_2018.csv")
vacc_2019 <- read_csv("data/campaign_2019.csv")
owner_2018 <- read_csv("data/owner_2018.csv")
vacc_pts_2018 <- read_csv("data/campaign_pts_2018.csv")

# Get all animals vaccinated
vacc_2018 %>%
  mutate(year = 2018, 
         location = ifelse(Commune %in% "Ambohibary", "Moramanga", Commune)) %>%
  group_by(year, location) %>%
  summarize(n_animals = n()) %>%
  bind_rows(vacc_2018 %>% 
              summarize(n_animals = n()) %>% 
              mutate(year = 2018, location = "All")) %>%
  bind_rows(vacc_2019 %>% 
              summarize(n_animals = n()) %>%
              mutate(year = 2019, 
                    location = "All")) -> animals_All

# filter to dogs
vacc_2018 %>%
  filter(Species %in% "A") %>%
  mutate(year = 2018, 
         prev_vacc = ifelse(!is.na(PreviousVaccine), 1, 0), 
         vacc_in_year = ifelse(PreviousVaccine %in% 2017, 1, 0), 
         Commune = ifelse(Commune %in% "Ambohibary", "Moramanga", Commune)
         ) %>%
  select(Fokontany, loc = Commune, Date, year, Sex, prev_vacc, 
         vacc_in_year) -> animals_2018

vacc_2019 %>%
  filter(`Dog or Cat` %in% "dog") %>%
  select(Date, Commune, Fokontany = `Owner Location`, 
         Sex, prev_vacc = `Previous Vaccination`,
         vacc_in_year = `Last Vaccination Date`) %>%
  mutate(year = 2019, loc = "All",
         vacc_in_year = year(dmy(vacc_in_year)), 
         prev_vacc = ifelse(prev_vacc %in% "YES", 1, 0), 
         vacc_in_year = ifelse(vacc_in_year %in% 2018, 1, 0)) %>%
  bind_rows(animals_2018) -> animals_all

animals_all %>%
  group_by(loc, year) %>%
  summarize(total = scales::number(n(), big.mark = ","), 
            prev_vacc = percent(sum(prev_vacc)/n()), 
            vacc_in_year = percent(sum(vacc_in_year)/n()),
            perc_m = percent(sum(Sex %in% "M")/n())) %>%
  ungroup() %>%
  pivot_longer(total:perc_m) %>%
  pivot_wider(names_from = c(loc, year), 
              values_from = value) -> dog_stats

animals_all %>%
  filter(year == 2018) %>%
  mutate(loc = "All") %>%
  group_by(loc, year) %>%
  summarize(total = scales::number(n(), big.mark = ","), 
            prev_vacc = percent(sum(prev_vacc)/n()), 
            vacc_in_year = percent(sum(vacc_in_year)/n()),
            perc_m = percent(sum(Sex %in% "M")/n())) %>%
  pivot_longer(total:perc_m) %>%
  pivot_wider(names_from = c(loc, year), 
              values_from = value) %>%
  left_join(dog_stats) -> dog_stats
  
# Get owner stats -------
owner_2018 %>%
  group_by(Commune) %>%
  summarize(avg_dogs = number(mean(AdultDogs + Puppies, na.rm = TRUE), accuracy = 0.1), 
            roaming = percent(sum(grepl("mireny", 
                                        Roaming, 
                                        ignore.case = TRUE))/n())) %>%
  filter(Commune != "Ambohibary") %>%
  mutate(loc = paste(Commune, 2018, sep = "_")) %>%
  select(-Commune) %>%
  pivot_longer(avg_dogs:roaming) %>%
  pivot_wider(names_from = loc, 
              values_from = value) -> owner_stats

owner_2018 %>%
  filter(Commune != "Ambohibary") %>%
  mutate(loc = "All_2018") %>%
  group_by(loc) %>%
  summarize(avg_dogs = number(mean(AdultDogs + Puppies, na.rm = TRUE), accuracy = 0.1), 
            roaming = percent(sum(grepl("mireny", 
                                        Roaming, 
                                        ignore.case = TRUE))/n())) %>%
  pivot_longer(avg_dogs:roaming) %>%
  pivot_wider(names_from = loc, 
              values_from = value) %>%
  left_join(owner_stats) %>%
  bind_rows(dog_stats, .) -> all_stats

# Get location/person stats -------
# N locs
vacc_pts_2018 %>%
  mutate(location = ifelse(Commune %in% "Moramanga Ville", "Moramanga", Commune)) %>%
  group_by(location) %>%
  summarize(ndays = n_distinct(Date), 
            npoint = n(), 
            n_person_days = sum(People)) %>%
  bind_rows(vacc_pts_2018 %>%
              summarize(ndays = n_distinct(Date), 
                        npoint = n(), 
                        n_person_days = sum(People)) %>% 
              mutate(location = "All")) %>%
  mutate(year = 2018) -> vacc_metrics_2018

vacc_2019 %>%
  summarize(ndays = n_distinct(Date), 
            npoint = n_distinct(Date, `Vaccination Location`), 
            n_person_days = n_distinct(Date)) %>%
  mutate(location = "All", year = 2019) -> vacc_metrics_2019

vacc_metrics <- bind_rows(vacc_metrics_2018, vacc_metrics_2019) 
write_csv(vacc_metrics, "out/vacc_metrics.csv")

vacc_metrics %>%
  left_join(animals_All) %>%
  mutate(across(ndays:n_person_days, 
                ~ paste0(round(n_animals/.x, 1), " (", .x, ")"))) -> vacc_mets_all

vacc_mets_all %>%
  mutate(loc = paste0(location, "_", year), 
         n_animals = as.character(n_animals)) %>%
  select(-location, -year) %>%
  pivot_longer(starts_with("n")) %>%
  pivot_wider(names_from = loc, 
              values_from = value) %>%
  bind_rows(all_stats, .) %>%
  select(name, All_2018, Moramanga_2018, Andasibe_2018, All_2019) -> all_stats

write_csv(all_stats, "out/table1_stats.csv")

# Table 1
# look up vals
lookup <- tribble(~name, ~label, ~order, 
                  "n_animals", "Total animals \n vaccinated", 1,
                  "total", "Total dogs \n vaccinated", 2, 
                  "prev_vacc", "Dogs with history of vaccination", 3,
                  "vacc_in_year",  "Dogs vaccinated within last year", 4,
                  "perc_m",  "Percent male dogs", 5,
                  "avg_dogs",  "Average dogs per owner", 6,
                  "roaming",  "Percent of owners with \n free-roaming dogs", 7,
                  "ndays", "Animals vaccinated per day (total days)", 8, 
                  "npoint", "Animals vaccinated per \n vaccination point (total points)", 9,
                  "n_person_days",  "Animals vaccinated per \n person day (total person days)", 10)


all_stats %>%
  left_join(lookup) %>%
  arrange(order) %>%
  select(label, everything(), -name, -order) %>%
  gt() %>%
  tab_spanner(
    label = "2018",
    columns = vars(Andasibe_2018, Moramanga_2018, All_2018)
  ) %>%
  tab_spanner(label = "2019",
              columns = vars(All_2019)) %>%
  cols_label(
    label = "",
    All_2019 = "All Communes",
    All_2018 = "All Communes",
    Andasibe_2018 = "Andasibe",
    Moramanga_2018 = "Moramanga Ville"
  ) -> table1

saveRDS(table1, "out/table1.rds")
gtsave(table1, "figs/table1.png")
