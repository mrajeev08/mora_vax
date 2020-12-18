# Table 1

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

# filter to dogs
vacc_2018 %>%
  filter(Species %in% "A") %>%
  mutate(year = 2018, 
         prev_vacc = ifelse(!is.na(PreviousVaccine), 1, 0), 
         vacc_in_year = ifelse(PreviousVaccine %in% 2017, 1, 0)) %>%
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
  filter(loc != "Ambohibary") %>%
  group_by(loc, year) %>%
  summarize(total = scales::number(n(), big.mark = ","), 
            prev_vacc = percent(sum(prev_vacc)/n()), 
            vacc_in_year = percent(sum(vacc_in_year)/n()),
            perc_m = percent(sum(Sex %in% "M")/n())) %>%
  ungroup() %>%
  pivot_longer(total:perc_m) %>%
  pivot_wider(names_from = c(loc, year), 
              values_from = value) -> dog_stats

owner_2018 %>%
  group_by(Commune) %>%
  summarize(avg_dogs = number(mean(AdultDogs + Puppies, na.rm = TRUE), accuracy = 0.1), 
            avg_cats = number(mean(Cats + Kittens, na.rm = TRUE),  accuracy = 0.1), 
            roaming = percent(sum(grepl("mireny", 
                                        Roaming, 
                                        ignore.case = TRUE))/n())) %>%
  filter(Commune != "Ambohibary") %>%
  mutate(loc = paste(Commune, 2018, sep = "_")) %>%
  select(-Commune) %>%
  pivot_longer(avg_dogs:roaming) %>%
  pivot_wider(names_from = loc, 
              values_from = value) %>%
  bind_rows(dog_stats, .) -> all_stats
  
all_stats %>%
  mutate(name = case_when(name == "total" ~ "Total # of dogs \n vaccinated", 
                          name == "prev_vacc" ~ "Previous history of vaccination", 
                          name == "vacc_in_year" ~ "Vaccinated within last year", 
                          name == "perc_m" ~ "Percent male", 
                          name == "avg_dogs" ~ "Average # of dogs per owner", 
                          name == "avg_cats" ~  "Average # of cats per owner", 
                          name == "roaming" ~ "Percent of owners with \n free-roaming animals")) %>%
  gt() %>%
  cols_move_to_end(columns = vars(All_2019)) %>%
  tab_spanner(
    label = "2018",
    columns = vars(Andasibe_2018, Moramanga_2018)
  ) %>%
  tab_spanner(label = "2019",
              columns = vars(All_2019)) %>%
  cols_label(
    name = "",
    All_2019 = "All Communes",
    Andasibe_2018 = "Andasibe",
    Moramanga_2018 = "Moramanga Ville"
  ) -> table1

saveRDS(table1, "figs/table1.rds")
gtsave(table1, "figs/table1.png")
