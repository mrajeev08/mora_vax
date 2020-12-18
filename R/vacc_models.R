
# annual prob -> monthly prob of surv
# annual birthrate -> monthly birthrate (Adults * births)
# Can work in proportions (need to get starting proportion of adults vs. pups aged 1 mo - 12)

# Add in revaccination!

simvacc <- function(tmax, pup_surv, adult_surv, revacc, pup_cov, 
                    campaign_cov, campaign_times, pup_month_vacc = 1,
                    birthrate, S_adult_0, V_adult_0, 
                    V_pup_0, S_pup_0) {
  
  # Set-up
  V_adult <- matrix(0, nrow = 36, ncol = tmax)
  S_adult <- rep(0, tmax)
  V_pup <- S_pup <- matrix(0, nrow = 12, ncol = tmax)
  
  # Initalize
  S_adult[1] <- S_adult_0
  V_adult[1, 1] <- V_adult_0
  V_pup[, 1] <- V_pup_0
  S_pup[, 1] <- S_pup_0
  
  for(t in 2:tmax) {
    
    # Births (everyone first has babies and then dies)
    births <- birthrate * (sum(V_adult[1:36, t - 1]) + S_adult[t - 1])
    
    # balance births (all to susceptibles)
    S_pup[1, t] <- births # get replaced
    
    # Pup mortality
    S_pup[2:12, t] <- S_pup[1:11, t - 1] * pup_surv
    V_pup[2:12, t] <- V_pup[1:11, t - 1] * pup_surv
    
    # Balance the adults (ageing in 12 mo old pups)
    S_adult[t] <- adult_surv * S_adult[t - 1] + pup_surv * S_pup[12, t - 1] 
    
    # pups to vaccinate (at age x)
    pups_to_vacc <- ifelse(pup_month_vacc > 1, S_pup[pup_month_vacc, t], 
                           births)
    
    # do puppy vaccination 
    V_pup[pup_month_vacc, t] <- pups_to_vacc * pup_cov 
    S_pup[pup_month_vacc, t] <- S_pup[pup_month_vacc, t] - pups_to_vacc * pup_cov 
    
    # Do the waning (adults only!)
    V_adult[2:36, t] <- adult_surv * V_adult[1:35, t - 1]
    S_adult[t] <- S_adult[t] + V_adult[36, t - 1] 
    
    # Vaccinate as part of campaign accounting for revax
    cov_now <- ifelse(t %in% campaign_times, campaign_cov, 0)
    camp_vacc_adult <- (S_adult[t] + revacc * sum(V_adult[, t])) * cov_now - revacc * sum(V_adult[, t])
    camp_vacc_adult <- ifelse(camp_vacc_adult < 0, 0, camp_vacc_adult)
    
    # every vax is revaccinated with a certain revacc prob
    if(camp_vacc_adult > 0) {
      V_adult[1, t] <- camp_vacc_adult + revacc * sum(V_adult[, t])
      V_adult[2:36, t] <- (1 - revacc) * V_adult[2:36, t]
    } else {
      V_adult[1, t] <- camp_vacc_adult # newly vaccinated
    }
    
    # Balance addtl campaign vax adults
    S_adult[t] <- S_adult[t] - camp_vacc_adult
    
    # Balance addtl campaign vax pups (assuming that no puppies are revax)
    camp_vacc_pup <- S_pup[pup_month_vacc:12, t] * cov_now 
    V_pup[pup_month_vacc:12, t] <- V_pup[pup_month_vacc:12, t] + camp_vacc_pup
    S_pup[pup_month_vacc:12, t] <- S_pup[pup_month_vacc:12, t] - camp_vacc_pup
    
    # Add in surviving pups aging to adults so that they can wane appropriately
    t_to_sub <- pup_month_vacc - 1
    V_adult[13 - t_to_sub, t] <- V_adult[13, t] +  pup_surv * V_pup[12, t - 1]
    
  }
  
  V <- colSums(V_pup) + colSums(V_adult)
  S <- colSums(S_pup) + S_adult
  
  return(list(S = S, V = V, V_adult = V_adult, V_pup = V_pup, 
              S_adult = S_adult, S_pup = S_pup))
  
}


# get rate from prob (annual to annual)
rate_from_prob <- function(prob) {
  -log(1 - prob)
}

# Vaccination model (SV)
## Project population
vacc_reconstruct <- function(t, y, parms){
  pups <- parms["births"] * (y["S"] + y["V"])
  pups_V <- parms["pup_cov"] * pups
  pups_S <- pups - pups_V
  dS <- -parms["deaths"] * y["S"] + pups_S * (y["S"] + y["V"]) + 
    parms["waning"] * y["V"] 
  dV <- -parms["deaths"] * y["V"] - parms["waning"] * y["V"] + pups_V
  return(list(c(dS, dV)))
} 

eventfun <- function(t, y, parms){
  with (as.list(y),{
    diff <- parms["campaign_cov"]*(S + V) - V ## newly vaccinated
    V <- V + diff
    S <- S - diff
    return(c(S, V))
  })
}


## times in relevant timestep
times <- seq (1/52, 5, by = 1/52) ## weekly for 10 years

## initial pop
IS <- 70000
IV <- 0

## param
births <- 0.50
deaths <- 0.42
waning <- 0.33
parms <- c(deaths = deaths, births = births, waning = waning)

## run
calcsus <- lsoda(y = c(S = IS, V = IV), times = times, func = vacc_reconstruct, parms = parms, 
                 events = list(func = eventfun, time = seq(1/52, 5, by = 1)))
vacc_df <- data.frame(Year = calcsus[, "time"], propVacc = calcsus[,"V"]/(calcsus[,"V"] + calcsus[,"S"]))

vacc_a <- ggplot(data = vacc_df, aes(x = Year, y = propVacc)) +
  geom_line(color = "lightseagreen", size = 1) +
  xlab ("Year") +
  ylab("Coverage") + 
  ylim(c(0, 1)) +
  geom_hline(yintercept = 0.69, color = "aquamarine4", linetype = 2, size = 1) +
  newtheme +
  theme(text = element_text(color = "aquamarine4"), axis.text = element_text(color = "aquamarine4"), 
        axis.line = element_line(color = "aquamarine4"))
