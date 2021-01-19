# Need to track down an accounting issue where some revax get lost
# but this is approximately right
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
  
  # Order of steps:
  # 1. all adults have babies (S + V)
  # 2. survival probabilities are applies to everyone
  # 3. Surviving vaccinated indiviudals immunity wanes
  # 4. Then new vax and revax happends
  
  for(t in 2:tmax) {
    
    # Births (everyone first has babies and then dies)
    births <- birthrate * (sum(V_adult[1:36, t - 1]) + S_adult[t - 1])
    
    # balance births (all surviving to susceptibles)
    S_pup[1, t] <- births
    
    # Pup mortality
    S_pup[2:12, t] <- S_pup[1:11, t - 1] * pup_surv
    V_pup[2:12, t] <- V_pup[1:11, t - 1] * pup_surv
    
    # Balance the adults (ageing in 12 mo old pups)
    S_adult[t] <- adult_surv * S_adult[t - 1] + pup_surv * S_pup[12, t - 1] 
    
    # pups to vaccinate (at age x)
    pups_to_vacc <- ifelse(pup_month_vacc > 1, S_pup[pup_month_vacc, t], 
                           births)
    
    # do puppy vaccination 
    # CHECK: this should be additive no? Or is it always zero? Yes because no vaccs at t yet! Unless others have been vaccinated in campaign?
    V_pup[pup_month_vacc, t] <- pups_to_vacc * pup_cov 
    S_pup[pup_month_vacc, t] <- S_pup[pup_month_vacc, t] - pups_to_vacc * pup_cov 
    
    # Do the waning (adults only!)
    V_adult[2:36, t] <- adult_surv * V_adult[1:35, t - 1]
    S_adult[t] <- S_adult[t] + V_adult[36, t - 1] 
    
    # Campaign vaccination of adults accounting for revaccination --------------
    
    # Do the waning (adults only)
    V_adult[2:36, t] <- adult_surv * V_adult[1:35, t - 1]
    # After applying survival probabilities
    S_adult[t] <- S_adult[t] + V_adult[36, t - 1] 
    
    # Coverage now
    cov_now <- ifelse(t %in% campaign_times, campaign_cov, 0)
    
    # First get the number of currently vaccinated doggoes that are revaccinated
    # Assume that these are distributed evenly across the waning continuum 
    # i.e. same prop across V_adult[, t]
    total_vax_adult <- cov_now * (S_adult[t] + sum(V_adult[, t]))
    revax_adult <- sum(revacc * V_adult[, t])
    
    # constrain new vaccination by preferentially revaccinating already vaxed adults
    new_vax_adult <- ifelse(revax_adult > total_vax_adult, 0, total_vax_adult - revax_adult)
    
    # constrain revaccination so it cannot exceed coverage level
    # if greater than total_vax_adult, then adjusted coverage should be total_vax_adult/revac_adult
    # to adjust to total expected vaccinations
    adj_cov <- ifelse(revax_adult > total_vax_adult, 
                      total_vax_adult/revax_adult * V_adult[, t], cov_now * V_adult[, t])
    
    # subtract these from currently vaccinated adults 
    V_adult[2:36, t] <- V_adult[2:36, t] - adj_cov
    
    # and move them into the first waning step along with the newly vaccinated
    V_adult[1, t] <- sum(adj_cov) + new_vax_adult
    
    # Balance the susceptibles as well (subtract newly vaccinated)
    S_adult[t] <- S_adult[t] - new_vax_adult
    
    # Do the same vax steps for pups
    # But assuming that puppies aren't revaccinated
    camp_vacc_pup <- S_pup[pup_month_vacc:12, t] * cov_now 
    V_pup[pup_month_vacc:12, t] <- V_pup[pup_month_vacc:12, t] + camp_vacc_pup
    S_pup[pup_month_vacc:12, t] <- S_pup[pup_month_vacc:12, t] - camp_vacc_pup
    
    # Add in surviving pups aging to adults so that they can wane appropriately
    # for example if vaccinated at three months, will enter adulthood at waning stage of 10 months
    # i.e. having spent 9 mos. vaccinated!
    v_to_sub <- 13 - pup_month_vacc
    V_adult[v_to_sub, t] <- V_adult[v_to_sub, t] +  pup_surv * V_pup[12, t - 1]
    
  }
  
  V <- colSums(V_pup) + colSums(V_adult)
  S <- colSums(S_pup) + S_adult
  
  return(list(S = S, V = V, V_adult = V_adult, V_pup = V_pup, 
              S_adult = S_adult, S_pup = S_pup))
  
}