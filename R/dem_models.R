# testing gh token issue
# other opts that you need to be able to do
# estimating pup_surv and assuming fertility vs. estimating fertility and backing out fertility
# constraining fertility to be greater than 1 (1 + fert) 
# can just pass par_trans = (..., ..., ..., function(x) 1 + exp(x))
# dpois (-ll minimize?) vs. sum of squares

les_mat <- function(n_classes = 5, years_in_class = c(1, 1, 1, 2, 1),
                    n_per_age, 
                    par = c(p1 = -1, 
                            p2 = -1, 
                            fert = -1),
                    par_trans = list(exp_trans, exp_trans, exp),
                    trans_probs = c("p2", "p2", "p2", "p2", "p2"), 
                    type = "fit") {
  
  
  for(i in 1:length(par)) {
    par[i] <- par_trans[[i]](par[i])
  }
  
  list2env(as.list(par), envir = environment())
  
  lmat <- matrix(0, n_classes, n_classes)
  
  for(i in 1:(n_classes - 1)) {
    lmat[i + 1, i] <- get(trans_probs[i]) * 1 / years_in_class[i]
    lmat[i, i] <- get(trans_probs[i]) * (1 - 1/years_in_class[i])
  }
  
  # last row / col (age class can be arbitrarily large)
  lmat[n_classes, n_classes] <- get(trans_probs[n_classes])
  
  # Fertility
   lmat[1, 2:n_classes] <- fert
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:n_classes, 1]) / 
    sum(Re(eigen(lmat)$vector[1:n_classes, 1]))
  
  if(type == "pred") {
    # predicted stable age
    stable_age <- prop_stable*sum(n_per_age)
    growth <- Re(eigen(lmat)$value[1])
    return(list(stable_age = stable_age, growth = growth))
  }
  
  if(type == "fit") {
    # get the sum of squares (distance between observed and predicted prop in each age class)
    sum_of_squares <- sum((n_per_age - prop_stable*sum(n_per_age))^2)
    return(sum_of_squares)
  }
}


# Function for getting data binned by different age classes



agemod <- function(par = c(psurv_pup = -1, 
                           psurv_adult = -1),
                    fert = 3, 
                    n_per_age, type = "fit") {
  
  list2env(as.list(par), envir = environment())
  
  # transform parameters to [0, 1]
  p1 <- exp(psurv_pup)/(1 + exp(psurv_pup))
  p2 <- exp(psurv_adult)/(1 + exp(psurv_adult))

  # make the Leslie matrix
  lmat <- matrix(0, 5, 5)
  lmat[2, 1] <- p2 # every surviving pup becomes y1
  lmat[3, 2] <- p2 # every surviving y1 becomes y2
  lmat[4, 3] <- p2 # every surviving y2 becomes y3-5
  lmat[4, 4] <- p2 * 1/2 # 1/2 of them stay in this age class (for years 3 -> 4)
  lmat[5 ,4] <- p2 * 1/2 # 1/2 of them age up (year 4 -> 5)
  lmat[5, 5] <- p2  # surviving to final age class (5 + years)
  
  # add in the fertility (same for all adults, i.e. excluding puppies)
  # this is pup survival * birthrate which is not actually identifiable
  # so we look at an average litter size of fert & try and estimate pup survival
  lmat[1, 2:5] <- fert * p1
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:5, 1]) / sum(Re(eigen(lmat)$vector[1:5, 1]))
  
  if(type == "pred") {
    # predicted stable age
    stable_age <- prop_stable*sum(n_per_age)
    growth <- Re(eigen(lmat)$value[1])
    return(list(stable_age = stable_age, growth = growth))
  }
  
  if(type == "fit") {
    # get the sum of squares (distance between observed and predicted prop in each age class)
    sum_of_squares <- sum((n_per_age - prop_stable*sum(n_per_age))^2)
    return(sum_of_squares)
  }
}


# bootsrap age data
age_booted <- function(age_mos, nsample = 1000) {
  
  ages <- sample(age_mos[!is.na(age_mos)], nsample, replace = TRUE)
  
  tibble(ages) %>%
    mutate(age = floor(ages/12)) %>%
    group_by(age) %>%
    summarize(N = n()) %>%
    mutate(age_class = case_when(age == 0 ~ 1, # < 1 yr olds
                                 age == 1 ~ 2, # 1 - 2 yr olds
                                 age == 2 ~ 3, # 2 - 3 yr olds
                                 age > 2 & age <= 4 ~ 4, # 3 - 4 yr olds
                                 age > 4 ~ 5)) %>% # 5+ yr olds
    group_by(age_class) %>%
    summarize(N = sum(abs(N))) %>%
    mutate(prop = N/sum(N)) -> n_by_age
    
  return(n_by_age$N)
  
}

exp_trans <- function(x) {
  exp(x)/(1 + exp(x))
}

agemod2 <- function(par = c(fert = -1, 
                            psurv_adult = -1),
                   n_per_age, type = "fit") {
  
  list2env(as.list(par), envir = environment())
  
  # transform parameters to be positive
  f1 <- exp(fert)

  # transform parameters to [0, 1]
  p2 <- exp(psurv_adult)/(1 + exp(psurv_adult))

  # make the Leslie matrix
  lmat <- matrix(0, 5, 5)
  lmat[2, 1] <- p2 # every surviving pup becomes y1
  lmat[3, 2] <- p2 # every surviving y1 becomes y2
  lmat[4, 3] <- p2 # every surviving y2 becomes y3-5
  lmat[4, 4] <- p2 * 1/2 # 1/2 of them stay in this age class (for years 3 and 4 in age bin)
  lmat[5 ,4] <- p2 * 1/2 # 1/2 of them age up (after year 5 in age bin)
  lmat[5, 5] <- p2  # surviving to final age class
  
  # add in the fertility (same for all adults, i.e. excluding puppies)
  # this is pup survival * birthrate which is not actually identifiable
  # so we will back out fertility based on litter sizes
  lmat[1, 2:5] <- f1
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:5, 1]) / sum(Re(eigen(lmat)$vector[1:5, 1]))
  
  if(type == "pred") {
    # predicted stable age
    stable_age <- prop_stable*sum(n_per_age)
    growth <- Re(eigen(lmat)$value[1])
    return(list(stable_age = stable_age, growth = growth))
  }
  
  if(type == "fit") {
    # get the sum of squares (distance between observed and predicted prop in each age class)
    sum_of_squares <- sum((n_per_age - prop_stable*sum(n_per_age))^2)
    return(sum_of_squares)
  }
}