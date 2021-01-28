
ages_to_class <- function(age_yrs, years_in_class = c(1, 1, 4, 1),
                          sample = FALSE, N = 1000) {
  
  years_grtr_eq <- cumsum(years_in_class) - years_in_class
  years_less <- cumsum(years_in_class)
  age_names <- glue::glue("{years_grtr_eq} - {years_less}")
  
  # last age class catches everyone
  nclass <- length(years_less)
  years_less[nclass] <- Inf
  age_names[nclass] <- glue::glue("{years_grtr_eq[nclass]} +")
  
  if(sample) {
    age_yrs <- sample(age_yrs[!is.na(age_yrs)], N, replace = TRUE)
  }
  
  # group by age class
  age_class <- hist(age_yrs, br = c(years_grtr_eq, Inf), 
                    plot = FALSE, include.lowest = TRUE)$counts
  names(age_class) <- age_names
  

  return(age_class)
  
  
}


exp_trans <- function(x) {
  exp(x)/(1 + exp(x))
}

apply_trans <- function(par, par_trans) {
  for(i in 1:length(par)) {
    par[i] <- par_trans[[i]](par[i])
  }
  return(par)
}


# First thing to try
# estimating fertility and backing out pup survival given litter sizes
# constraining fertility to be greater than 1 (1 + fert) vs not constrained
# can just pass par_trans = (..., ..., ..., function(x) 1 + exp(x))
# dpois (-ll minimize?) vs. sum of squares

les_mat <- function(n_classes = 5, years_in_class = c(1, 1, 1, 2, 1),
                    n_per_age, 
                    par = c(p1 = -1, 
                            p2 = -1, 
                            fert = -1),
                    par_trans = list(exp_trans, exp_trans, exp),
                    trans_probs = c("p2", "p2", "p2", "p2", "p2"), 
                    type = "ss") {
  
  par <- apply_trans(par, par_trans)
  
  list2env(as.list(par), envir = environment())
  
  lmat <- matrix(0, n_classes, n_classes)
  
  for(i in 1:(n_classes - 1)) {
    lmat[i + 1, i] <- get(trans_probs[i]) * 1 / years_in_class[i]
    lmat[i, i] <- get(trans_probs[i]) * (1 - 1/years_in_class[i])
  }
  
  # last row / col (age class can be arbitrarily large)
  lmat[n_classes, n_classes] <- get(trans_probs[n_classes])
  
  # fertility
  lmat[1, 2:n_classes] <- fert
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:n_classes, 1]) / 
    sum(Re(eigen(lmat)$vector[1:n_classes, 1]))
  
  if(type == "pred") {
    # predicted stable age
    stable_age <- prop_stable * sum(n_per_age)
    growth <- Re(eigen(lmat)$value[1])
    return(list(stable_age = stable_age, growth = growth))
  }
  
  if(type == "ss") {
    # get the sum of squares (distance between observed and predicted prop in each age class)
    sum_of_squares <- sum((n_per_age - prop_stable * sum(n_per_age))^2)
    return(sum_of_squares)
  }
  
  if(type == "pois") {
    # get the negative log likelihood assuming poisson distribution
    return(-sum(dpois(n_per_age, prop_stable * sum(n_per_age), log = TRUE)))
  }
}




