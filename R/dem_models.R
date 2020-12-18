
agemod1 <- function(par = c(psurv_pup = -1, 
                            psurv_adult = -1),
                    fert = 6, 
                    n_per_age, type = "fit") {
  
  list2env(as.list(par), envir = environment())
  
  # transform parameters to [0, 1]
  p1 <- exp(psurv_pup)/(1 + exp(psurv_pup))
  p2 <- exp(psurv_adult)/(1 + exp(psurv_adult))

  # make the Leslie matrix
  lmat <- matrix(0, 4, 4)
  lmat[2, 1] <- p2 # every surviving baby becomes juvenile
  lmat[3, 2] <- p2 # every surviving juvenile becomes adult
  lmat[3, 3] <- p2 * 2/3 # 2/3 of them stay in this age class (for years 1 & 2 in age bin)
  lmat[4 ,3] <- p2 * 1/3 # 1/3 of them age up (after year 3 in age bin)
  lmat[4, 4] <- p2  # surviving of final age class
  
  # add in the fertility (same for all adults, i.e. excluding puppies)
  # this is pup survival * birthrate which is not actually identifiable
  # so we look at an average litter size of 6 / 2 = 3 
  lmat[1, 2:4] <- fert * p1
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:4, 1]) / sum(Re(eigen(lmat)$vector[1:4, 1]))
 
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

agemod2 <- function(par = c(psurv_pup = -1, 
                            psurv_adult = -1),
                    fert = 6, 
                    n_per_age, type = "fit") {
  
  list2env(as.list(par), envir = environment())
  
  # transform parameters to [0, 1]
  p1 <- exp(psurv_pup)/(1 + exp(psurv_pup))
  p2 <- exp(psurv_adult)/(1 + exp(psurv_adult))
  
  # make the Leslie matrix
  lmat <- matrix(0, 4, 4)
  lmat[2, 1] <- p2 # every surviving baby becomes juvenile
  lmat[3, 2] <- p2 # every surviving juvenile becomes adult
  lmat[3, 3] <- p2 * 2/3 # 2/3 of them stay in this age class (for years 1 & 2 in age bin)
  lmat[4 ,3] <- p2 * 1/3 # 1/3 of them age up (after year 3 in age bin)
  lmat[4, 4] <- p2  # surviving of final age class
  
  # add in the fertility (same for all adults, i.e. excluding puppies)
  # this is pup survival * birthrate which is not actually identifiable
  # so we look at an average litter size of 6 / 2 = 3 
  lmat[1, 2:4] <- fert * p1
  
  # calculate what this does to the stable structure
  prop_stable <- Re(eigen(lmat)$vector[1:4, 1]) / sum(Re(eigen(lmat)$vector[1:4, 1]))
  stable_age <- prop_stable*sum(n_per_age)
  
  if(type == "pred") {
    # predicted stable age
    growth <- Re(eigen(lmat)$value[1])
    return(list(stable_age = stable_age, growth = growth))
  }
  
  if(type == "fit") {
    # get the sum of squares (distance between observed and predicted prop in each age class)
    return(-sum(dpois(n_per_age, lambda = stable_age, log=TRUE)))
  }
}

     # Jess's original 

SS <- function(par, number.individuals) {
  ## put chosen parameters into the matrix; and use exponential to
  # ensure doesnt go negative
  psurv <- exp(par[1:2])/(1+exp(par[1:2]))
  ## make the matrix with this value of psurv
  mat1 <- matrix(0,4,4)
  mat1[1,1] <- psurv[1]*0 ## after one year, no one stays a baby
  mat1[2,1] <- psurv[1]*1 ## every baby becomes juvenile
  mat1[2,2] <- psurv[2]*0 ## no one stays a juvenile
  mat1[3,2] <- psurv[2]*1 ## every juvenile becomes adult
  mat1[3,3] <- psurv[2]*(1-1/3) ## 1-1/3 of them stay in that age
  class
  mat1[4,3] <- psurv[2]*1/3 ## 1/3 of them age up
  mat1[4,4] <- psurv[2] ## surviving
  ## add in the fertility
  ## xxj note I've added another value - so I've said that in column 3,
  # we have exp(par[3])*1,
  ### and in column 4 we have exp(par[3])*a probabilty defined by
  # par[4]. This is so it has to be smaller!
    mat1[1,3:4] <- 1+exp(par[c(3)])*c(1,exp(par[4])/(1+exp(par[4])))
  ## calculate what this does to the stable structure
  prop.stable.struct.after.seed1 <-
    Re(eigen(mat1)$vector[1:4,1])/sum(Re(eigen(mat1)$vector[1:4,1]))
  ## get the sum of squares (measure of distance between observed and
  # predicted, which we want to be as small as possible)
  sum.of.squares <- sum((number.individuals[1:4]-
                         prop.stable.struct.after.seed1*sum(number.individuals,na.rm=TRUE))^2)
  ## put it back
  return(sum.of.squares)
}

tmp <- optim(par=c(-0.5, -0.5, log(2.5), log(2.5)), fn = SS, 
             number.individuals = n_by_age$N)

## build matrix and predictions with these optimal results
psurv <- exp(tmp$par[1:2])/(1+exp(tmp$par[1:2]))

## make the matrix with this value of psurv
mat1 <- matrix(0,4,4)
mat1[2,1] <- psurv[1]*1 ## every baby becomes juvenile
mat1[3,2] <- psurv[2]*1 ## every juvenile becomes adult
mat1[3,3] <- psurv[2]*(1-1/3) ## 1-1/3 of them stay in that age class
mat1[4,3] <- psurv[2]*1/3 ## 1/3 of them age up
mat1[4,4] <- psurv[2] ## surviving
p.reduce.fert.old <- exp(tmp$par[4])/(1+exp(tmp$par[4])) 
mat1[1,3:4] <- 1+exp(tmp$par[3])*c(1,p.reduce.fert.old)

## predict the size distribution
optim.prop.stable.struct.after.seed <-
  Re(eigen(mat1)$vector[1:4,1])/Re(sum(eigen(mat1)$vector[1:4,1]))

## and add this one to the plot from before 
plot(1:4, optim.prop.stable.struct.after.seed*sum(n_by_age$N, na.rm=TRUE),
     type = "b", pch=19,
     xlab="Age Stage", ylab="Number of Dogs", main="Canine Population",
     ylim=range(c(0, n_by_age$N), na.rm=TRUE))
points(1:4, n_by_age$N[1:4], type="b", col="red") 

## and this means, now you can figure out what the population growth
Re(eigen(mat1)$value[1])

 
