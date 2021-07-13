

### takes a list of function and parameters and data, produces para_est

find_the_best_pars <- function(lpar, upar, fun, df, nm, nt){
  value <- rep("NA", nm)
  parameter <- rep(0, nm)
  for (i in 1:nm){
    x <- (i-1)*nt + 1
    y <- i*nt
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    value[i] <- list(est_DE$optim$bestval)
    parameter[i] <- list(est_DE$optim$bestmem)
  }
  return(list(parameter = parameter, value = value))
}

est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, df_sim,
                          1, 80)

sym_negLogLik(as.numeric(unlist(est$parameter)), df_sim)
