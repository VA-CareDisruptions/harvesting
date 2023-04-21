formula1 <- N_deaths ~  0 + #intercept
  f(agey,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-specific intercept
  f(agey2,t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) +   #age-varying effect for time
  f(agey3,pandemic_period,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-varying effect for pandemic
  offset(log.offset)


#from Josh warren

#prior_fixed<-list(mean.intercept = 0.00, prec.intercept = (1.00/(100.00^2)),  #\beta_0 ~ N(0.00, 100.00^2)
#                  mean = 0.00, prec = (1.00/(100.00^2)))                      #\beta_j ~ N(0.00, 100.00^2); j=1,...,p

prior_fixed<-list(mean.intercept = 0.00, prec.intercept = (1.00/(100.00^2)))  #\beta_0 ~ N(0.00, 100.00^2)


prior_hyper<-list(theta = list(prior = "normal", param = c(0.00, (1.00/(100.00^2)))))  #ln(r) ~ N(0.00, 100.00^2)

# Run model
# Start the clock!
ptm <- proc.time()

inla.out <- inla(formula1, family = "nbinomial", data = b1,
                 control.fixed = prior_fixed,
                 control.family = list(hyper = prior_hyper),
                 control.compute = list(dic = TRUE, 
                                        waic = TRUE, 
                                        config = TRUE,
                                        return.marginals.predictor = TRUE))

# Stop the clock
proc.time() - ptm
