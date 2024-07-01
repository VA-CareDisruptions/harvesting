#from Josh warren

spatial_neighbors_mat <- spatstat.sparse::gridadjacencymatrix(dims=c(max(b1$t),max(b1$agey +1 )), across = TRUE, down = TRUE, diagonal=FALSE)

Q<-diag(rowSums(spatial_neighbors_mat)) - 
  spatial_neighbors_mat

C<-diag(nrow(spatial_neighbors_mat)) - 
  Q


C.mat <- as.matrix(C)

b1 <- b1 %>%
  mutate(agey3=agey, agey_str = str_pad(agey,3,'0', side='left'),
           id = as.numeric(as.factor( paste(str_pad(t,2 , pad = "0", side='left'), agey_str )  )),
         id2 = ( paste(str_pad(t, 3, pad = "0", side='left'),agey_str)),
                 qtr=as.factor(qtr),
         pandemicI = ifelse(year>=2020,1,0),
         t2=t
         )
  



formula1 <- N_deaths ~  1+ #intercept
  offset(log.offset) +
  f(agey,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))  +
    f(t ,model = "ar1",  group=pandemicI, hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))  +
    f(t2 ,model = "iid",  group=!pandemicI)   
    
    
    #+  #global age-specific effect
  #f(pandemic_t,model = "iid") #+  #global age-specific effect
  
    #f(agey2,t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) +   #age-varying effect for linear time
    #f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))  # + # global pandemic time effect
 # f(id,
 #   model = "generic1",
 #   Cmatrix = C.mat,
 #   hyper = list(theta1 = list(prior = "loggamma", param = c(0.01, 0.01)),                #\tau^2 ~ IG(0.01, 0.01)
 #                theta2 = list(prior = "gaussian", param = c(0.00, (1.00/(100.00^2))))   #logit(\rho) ~ N(0.00, 100.00^2)
 #     )
 #)




#prior_fixed<-list(mean.intercept = 0.00, prec.intercept = (1.00/(100.00^2)),  #\beta_0 ~ N(0.00, 100.00^2)
#                  mean = 0.00, prec = (1.00/(100.00^2)))                      #\beta_j ~ N(0.00, 100.00^2); j=1,...,p



#prior_fixed <- list(mean.intercept = 0.00, prec.intercept = (1.00/(100.00^2)))  #\beta_0 ~ N(0.00, 100.00^2)

prior_fixed <- list(mean.intercept = 0.00, prec.intercept = (1.00/(100.00^2)))  #\beta_0 ~ N(0.00, 100.00^2)

prior_hyper<-list(theta = list(prior = "normal", param = c(0.00, (1.00/(100.00^2)))))  #ln(r) ~ N(0.00, 100.00^2)

# Run model
# Start the clock!
ptm <- proc.time()

inla.out <- inla(formula1, family = "nbinomial", data = list(pandemicI=b1$pandemicI, t2= b1$t2,qtr=b1$qtr,agey= b1$agey,pandemic_t=b1$pandemic_t,agey2= b1$agey2,agey3=b1$agey3,t=b1$t, id = b1$id, C.mat = C.mat, N_deaths = b1$N_deaths, log.offset=b1$log.offset ),
                 control.fixed = prior_fixed,
                 control.family = list(hyper = prior_hyper),
                 control.compute = list(dic = TRUE, 
                                        waic = TRUE, 
                                        config = TRUE,
                                        return.marginals.predictor = TRUE) 
                 )

#                 lincomb = inla.make.lincomb(agey=80, pandemic_t=c(0:8)) )


# Stop the clock
proc.time() - ptm

# test1 <-inla.posterior.sample(1000, inla.out)
# 
# sample.ds1 <- sapply(test1, function(x){
#   preds <- x$latent[,1]
#   return(preds)
# }, simplify='array') %>%
#   as.data.frame() %>%
#   filter(grepl('Predictor',row.names(.)))

