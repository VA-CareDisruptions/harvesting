mod.mat <- model.matrix(~ agey +pandemic_period +qtr, data=b1)

model_string <- "
model{
for(t in 1:N_times){ 
  N_deaths[t] ~ dnegbin(prob[t],r)
  prob[t]<- r/(r+lambda[t])
  
  
  log(lambda[t]) <- ( #int  + 
          log.offset[t] + #offset
          beta[agey[t]+1] + #age-specific intercept
          pandemic[t]*delta[agey[t]+1] + #age specific pandemic effect 
          epsilon[agey[t]+1]*(t - N_times/2)/N_times #age specific trend
  )

}
  
  beta[1] ~ dnorm(0, 1e-4)
 # baseline_prob[1] <- beta[1] +int
  
   re.beta[1] <- 0
  for(i in 2:101){
      beta[i] <- beta[i-1] + re.beta[i]
     re.beta[i] ~ dnorm(0, tau.beta) #AR(1) model
  }
  
  re.delta[1] <- 0
  delta[1] ~ dnorm(0, 1e-4)
    for(i in 2:101){
      re.delta[i] ~ dnorm(0, tau.delta) #AR(1) model
      delta[i] <- delta[i-1] + re.delta[i]
    }
  
    
    re.epsilon[1] <- 0
  epsilon[1] ~ dnorm(0,1e-4)
    for(i in 2:101){
      epsilon[i]  <- epsilon[i-1] + re.epsilon[i]
      re.epsilon[i] ~ dnorm(0, tau.epsilon)
  }
  
   #int ~ dnorm(0, 1e-4)
   
   tau.beta~ dgamma(3, 2)
   tau.delta~ dgamma(3,2)
   tau.epsilon~ dgamma(3,2)


    r ~ dunif(0,250)

 }
"
##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')
##############################################
#Model Organization
##############################################
model_spec<-textConnection(model_string)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('N_deaths'=b1$N_deaths ,
                                 'N_times'=nrow(b1) ,
                                 'log.offset'=b1$log.offset,
                                 'agey'=b1$agey,
                                 'pandemic'=b1$pandemic_period
                       ),
                       n.adapt=10000, 
                       n.chains=3)

params<-c( 'beta','delta','epsilon','tau.beta')
##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000)
posterior_samples.all<-do.call(rbind,posterior_samples)

saveRDS(posterior_samples.all, './Results/posteriors_rw1.rds')
