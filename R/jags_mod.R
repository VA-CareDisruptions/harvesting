mod.mat <- model.matrix(~ agey +pandemic_period +qtr, data=b1)

model_string <- "
model{
for(t in 1:N_times){ 
  N_deaths[t] ~ dnegbin(prob[t],r)
  prob[t]<- r/(r+lambda[t])
  
  #N_deaths[t] ~ dpois(lambda[t])
  
  log(lambda[t]) <- ( int  + 
  log.offset[t] + #offset
  beta[agey[t]+1] + #age-specific intercept
  pandemic[t]*delta[agey[t]+1] + #age specific pandemic effect 
  epsilon[agey[t]+1]*t #age specific trend
  )

}
  
  beta[1] ~ dnorm(0, (1-rho^2)*tau.beta)
  baseline_prob[1] <- beta[1] +int
  
  for(i in 2:101){
      beta[i] ~ dnorm(rho*beta[i-1], tau.beta) #AR(1) model
      baseline_prob[i] <- beta[i] +int
  }
  
  delta[1] ~ dnorm(0, (1-rho2^2)*tau.delta)
    for(i in 2:101){
      delta[i] ~ dnorm(rho2*delta[i-1], tau.delta) #AR(1) model
    }
  
    
  epsilon[1] ~ dnorm(0, (1-rho3^2)*tau.epsilon)
    for(i in 2:101){
      epsilon[i] ~ dnorm(rho2*epsilon[i-1], tau.epsilon) #AR(1) model
  }
  
   int ~ dnorm(0, 1e-4)
   
   tau.beta~ dgamma(3, 2)
   tau.delta~ dgamma(3,2)
   tau.epsilon~ dgamma(3,2)

   rho ~ dunif(-1,1)
   rho2 ~ dunif(-1,1)
   rho3 ~ dunif(-1,1)
   
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

params<-c( 'beta','delta','epsilon','rho','tau.beta','int','baseline_prob')
##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000)
posterior_samples.all<-do.call(rbind,posterior_samples)

saveRDS(posterior_samples.all, './Results/posteriors.rds')