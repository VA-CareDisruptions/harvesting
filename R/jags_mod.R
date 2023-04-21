mod.mat <- model.matrix(~ agey +pandemic_period +qtr + t, data=b1)

model_string <- "
model{
for(j in 1:N_obs){ 
  N_deaths[j] ~ dnegbin(prob[j],r)
  prob[j]<- r/(r+lambda[j])
  
  log(lambda[j]) <- ( #int  + 
  log.offset[j] + #offset
  beta[agey[j]+1] + #age-specific intercept
  pandemic[j]*delta[agey[j]+1] + #age specific pandemic effect 
  epsilon[agey[j]+1]*time[j] #age specific trend
  )
}
  
 beta[1] ~ dnorm(0, (1-rho^2)*tau.beta)
 # baseline_prob[1] <- beta[1] +int
  
  for(i in 2:101){
      beta[i] ~ dnorm(rho*beta[i-1], tau.beta) #AR(1) model
    #  baseline_prob[i] <- beta[i] +int
  }
  
  delta[1] ~ dnorm(0, (1-rho2^2)*tau.delta)
    for(i in 2:101){
      delta[i] ~ dnorm(rho2*delta[i-1], tau.delta) #AR(1) model
    }
  
  epsilon[1] ~ dnorm(0, (1-rho3^2)*tau.epsilon)
    for(i in 2:101){
      epsilon[i] ~ dnorm(rho3*epsilon[i-1], tau.epsilon) #AR(1) model
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

#inits.inla <- list(int = 5.731, tau.beta = 0.451, tau.delta = 1.7 , tau.epsilon= 2.93 , rho= 0.99 , rho2=0.99, rho3=0.99, r = 0.002 )
##############################################
#Model Organization
##############################################
model_spec<-textConnection(model_string)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1),
                       data=list('N_deaths'=b1$N_deaths ,
                                 'N_obs'=nrow(b1) ,
                                 'time'= b1$t,
                                 'log.offset'=b1$log.offset,
                                 'agey'=b1$agey,
                                 'pandemic'=b1$pandemic_period
                       ),
                       n.adapt=10000, 
                       n.chains=1)

params<-c( 'beta','delta','epsilon','rho','tau.beta') #,'int','baseline_prob')
##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000)
posterior_samples.all<-do.call(rbind,posterior_samples)

saveRDS(posterior_samples.all, './Results/posteriors.rds')