##FROM KISSLER AND GRAD,  AJE https://github.com/gradlab/LifeExpectancy
library(tidyverse) 

parse_life_table <- function(lt){
  out <- lt %>% 
    rename(Age=age) %>% 
    rename(PDeath=qx) %>% 
    rename(NSurv=lx) %>% 
    rename(NDie=dx) %>% 
    rename(PersonYears=Lx) %>% 
    rename(FuturePersonYears=Tx) %>% 
    rename(LifeExpectancy=ex) %>% 
    separate(Age, c("Age","Age_to"), sep="-") %>% 
    select(-Age_to) %>% 
    mutate(Age=as.numeric(Age))
  return(out)
}

calc_pdeath_inst <- function(pdeath_cond){
  lt <- tibble(age=0:(length(pdeath_cond)-1), pdeath_cond=pdeath_cond) %>% 
    bind_rows(tibble(age=-1, pdeath_cond=0)) %>% 
    arrange(age) %>% 
    mutate(psurv=1-pdeath_cond) %>% 
    mutate(cumpsurv=cumprod(psurv)) %>% 
    mutate(pdeath_inst=pdeath_cond*lag(cumpsurv)) %>% 
    filter(age>=0)
  return(lt$pdeath_inst)
}

calc_pdeath_cond <- function(pdeath_inst){
  lt <- tibble(age=0:(length(pdeath_inst)-1), pdeath_inst=pdeath_inst) %>% 
    arrange(desc(age)) %>% 
    mutate(pdeath_inst_cumsum=cumsum(pdeath_inst)) %>% 
    mutate(pdeath_cond=pdeath_inst/pdeath_inst_cumsum) %>% 
    arrange(age)
  return(lt$pdeath_cond)
}

norm_rows <- function(m){
  m_rs <- rowSums(m)
  for(indexA in 1:nrow(m)){
    if(m_rs[indexA]==0){
      m[indexA,] <- rep(0,ncol(m))
    } else {
      m[indexA,] <- m[indexA,]/m_rs[indexA]
    }
  }
  return(m)
}

get_le <- function(m){
  le_df <- tibble(age=-1:(nrow(m)-1),p_die_cond=c(0,diag(m))) %>% 
    mutate(p_surv_lag=1-lag(p_die_cond)) %>% 
    slice(2:n()) %>% 
    mutate(p_surv_cumprod=cumprod(p_surv_lag)) %>% 
    mutate(p_die_inst=p_die_cond*p_surv_cumprod) %>% 
    summarise(le=sum((age+0.5)*p_die_inst))
  return(le_df$le)
}

get_le_new <- function(m){
  # le_df <- tibble(age=-1:(nrow(m)-1),qx=c(0,diag(m))) %>% 
  le_df <- tibble(age=0:(nrow(m)-1),qx=diag(m)) %>% 
    append_Ix() %>% 
    mutate(dx=Ix*qx) %>% 
    # mutate(dx_check=Ix-lead(Ix)) %>% 
    mutate(Lx=Ix-0.5*dx) %>% 
    mutate(Lx=case_when(age==0~0.116*Ix+(1-0.116)*lead(Ix), TRUE~Lx)) %>%
    # mutate(Lxcheck=0.5*(Ix+lead(Ix))) %>%
    arrange(desc(age)) %>% 
    mutate(Tx=cumsum(Lx)) %>% 
    arrange(age) %>%
    mutate(ex=Tx/Ix)
  
  return(le_df)
  
}

append_Ix <- function(le_df){
  
  qxvec <- le_df %>% 
    arrange(age) %>% 
    pull(qx)
  
  Ixvec <- rep(NA, length(qxvec))
  Ixvec[1] <- 100000
  for(indexA in 2:length(qxvec)){
    Ixvec[indexA] <- Ixvec[indexA-1]*(1-qxvec[indexA-1])
  }
  
  le_df <- mutate(le_df, Ix=Ixvec)
  
  return(le_df)
}

make_upper_tri <- function(v,shift=TRUE){
  if(shift==FALSE){
    
    vnew <- c()
    for(i in 1:length(v)){
      vnew <- c(vnew, c(rep(0,i-1),v[i:length(v)]))
    }
    
    out <- matrix(vnew, nrow=length(v), byrow=TRUE)
    return(out)
    
  } else {
    
    vnew <- c()
    for(i in 1:length(v)){
      vnew <- c(vnew, c(rep(0,i-1),v[1:(length(v)-i+1)]))
    }
    
    out <- matrix(vnew, nrow=length(v), byrow=TRUE)
    return(out)
    
  }
}

increment_lifematrix <- function(m,v0){
  # pull off diagnoal
  # pull off last row
  # add first row
  # normalize	
  m_new <- m-diag(diag(m))
  m_new <- m_new[1:(nrow(m)-1),]
  m_new <- rbind(matrix(v0,nrow=1,byrow=TRUE),m_new)
  m_new <- norm_rows(m_new)
  return(m_new)
}

get_le_from_pdeath <- function(v){
  get_le(norm_rows(make_upper_tri(calc_pdeath_inst(v),shift=FALSE)))
}

sim_covid <- function(trng, pdeath_inst_baseline, xi, k){
  
  # Inputs: 
  #	trng: range of years to simulate life expectancy (pandemic at time 0)
  # 	pdeath_inst_baseline: instantaneous probability of death, baseline
  # 	xi: fold-change in death *rate* due to COVID
  # 	k: fold-dropoff in probability of death with additional future-years
  
  # Initialize an output vector for life expectancies: 
  levec <- c()
  
  prob_death_mat <- rep(NA, times=length(pdeath_inst_baseline))
  
  
  # Create a life matrix using the baseline probabilities of death: 
  m <- make_upper_tri(pdeath_inst_baseline, shift=FALSE)
  m <- norm_rows(m)
  
  # Begin the simulation
  for(t in trng){
    
    if(t==0){
      
      # --- Calculate excess probability of death from COVID:
      # Turn the baseline probability of death...
      p_death <- diag(m)
      # into a baseline rate of death...
      r_death <- -log(1-p_death)
      # then calculate the rate of death from COVID...
      r_death_covid <- xi*r_death
      # and convert back into a probability of death during COVID.
      p_death_covid <- 1-exp(-r_death_covid)
      # Calculate the excess probability of death from COVID
      excess_p_death_covid <- p_death_covid - p_death
      
      # --- Distribute excess probability across future-years: 
      # Create a matrix of future-year proportions: 
      fymat <- m-diag(diag(m))
      # Create a matrix of death-probability dropoff: 
      kmat <- make_upper_tri(c(0,k^(-(0:(ncol(m)-2)))), shift=TRUE)
      # Calculate the normalizing factor for death dropoff: 
      x <- excess_p_death_covid/rowSums(kmat*fymat)
      x[is.na(x)] <- 0
      # Turn this normalizing factor into a matrix:
      xmat <- matrix(rep(x,nrow(m)),nrow=nrow(m))
      # Calculate the distribution of excess death across future-years:
      fy_death_dist_mat <- xmat*kmat*fymat
      # Reduce future-year proportions to account for excess death:
      m <- m - fy_death_dist_mat
      # Add that excess death to this year's deaths:
      diag(m) <- diag(m) + excess_p_death_covid
    }
    
    # Calculate life expectancy
    levec <- c(levec, get_le(m))

    #save the diagonals, which is conditional prob of death in this year
    prob_death_mat <- cbind(prob_death_mat, diag(m))
    
    # Increment the life matrix
    m <- increment_lifematrix(m,pdeath_inst_baseline)
    

    
  }
  prob_death_mat <- prob_death_mat[,-1] #first one is just NAs used to initialize the matrx
  return(list('le'=tibble(t=trng,le=levec), 'prob_death'=prob_death_mat ))
  
}


















