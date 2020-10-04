# Restricted Dynamic Occupancy Model Parameterization
# Adapted from Lyons et al. (2018) and Kery and Schaub (2012)

sink("RDO-model.jags")
cat("
    
model {
    
# Priors and constraints
  for (i in 1:M) {
    for (t in 1:(n.occasions-1)) {
      phi[i,t] <- mean.phi
    } # t
  for (t in 1:n.occasions) {
    p[i,t] <- mean.p
    } 
  }
    
# Persistence
    
  mean.phi ~ dunif(0,1)
    
# Resight
    
  mean.p ~ dunif(0,1)
    
  for (t in 1:n.occasions) {
    gamma[t] ~ dunif(0,1)
  } 
    
# Likelihood
  for (i in 1:M) {

    # First occasion
    # State process
    z[i,1] ~ dbern(gamma[1])
    mu1[i] <- z[i,1]*p[i,1]

    # Observation process
    y[i,1] ~ dbern(mu1[i])

    # Subsequent occasions
    for (t in 2:n.occasions) {
      # State process
      q[i,t-1] <- 1 - z[i,t-1] # Availability for recruitment
      mu2[i, t] <- phi[i,t-1] * z[i,t-1] + gamma[t]*prod(q[i,1:(t-1)])
      z[i,t] ~ dbern(mu2[i,t])
      
      # Observation process
      mu3[i,t] <- z[i,t] * p[i,t]
      y[i,t] ~ dbern(mu3[i,t])
    } # t
  } # i
    
# ------------- flag scans ---------------------------------  
# # Binomial model for flag scans
    #   
    # # Priors
    #   
    #   pflag ~ dbeta(1,1)
    #   
    # # Likelihood
    #   
    #   for (i in 1:n) {
    #     m[i] ~ dbin(pflag,K[i])
    #   }
    
    # GLMM for scan samples
    
    # Priors
    
    alpha ~ dnorm(0,0.001)
    
    for (i in 1:n.per) {
    delta[i] ~ dnorm(0, tau.per)
    }
    
    tau.per <- pow(sigma.per, -2)
    
    sigma.per ~ dunif(0,10)
    
    # Likelihood
    
    for (i in 1:n) {
    m[i] ~ dbin(lp[i],K[i])
    
    logit(lp[i]) <- alpha + delta[ per[i] ]
    }
    
    for (i in 1:n.per) {
    pflag[i] <- exp(alpha + delta[i])/(1 + exp(alpha + delta[i]))
    }
    
    meanpflag <- exp(alpha)/(1 + exp(alpha))
    
# ------------- end : flag scans ---------------------------------
    
# ------------- age scans ---------------------------------  

# GLM for age scans
    
# Priors
    
  for (i in 1:n.per) {
    beta.rper[i] ~ dnorm(0, 0.001)
  }
    
# Likelihood
    
  for (i in 1:n2) {
    a[i] ~ dbin(pad[i],Tot[i]) # Distribution for random part
    logit(pad[i]) <- beta.rper[ rper[i] ] # Link function and linear predictor
  }
    
  for (j in 1:n.per) {
    padult[j] <- 1/(1 + exp(-beta.rper[j]))
  }
    
  meanpadult <- mean(padult[])
    
# ------------- end : age scans ---------------------------------
    
# Calculate derived population parameters
  for (t in 1:n.occasions) {
    qgamma [t] <- 1 - gamma[t]
  }

  cprob[1] <- gamma[1]

  for (t in 2:n.occasions) {
    cprob[t] <- gamma[t] * prod(qgamma[1:(t-1)])
  } # t

  psi <- sum(cprob[])                                 # Inclusion probability

  for(t in 1:n.occasions) {
    b[t] <- cprob[t]/psi                              # Entry probability
  } # t

  for(i in 1:M) {
    recruit[i,1] <- z[i,1]

    for (t in 2:n.occasions) {
      recruit[i,t] <- (1-z[i,t-1]) * z[i,t]
    }
  }
    
  for(t in 1:n.occasions) {
    Nflag[t]   <- sum( z[1:M,t] )
    Nstopadult[t]   <- sum( z[1:M,t] )/pflag[t]
    #Nstop[t] <- sum(( z[1:M,t] )/pflag)/meanpadult
    Nstop_t[t] <- sum(( z[1:M,t] )/pflag[t])/padult[t]
    Bflag[t]   <- sum(recruit[1:M,t])                     
    Bstopadult[t]  <- sum(recruit[1:M,t])/pflag[t]
    #Bstop[t] <- sum((recruit[1:M,t])/pflag)/meanpadult
    Bstop_t[t] <- (sum((recruit[1:M,t]))/pflag[t])/padult[t]             
  } 
    
  for(i in 1:M) {
    Nind[i] <- sum(z[i,1:n.occasions])
    Nalive[i] <- 1 - equals(Nind[i],0)
  }
    
  Nsuperflag <- sum(Nalive[])  
  #Nsuperflag2 <- sum(Bflag[])                         
  #Nsuperstopadult <- sum(Nalive[])/pflag
  #Nsuperstop <- sum((Nalive[])/pflag)/meanpadult
  #Nsuperstop_B <- sum(Bstop[])
  Nsuperstopadult <- sum(Bstopadult[])
  Nsuperstop_Bt <- sum(Bstop_t[])
    
# ------------------- Stopover duration -----------------------------------

  for (i in 1:n.ch) {
    zstop[i] <- sum(z[i,1:n.occasions])*3
  }

  zes.days <- mean(zstop[])

# ------------------- end : Stopover duration ---------------------------------
    
}
    ",fill = TRUE)
sink()