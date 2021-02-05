sink("js-super.jags")
cat("
    model {
    # Priors and constraints
    for (i in 1:M){
    for (t in 1:(n.occasions-1)){
    phi[i,t] <- phi_t[t]
    } #t
    for (t in 1:n.occasions){
    p[i,t] <- psight[t]
    } #t
    } #i
    
    # Persistence
    
    for (t in 1:(n.occasions-1)){
    phi_t[t] ~ dunif(0,1)
    }
    
    # Resight
    
    p.first ~ dunif(0,1)
    p.last ~ dunif(0,1)
    
    for (t in 1:2) {
    psight[t] <- p.first
    }
    
    for (t in 3:(n.occasions-2)) {
    psight[t] ~ dunif(0,1)
    }
    
    for (t in (n.occasions-1):n.occasions) {
    psight[t] <- p.last
    }
    
    # Prior for inclusion probability

    psi ~ dunif(0, 1)              
    
    # Dirichlet prior for entry probabilities
    for (t in 1:n.occasions){
    beta[t] ~ dgamma(1, 1)
    b[t] <- beta[t] / sum(beta[1:n.occasions])
    }
    
    # Convert entry probs to conditional entry probs
    nu[1] <- b[1]
    for (t in 2:n.occasions){
    nu[t] <- b[t] / (1-sum(b[1:(t-1)]))
    } #t
    
    # Likelihood
    for (i in 1:M){
    # First occasion
    # State process
    w[i] ~ dbern(psi)                  # Draw latent inclusion
    z[i,1] ~ dbern(nu[1])
    # Observation process
    mu1[i] <- z[i,1] * p[i,1] * w[i]
    y[i,1] ~ dbern(mu1[i])
    
    # Subsequent occasions
    for (t in 2:n.occasions){
    # State process
    q[i,t-1] <- 1-z[i,t-1]
    mu2[i,t] <- phi[i,t-1] * z[i,t-1] + nu[t] * prod(q[i,1:(t-1)]) 
    z[i,t] ~ dbern(mu2[i,t])
    # Observation process
    mu3[i,t] <- z[i,t] * p[i,t] * w[i]
    y[i,t] ~ dbern(mu3[i,t])
    } #t
    } #i 
    
    # ------------- flag scans ---------------------------------  
    # # Binomial model for flag scans
    # 
    # # Priors
    # 
    # pflag ~ dbeta(1,1)
    # 
    # # Likelihood
    # 
    # for (i in 1:n) {
    # m[i] ~ dbin(pflag,K[i])
    # }
    
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
    padult[j] <- 1/(1 + exp(-beta.rper[j])) # convert logit to prob/proportion
    }
    
    meanpadult <- mean(padult[])
    
    # ------------- end : age scans ---------------------------------
    
    # Calculate derived population parameters
    for (i in 1:M){
    for (t in 1:n.occasions){
    u[i,t] <- z[i,t]*w[i]     # Deflated latent state (u)
    }
    }
    for (i in 1:M){
    recruit[i,1] <- u[i,1]
    for (t in 2:n.occasions){
    recruit[i,t] <- (1-u[i,t-1]) * u[i,t]
    } #t
    } #i
    
    for (t in 1:n.occasions){
    Nflag[t] <- sum(u[1:M,t])                         # Number of flagged birds
    Nstopadult[t]   <- sum( u[1:M,t] )/pflag[t]
    #Nstop[t] <- sum(( u[1:M,t] )/pflag)/meanpadult
    Nstop_t[t] <- sum(( u[1:M,t] )/pflag[t])/padult[t]
    Bflag[t] <- sum(recruit[1:M,t])                   # Number of flagged entries
    Bstopadult[t]  <- sum(recruit[1:M,t])/pflag[t]
    #Bstop[t] <- sum((recruit[1:M,t])/pflag)/meanpadult
    Bstop_t[t] <- (sum((recruit[1:M,t]))/pflag[t])/padult[t]
    } #t
    
    for (i in 1:M){
    Nind[i] <- sum(u[i,1:n.occasions])
    Nalive[i] <- 1-equals(Nind[i], 0)
    } #i
    
    Nsuperflag <- sum(Nalive[])         # Superpopulation size
    #Nsuperflag2 <- sum(Bflag[])                         
    #Nsuperstopadult <- sum(Nalive[])/pflag
    #Nsuperstop <- sum((Nalive[])/pflag)/meanpadult
    #Nsuperstop_B <- sum(Bstop[])
    Nsuperstopadult <- sum(Bstopadult[])
    Nsuperstop_Bt <- sum(Bstop_t[])
    
    # ------------------- Stopover duration -----------------------------------
    for (i in 1:n.ch) {
    zstop[i] <- sum(u[i,1:n.occasions])*3
    }
    zes.days <- mean(zstop[])
    # ------------------- end : Stopover duration ---------------------------------
    
    }
    ",fill=TRUE)
sink()

