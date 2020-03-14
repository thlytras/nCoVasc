# Run script 2-prepData.R first

library(rjags)

# Set up data for JAGS
datJ <- list(
  N = 11000000, # Wuhan population
  T = nrow(dat), # Timepoints in the data
  r = c(rep(1,19), rep(2,13), rep(3,(nrow(dat)-19-13))),
  dE = dat$exp, # New exposures (infections)
  dI = dat$inf, # New cases (infectious, by symptom onset)
  dR = dat$rep # New removals (not infectious anymore)
)

model <- jags.model(file = "sens2-model.jag", data = datJ, n.chains = 4, n.adapt = 5000)

res <- coda.samples(model, var=c("beta", "gamma", "kappa", "R0", "E", "I", "R"), n.iter=20000)

summ <- summary(res[,c(
    grep("beta\\[", varnames(res)), 
    grep("gamma", varnames(res)), 
    grep("kappa", varnames(res)), 
    grep("R0\\[", varnames(res)))])[[2]][,c(3,1,5)]

E <- summary(res[,grep("E\\[", varnames(res))])[[2]][,c(3,1,5)]
I <- summary(res[,grep("I\\[", varnames(res))])[[2]][,c(3,1,5)]
R <- summary(res[,grep("R\\[", varnames(res))])[[2]][,c(3,1,5)]

save(res, summ, E, I, R, file="sens2-results.RData")

