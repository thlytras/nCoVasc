# Run script 1-downloadData.R first, to create dat.RData
load("dat.RData")

# Helper function
neg0 <- function(x) { x[x<0] <- 0; x[is.na(x)] <- 0; x }

# Set up data.frame
dat <- data.frame(time=seq.Date(as.Date("2020-1-3"), wuhan$time[nrow(wuhan)], by="day"))
dat <- merge(dat, wuhan, all.x=TRUE)
dat$rep <- c(NA, diff(dat$cum_confirm))
dat <- dat[-1, c("time","rep")]
rownames(dat) <- NULL

# Trim the retrospectively reported cases on 2020-2-12 
# and assign them proportionally to the past
est40 <- round(mean(dat$rep[c(39,41)]))
dat$rep[1:39] <- dat$rep[1:39] + round(dat$rep[1:39]/sum(dat$rep[1:39])*(dat$rep[40]-est40))
dat$rep[40] <- est40




# 8/15.7 -> 4/5.5
# Set up vectors for the shape and scale parameters
shW <- c(rep(8, 6), seq(8, 4, length.out=32))[1:nrow(dat)]
shW[is.na(shW)] <- 4
scW <- c(rep(15.7, 6), seq(15.7, 5.5, length.out=32))[1:nrow(dat)]
scW[is.na(scW)] <- 5.5

# Estimate times of symptom onset (infectious) from the times of reporting 
# (Disregard first 12 rows, i.e. the time before 2020-1-16)
M2 <- t(sapply(1:(nrow(dat)-12+1), function(i) {
  propSymp <- rev(diff(pweibull(0:30, shape=shW[i], scale=scW[i])))
  propSymp <- propSymp/sum(propSymp)
  c(rep(0,i-1), propSymp, rep(0, nrow(dat)-12+1-i+1))
}))[,-(1:(30-12+1))]
dat$inf <- round(colSums(M2*dat$rep[12:nrow(dat)]))

# --------

# 4.4/22.3 -> 2.6/12.1
# Set up vectors for the shape and scale parameters
shW <- c(rep(4.4, 6), seq(4.4, 2.6, length.out=32))[1:nrow(dat)]  # From 5 to 5
shW[is.na(shW)] <- 2.6
scW <- c(rep(22.3, 6), seq(22.3, 12.1, length.out=32))[1:nrow(dat)] # From 21 (10 Jan) to 10 (10 Feb)
scW[is.na(scW)] <- 12.1

# Estimate times of exposure from the times of reporting 
# (Disregard first 12 rows, i.e. the time before 2020-1-16)
M <- t(sapply(1:(nrow(dat)-12+1), function(i) {
  propSymp <- rev(diff(pweibull(0:30, shape=shW[i], scale=scW[i])))
  propSymp <- propSymp/sum(propSymp)
  c(rep(0,i-1), propSymp, rep(0, nrow(dat)-12+1-i+1))
}))[,-(1:(30-12+1))]
dat$exp <- round(colSums(M*dat$rep[12:nrow(dat)]))



# Trim the last 15 days in the dataset, to adjust for delays in case ascertainment
dat <- dat[1:(nrow(dat)-15),]

