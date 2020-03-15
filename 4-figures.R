# Figure 1
l <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
cairo_pdf("Figure1.pdf", width=10, height=8)
par(mfrow=c(3,1))
with(dat, plot(time, rep, type="h", lend=1, lwd=10, col="limegreen",
    main="Reported cases in Wuhan", xlab="Date", bty="l", 
    ylab="Number of cases"))
with(dat, plot(time, inf, type="h", lend=1, lwd=10, col="red",
    main="Estimated symptomatic cases in Wuhan", xlab="Date", bty="l", 
    ylab="Number of cases"))
with(dat, plot(time, exp, type="h", lend=1, lwd=10, col="orange",
    main="Estimated infections in Wuhan", xlab="Date", bty="l", 
    ylab="Number of infections"))
dev.off()
Sys.setlocale("LC_TIME", l)


# Figure 2
l <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
cairo_pdf("Figure2.pdf", width=10, height=6)
plot(seq.Date(dat$time[1], rev(dat$time)[1]+13, by="day"), rep(0, nrow(dat)+13), 
    type="n", ylim=c(0, 12000000), bty="l", yaxt="n", yaxs="i",
    xlab="Date", ylab="Number of persons", 
    main="Estimated course of the COVID-19 epidemic in Wuhan")
axis(2, at=c(5,10)*10^6, label=c("5,000,000", "10,000,000")) 
axis(2, at=c(1:11)*10^6, label=NA, tcl=-0.2) 
points(dat$time, 11000000 - E[,1] - I[,1] - R[,1], type="l", lwd=2, col="skyblue")
points(dat$time, E[,1], type="l", lwd=2, col="orange")
points(dat$time, I[,1], type="l", lwd=2, col="red")
points(dat$time, R[,1], type="l", lwd=2, col="limegreen")
abline(h=11000000, lwd=2, lty="dashed")
text(x=as.Date("2020-2-24"), y=11000000, "Total population of Wuhan", pos=3)
legend("left", c("Susceptible", "Exposed", "Infected", "Removed"), lwd=2,
    col=c("skyblue", "orange", "red","green"), 
    seg.len=7, bty="n", inset=c(0.015,0.10))
dev.off()
Sys.setlocale("LC_TIME", l)


# Figure 3 
cairo_pdf("Figure3.pdf", width=10, height=8)
par(mfrow=c(2,1))
plot(density(unlist(res[,"R0"])), lwd=2, yaxt="n", bty="l", 
    xlab=NA, ylab=NA, col="purple",
    main=expression(paste("Basic Reproduction Number ", (R[0]))))
mtext("Density", side=2, line=1)
plot(density(unlist(res[,"asc"])), lwd=2, yaxt="n", bty="l", 
    xlab=NA, ylab=NA, col="blue", xaxt="n",
    main=expression(paste("Ascertainment Rate (%)")))
axis(1, at=axTicks(1), label=axTicks(1)*100)
mtext("Density", side=2, line=1)
dev.off()


# Figure S1
library(shape)
cairo_pdf("FigureS1.pdf", width=10, height=8)
par(mfrow=c(2,1), mar=c(5,4,2,2))
curve(dweibull(x, shape=4.4, scale=22.3), from=0, to=30, 
    ylim=c(0,0.12), bty="l", lwd=2, col="blue", 
    ylab="Probability density",
    xlab="Time from infection to case confirmation and reporting")
curve(dweibull(x, shape=2.6, scale=12.1), from=0, to=30, 
    add=TRUE, lwd=2, col="green")
Arrows(19, 0.075, 13,0.085)
legend("topright", c("Distribution on January 10, 2020", 
    "Distribution on February 10, 2020"), 
    col=c("blue","green"), bty="n", seg.len=6, lwd=2)
curve(dweibull(x, shape=8, scale=15.7), from=0, to=30, 
    ylim=c(0,0.27), bty="l", lwd=2, col="blue", 
    ylab="Probability density",
    xlab="Time from symptom onset to case confirmation and reporting")
curve(dweibull(x, shape=4, scale=5.5), from=0, to=30, 
    add=TRUE, lwd=2, col="green")
Arrows(14, 0.18, 7,0.24)
legend("topright", c("Distribution on January 10, 2020", 
    "Distribution on February 10, 2020"), 
    col=c("blue","green"), bty="n", seg.len=6, lwd=2)
dev.off()
