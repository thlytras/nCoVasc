library(nCov2019) # install with remotes::install_github("GuangchuangYu/treeio")

b <- load_nCov2019(lang="en")
wuhan <- subset(b["Hubei"], city=="Wuhan")
hubei_rest <- subset(b["Hubei"], city!="Wuhan")

rownames(wuhan) <- NULL
rownames(hubei_rest) <- NULL

# Freeze the data at 2020-3-9
wuhan <- subset(wuhan, time <= "2020-3-9")
hubei_rest <- subset(hubei_rest, time <= "2020-3-9")

# Save the data
save(wuhan, hubei_rest, file="dat.RData")


