## Generating Sequences:
## MT
set.seed(1234567890, kind = "Mersenne-Twister")

MT1k <- runif(1000)
MT50k <- runif(50000)

## Randu
seed <- as.double(1)
RANDU <- function() {
  seed <<- ((2^16 + 3) * seed) %% (2^31)
  seed/(2^31)
}

Randu1k <- vector()
for(i in 1:1000) {
  
  Randu1k[i] <- c(RANDU())
}

Randu50k <- vector()
for(i in 1:50000) {
  
  Randu50k[i] <- c(RANDU())
}

## Loading Bandt&Pompe Functions

source("bandt_pompe/bandt_pompe.R")
source("bandt_pompe/measures.R")
source("bandt_pompe/features.R")
source("bandt_pompe/visibility.R")
source("bandt_pompe/helpers.R")

#############################################
## Calculating HC ...
## MT
Points_MT_1k <- data.frame()
for(p in 3:6) {
  for(q in c(1,10,30,50)) {
    Points_MT_1k <- rbind(Points_MT_1k, c(p, q, complexity_entropy(MT1k, p, q)))
  }  
}
names(Points_MT_1k)=c("D", "tau", "H", "C", "JS")
Points_MT_1k <- Points_MT_1k[,-5]
## Putting things together
Points_MT_1k$D <- as.factor(Points_MT_1k$D)
Points_MT_1k$tau <- as.factor(Points_MT_1k$tau)
Points_MT_1k$N <- "1000"
Points_MT_1k$N <- as.factor(Points_MT_1k$N)
Points_MT_1k$dEuclid = sqrt((Points_MT_1k$H-1)^2 + Points_MT_1k$C^2)

## 50k
Points_MT_50k <- data.frame()
for(n in 3:6) {
  for(m in c(1,10,30,50)) {
    data <- MT50k
    Points_MT_50k <- rbind(Points_MT_50k, c(n, m, complexity_entropy(MT50k, n, m)))
  }  
}
names(Points_MT_50k)=c("D", "tau", "H", "C", "JS")
Points_MT_50k <- Points_MT_50k[,-5]
## Putting things together
Points_MT_50k$D <- as.factor(Points_MT_50k$D)
Points_MT_50k$tau <- as.factor(Points_MT_50k$tau)
Points_MT_50k$N <- "50k"
Points_MT_50k$N <- as.factor(Points_MT_50k$N)
Points_MT_50k$dEuclid = sqrt((Points_MT_50k$H-1)^2 + Points_MT_50k$C^2)

Points_MT <- rbind(Points_MT_1k, Points_MT_50k)

# i=3
# ptm <- proc.time()
# complexity_entropy(MT_SEQ_50k, i, j)
# proc.time() - ptm
# usuário   sistema decorrido 
# 12.702     2.170    14.914 

#############################################
## Calculating HC ...
## RANDU
# Start the clock!
ptm <- proc.time()
Points_Randu_1k <- data.frame()
for(r in 3:6) {
  for(t in c(1,10,30,50)) {
    Points_Randu_1k <- rbind(Points_Randu_1k, c(r,t,complexity_entropy(Randu1k, r, t)))
  }  
}
# Stop the clock
proc.time() - ptm
names(Points_Randu_1k)=c("D", "tau", "H", "C", "JS")
Points_Randu_1k <- Points_Randu_1k[,-5]
## Putting things together
Points_Randu_1k$D <- as.factor(Points_Randu_1k$D)
Points_Randu_1k$tau <- as.factor(Points_Randu_1k$tau)
Points_Randu_1k$N <- "1000"
Points_Randu_1k$N <- as.factor(Points_Randu_1k$N)
Points_Randu_1k$dEuclid = sqrt((Points_Randu_1k$H-1)^2 + Points_Randu_1k$C^2)

# Start the clock!
ptm <- proc.time()
Points_Randu_50k <- data.frame()
for(w in 3:6) {
  for(z in c(1,10,30,50)) {
    Points_Randu_50k <- rbind(Points_Randu_50k, c(w, z, complexity_entropy(Randu50k, w, z)))
  }  
}
# Stop the clock
proc.time() - ptm
names(Points_Randu_50k)=c("D", "tau", "H", "C", "JS")
Points_Randu_50k <- Points_Randu_50k[,-5]
## Putting things together
Points_Randu_50k$D <- as.factor(Points_Randu_50k$D)
Points_Randu_50k$tau <- as.factor(Points_Randu_50k$tau)
Points_Randu_50k$N <- "50k"
Points_Randu_50k$N <- as.factor(Points_Randu_50k$N)
Points_Randu_50k$dEuclid <- sqrt((Points_Randu_50k$H-1)^2 + Points_Randu_50k$C^2)

Points_Randu <- rbind(Points_Randu_1k, Points_Randu_50k)

#############################################
## Plotting

source("DataSetup.R")

require(Hmisc)

attach(HC_no_MT)
quant = function(x){quantile(x, probs=c(900/1000, 950/1000, 990/1000, 999/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "90%", "95%", "99%", "99.9%")
detach(HC_no_MT)

## N=1000 Tau = 1
N.Atual <- "1000" #1000 ou 50k
tau.Atual <- "10" #1, 10, 30 ou 50

HCN1000tau1melt <- melt(subset(HC_no_MT, N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
HCN1000tau1melt <- HCN1000tau1melt[,c(5,9)]
names(HCN1000tau1melt) <- c("D", "dEuclid")
HCN1000tau1meltQuant <- summarize(dEuclid, llist(D), quant)
names(HCN1000tau1meltQuant) <- c("D", "90%", "95%", "99%", "99,9%") 

Points_MT_melt <- melt(subset(Points_MT,N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
Points_MT_melt <- Points_MT_melt[,c(1,7)]

Points_Randu_melt <- melt(subset(Points_Randu,N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
Points_Randu_melt <- Points_Randu_melt[,c(1,7)]

### Plotando ...
tt <- melt(HCN1000tau1meltQuant)

ggplot() +
  geom_line(data=tt, aes(D, value, color=variable, group=variable), alpha=.3) +
  geom_point(data=HCN1000tau1melt, aes(D,dEuclid), alpha=.5) +
  geom_point(data=tt, aes(D, value, color = variable), size=.5) +
  geom_point(data=Points_MT_melt, aes(D,value), colour = "red", size = 2) +
  geom_point(data=Points_Randu_melt, aes(D,value), colour = "yellow", size = 2) +
  scale_color_discrete(name="Quantis") +
  theme_light() +
  xlab("D") +
  ylab("Distância Euclidiana") +
  labs(caption = paste("N =", N.Atual, "tau =", tau.Atual))

