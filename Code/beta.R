set.seed(1234567890)

z <- rnorm(1000)
est1k <- data.frame()
for (beta in c(seq(.01,.19, by=.01),seq(.2,.6, by=.1))){
  est1k <- rbind(est1k, filter(z, filter=c(beta,1,beta), circular=TRUE))
}
for(n in 1:length(est1k[,1])){
  est1k[n,] <- est1k[n,]/max(est1k[n,])
}

# est_0.1 <- filter(z, filter=rep(0.1,3), circular=TRUE)
## Loading Bandt&Pompe Functions

source("bandt_pompe/bandt_pompe.R")
source("bandt_pompe/measures.R")
source("bandt_pompe/features.R")
source("bandt_pompe/visibility.R")
source("bandt_pompe/helpers.R")

#############################################
## Calculating HC ...
## 
Points_Est_1k_D6_t1 <- data.frame()
for(p in 1:length(est1k[,1])) {
    Points_Est_1k_D6_t1 <- rbind(Points_Est_1k_D6_t1, complexity_entropy(est1k[p,], 6, 1))
  }  
names(Points_Est_1k_D6_t1)=c("H", "C", "JS")

Points_Est_1k_D6_t1$dEuclid = sqrt((Points_Est_1k_D6_t1$H-1)^2 + Points_Est_1k_D6_t1$C^2)
Points_Est_1k_D6_t1$beta <- cbind( c(seq(.01,.19, by=.01),seq(.2,.6, by=.1)))

#  calcular HxC
#  calcular d

## Plotting

source("DataSetup.R")

require(Hmisc)

attach(HC_no_MT)
quant = function(x){quantile(x, probs=c(900/1000, 950/1000, 990/1000, 999/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "90%", "95%", "99%", "99.9%")
#detach(HC_no_MT)

## N=1000 Tau = 1
N.Atual <- "1000" #1000 ou 50k
tau.Atual <- "1" #1, 10, 30 ou 50

HCN1000tau1melt <- melt(subset(HC_no_MT, N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
HCN1000tau1melt <- HCN1000tau1melt[,c(5,9)]
names(HCN1000tau1melt) <- c("D", "dEuclid")
HCN1000tau1meltQuant <- summarize(dEuclid, llist(D), quant)
names(HCN1000tau1meltQuant) <- c("D", "90%", "95%", "99%", "99,9%") 

Points_MT_melt <- melt(subset(Points_MT,N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
Points_MT_melt <- Points_MT_melt[,c(1,7)]

Points_Randu_melt <- melt(subset(Points_Randu,N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
Points_Randu_melt <- Points_Randu_melt[,c(1,7)]

### Plotando ... MT e RANDU
tt <- melt(HCN1000tau1meltQuant)

ggplot() +
  geom_segment(data=subset(tt, D==6), aes(0, value, xend = .65, yend = value, color = variable), alpha=.3) +
  geom_point(data=Points_Est_1k_D6_t1, aes(beta, dEuclid), size=.5) +
  xlab("Beta") +
  ylab("Distância Euclidiana") +
  scale_color_discrete(name="beta") +
  theme_light() 

## Select points that are under the confidence intervals
#
subset(Points_Est_1k_D6_t1, dEuclid<=(subset(tt, D==6 & tt$variable=="90%")))
subset(Points_Est_1k_D6_t1, dEuclid>=(subset(tt, D==6 & tt$variable=="90%")) & dEuclid<=(subset(tt, D==6 & tt$variable=="95%")))
subset(Points_Est_1k_D6_t1, dEuclid>=(subset(tt, D==6 & tt$variable=="95%")) & dEuclid<=(subset(tt, D==6 & tt$variable=="99%")))
