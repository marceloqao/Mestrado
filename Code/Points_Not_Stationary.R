## Séries não Estacionárias
## 
NoEst1k <- diffinv(rnorm(1000))
NoEst1k <- abs(NoEst1k/max(NoEst1k))

# NoEst50k <- diffinv(rnorm(50000))
# NoEst50k <- abs(NoEst50k/max(NoEst50k))

## Loading Bandt&Pompe Functions

source("bandt_pompe/bandt_pompe.R")
source("bandt_pompe/measures.R")
source("bandt_pompe/features.R")
source("bandt_pompe/visibility.R")
source("bandt_pompe/helpers.R")

############################################
## Calculating HC ...
## 1000
# Start the clock!
ptm <- proc.time()
Points_Est1k <- data.frame()
for(r in 3:6) {
  for(t in c(1,10,30,50)) {
    Points_Est1k <- rbind(Points_Est1k, c(r,t,complexity_entropy(NoEst1k, r, t)))
  }  
}
# Stop the clock
proc.time() - ptm
names(Points_Est1k)=c("D", "tau", "H", "C", "JS")
Points_Est1k <- Points_Est1k[,-5]
Points_Est1k$dEuclid <- sqrt((Points_Est1k$H-1)^2 + Points_Est1k$C^2)

## Organize
Points_Est1k$D <- as.factor(Points_Est1k$D)
Points_Est1k$tau <- as.factor(Points_Est1k$tau)
Points_Est1k$N <- "1000"
Points_Est1k$N <- as.factor(Points_Est1k$N)
Points_Est1k$dEuclid = sqrt((Points_Est1k$H-1)^2 + Points_Est1k$C^2)

# ############################################
# ## Calculating HC ...
# ## 50k
# # Start the clock!
# ptm <- proc.time()
# Points_Est50k <- data.frame()
# for(r in 3:6) {
#   for(t in c(1,10,30,50)) {
#     Points_Est50k <- rbind(Points_Est50k, c(r,t,complexity_entropy(NoEst50k, r, t)))
#   }  
# }
# # Stop the clock
# proc.time() - ptm
# names(Points_Est50k)=c("D", "tau", "H", "C", "JS")
# Points_Est50k <- Points_Est50k[,-5]
# 
# ## Organize
# Points_Est50k$D <- as.factor(Points_Est50k$D)
# Points_Est50k$tau <- as.factor(Points_Est50k$tau)
# Points_Est50k$N <- "50k"
# Points_Est50k$N <- as.factor(Points_Est50k$N)
# Points_Est50k$dEuclid = sqrt((Points_Est50k$H-1)^2 + Points_Est50k$C^2)

## Putting things together
# Points_Est <- rbind(Points_Est1k, Points_Est50k)
Points_Est <- Points_Est1k
#############################################
## Prepare Plotting

source("DataSetup.R")

require(Hmisc)

## Gerando quantis
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

Points_Est_melt <- melt(subset(Points_Est,N==N.Atual & tau==tau.Atual), measure.vars = "dEuclid")
Points_Est_melt <- Points_Est_melt[,c(1,7)]

### Plotando ...
tt <- melt(HCN1000tau1meltQuant)

ggplot() +
  geom_line(data=tt, aes(D, value, color=variable, group=variable), alpha=.3) +
  #geom_point(data=HCN1000tau1melt, aes(D,dEuclid), alpha=.5) +
  #geom_point(data=tt, aes(D, value, color = variable), size=.5) +
  # geom_segment(data=tt, aes(as.numeric(D)-.05, value, xend = as.numeric(D)+.05, yend = value, color = variable), alpha=.3) +
  geom_segment(data=tt, aes(as.numeric(D)-.05, value, xend = as.numeric(D)+.05, yend = value)) +
  #geom_point(data=Points_MT_melt, aes(D,value), colour = "red", size = 2) +
  geom_point(data=Points_Est_melt, aes(D,value), colour = "red", size = 2) +
  scale_color_discrete(name="Quantis") +
  theme_light() +
  xlab("D") +
  ylab("Distância Euclidiana") +
  labs(caption = paste("N =", N.Atual, "tau =", tau.Atual))

ggsave("ConfidInt_nao_estacionaria_1k_t1.png", plot = last_plot(), device = "png", path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)
