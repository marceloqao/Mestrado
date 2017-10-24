require(Hmisc)

attach(HC_no_MT)
quant = function(x){quantile(x, probs=c(900/1000, 950/1000, 990/1000, 999/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "90%", "95%", "99%", "99.9%")
detach(HC_no_MT)

## N=1000 Tau = 1
HCN1000tau1melt <- melt(subset(HC_no_MT, N=="1000" & tau=="10"), measure.vars = "dEuclid")
# HCN1000tau1melt <- HC_no_MT
HCN1000tau1melt <- HCN1000tau1melt[,c(5,9)]
names(HCN1000tau1melt) <- c("D", "dEuclid")
HCN1000tau1meltQuant <- summarize(dEuclid, llist(D), quant)
names(HCN1000tau1meltQuant) <- c("D", "90%", "95%", "99%", "99,9%") 

#Gerar a tabela com os Quantis
HCN1000tau1meltQuant <- summarize(dEuclid, llist(N,D,tau), quant)
names(HCN1000tau1meltQuant) <- c("N", "D", "tau", "90%", "95%", "99%", "99,9%") 
#Gerar a tabela com os Quantis

### Plotando ...
tt <- melt(HCN1000tau1meltQuant)

ggplot() +
  geom_line(data=tt, aes(D, value, color=variable, group=variable), alpha=.3) +
  geom_point(data=HCN1000tau1melt, aes(D,dEuclid), alpha=.5) +
  geom_point(data=tt, aes(D, value, color = variable), size=.5) +
  scale_color_discrete(name="Quantis") +
  theme_light() +
  xlab("D") +
  ylab("Distância Euclidiana") +
  labs(caption = "(N=50.000, tau=50)")
  
  