require(Hmisc)

attach(HC)

quant = function(x){quantile(x, probs=c(900/1000, 950/1000, 990/1000, 999/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "90%", "95%", "99%", "99.9%")

detach(HC)

HCN1000tau1melt <- melt(subset(HC, N=="1000" & tau=="1"))

quantis.por.fator

Rt1N1000 <- subset(quantis.por.fator, Source=="Radio" & tau=="1" & N=="50k")[,-c(1,2,4)]
summary(Rt1N1000)
Rt1N1000

### Usando ggplot
meltRt1N1000 <- melt(Rt1N1000) 

HCN1000tau1melt <- melt(subset(HC, N=="1000" & tau=="1"), measure.vars = "dEuclid")
HCN1000tau1melt <- HCN1000tau1melt[,c(5,9)]
names(HCN1000tau1melt) <- c("D", "dEuclid")
HCN1000tau1meltQuant <- summarize(dEuclid, llist(D), quant)
names(HCN1000tau1meltQuant) <- c("D", "90%", "95%", "99%", "99,9%")

(plot2 <- ggplot(NULL, aes(D, dEuclid)) + 
    geom_point(data = HCN1000tau1meltQuant) +
    geom_line(group=dEuclid)
    #geom_step(data = )
)

ggplot(data = HCN1000tau1meltQuant,
       aes(x=, y=D)
       )

#ggplot(data=meltRt1N1000,
# aes(x=value, y=D, colour = value, alpha=.1)) +

ggplot(data=HCN1000tau1melt, 
       aes(x=value, y=D)) +
  # geom_point(aes(x=dEuclid, y=value, )) +
  # geom_line(aes(group = D)) 
  geom_point(size=2, alpha=1) +
  scale_colour_gradient(low = "yellow", high = "black", guide = "legend")
  #scale_color_discrete(name = "Quantis") +
  #theme(legend.position = "bottom")

#############



# plot(x=c(min(Rt1N1000$`90%`), max(Rt1N1000$`99.9%`)), y=c(3,6), type="n")
# lines(x=Rt1N1000$`90%`, y=strtoi(Rt1N1000$D), col='red')
# lines(x=Rt1N1000$`95%`, y=strtoi(Rt1N1000$D), col='black')
# lines(x=Rt1N1000$`99%`, y=strtoi(Rt1N1000$D), col='blue')
# lines(x=Rt1N1000$`99.9%`, y=strtoi(Rt1N1000$D), col='green')
# 
# plot(x=c(min(Rt1N1000$`90%`), max(Rt1N1000$`99.9%`)), y=c(3,6), type="n", log="x")
# lines(x=Rt1N1000$`90%`, y=strtoi(Rt1N1000$D), col='red')
# lines(x=Rt1N1000$`95%`, y=strtoi(Rt1N1000$D), col='black')
# lines(x=Rt1N1000$`99%`, y=strtoi(Rt1N1000$D), col='blue')
# lines(x=Rt1N1000$`99.9%`, y=strtoi(Rt1N1000$D), col='green')
