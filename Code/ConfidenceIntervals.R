require(Hmisc)

attach(HC)

quant = function(x){quantile(x, probs=c(900/1000, 950/1000, 990/1000, 999/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "90%", "95%", "99%", "99.9%")

quantis.por.fator

dim(quantis.por.fator)
typeof(quantis.por.fator)

Rt1N1000 <- subset(quantis.por.fator, Source=="Radio" & tau=="1" & N=="1000")
Rt1N1000
plot(x=c(min(Rt1N1000$`90%`), max(Rt1N1000$`99.9%`)), y=c(3,6), type="n")
lines(x=Rt1N1000$`90%`, y=strtoi(Rt1N1000$D), col='red')
lines(x=Rt1N1000$`95%`, y=strtoi(Rt1N1000$D), col='black')
lines(x=Rt1N1000$`99%`, y=strtoi(Rt1N1000$D), col='blue')
lines(x=Rt1N1000$`99.9%`, y=strtoi(Rt1N1000$D), col='green')

plot(x=c(min(Rt1N1000$`90%`), max(Rt1N1000$`99.9%`)), y=c(3,6), type="n", log="x")
lines(x=Rt1N1000$`90%`, y=strtoi(Rt1N1000$D), col='red')
lines(x=Rt1N1000$`95%`, y=strtoi(Rt1N1000$D), col='black')
lines(x=Rt1N1000$`99%`, y=strtoi(Rt1N1000$D), col='blue')
lines(x=Rt1N1000$`99.9%`, y=strtoi(Rt1N1000$D), col='green')
