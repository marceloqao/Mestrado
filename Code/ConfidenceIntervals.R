require(Hmisc)

quant = function(x){quantile(x, probs=c(1/1000, 10/1000, 50/1000, 100/1000), type=4)}
quantis.por.fator <- summarize(HC$dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "0.1%", "1%", "5%", "10%")

quantis.por.fator

