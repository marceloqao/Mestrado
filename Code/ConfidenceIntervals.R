attach(HC)

dEuclid.quantiles = data.frame()

q <- c(1/1000, 10/1000, 50/1000, 100/1000)


for(s in 1:length(levels(Source))) {
  for(n in 1:length(levels(N))) {
    for(d in 1:length(levels(D))) {
      for(t in 1:length(levels(tau))) {
        dEuclid.current <- subset(HC$dEuclid, 
                                  Source==levels(Source)[s] & 
                                  N==levels(N)[n] & 
                                  D==levels(D)[d] & 
                                  tau==levels(tau)[t]
                                  )
        dEuclid.range <- range(dEuclid.current)
        print(dEuclid.range)
        dEuclid.quantiles <- rbind(dEuclid.quantiles, 
                                        c(dEuclid.range[1], quantile(dEuclid.current, q, type=4), dEuclid.range[2],
                                 levels(Source)[s], levels(N)[n], levels(D)[d], levels(tau)[t]
                               )
        )
      }  
    }
  }
}

names(dEuclid.quantiles) <- c("min", "1/1000", "10/1000", "50/1000", "100/1000", "max", "Source", "N", "D", "tau")
