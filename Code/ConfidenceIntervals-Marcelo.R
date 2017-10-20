require(Hmisc)
#rm(quantis.por.fator)

attach(HC)
# quant = function(x){quantile(x, probs=c(100/1000, 50/1000, 10/1000, 1/1000), type=4)}
quant = function(x){quantile(x, probs=c(1/1000, 10/1000, 50/1000, 100/1000), type=4)}
quantis.por.fator <- summarize(dEuclid, llist(D,N,tau,Source),quant)
names(quantis.por.fator) <- c("D", "N", "tau", "Source", "0.1%", "1%", "5%", "10%")
# names(quantis.por.fator) <- c("D", "N", "tau", "Source", "10%", "5%", "1%", "0.1%")
detach(HC)

Rt1N1000 <- subset(quantis.por.fator, Source=="Radio" & tau=="1" & N=="1000")
plot(x=c(0, 0.17), y=c(3,6), type="n")
lines(x=Rt1N1000$"0.1%", y=strtoi(Rt1N1000$D), col='red')
lines(x=Rt1N1000$'1%', y=strtoi(Rt1N1000$D), col='black')
lines(x=Rt1N1000$'5%', y=strtoi(Rt1N1000$D), col='blue')
lines(x=Rt1N1000$'10%', y=strtoi(Rt1N1000$D), col='green')

plot(x=c(10^-6, 0.17), y=c(3,6), type="n", log="x")
lines(x=Rt1N1000$'0.1%', y=strtoi(Rt1N1000$D), col='red')
lines(x=Rt1N1000$'1%', y=strtoi(Rt1N1000$D), col='black')
lines(x=Rt1N1000$'5%', y=strtoi(Rt1N1000$D), col='blue')
lines(x=Rt1N1000$'10%', y=strtoi(Rt1N1000$D), col='green')

ggplot(Rt1N1000, )



ggplot(subset(quantis.por.fator, N=="1000" & tau=="1" & Source=="1000" & Source=="50k"),
       aes(x = quantis.por.fator)) +
  labs(x = "Quantis",
       y = "D") +
  # geom_segment(aes(x = quantis.por.fator$D ,
  #                  y = category,
  #                  xend = quantis.por.fator$D,
  #                  yend = category,
  #              size = 1)) +
  geom_point(aes(x = quantis.por.fator$`0.1%`,
                 color = "0.1"),
             size = 4, shape = 15) +
  geom_point(aes(x = data$"1960",
                 color = "1960"),
             size = 4, shape = 15) +
  scale_color_discrete(name = "Year") +
  theme(legend.position = "bottom")

ggplot(data=subset(quantis.por.fator, N=="1000" & tau=="1"), aes(x=D, y=probs)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  # geom_line(data = inf, aes(x=H, y=Cinf)) +
  # geom_line(data = sup, aes(x=H, y=Cmax)) +
  # scale_x_continuous(limits = c(min(subset(HC, N=="50k" & Source=="Radio")$H), 1)) +
  # scale_y_continuous(limits = c(0, max(subset(HC, N=="50k" & Source=="Radio")$C))) +
  #facet_grid(D ~ quant) +
  theme_light()


#<<<<<<< Updated upstream
quant = function(x){quantile(x, probs=c(1/1000, 10/1000, 50/1000, 100/1000), type=4)}
quantis.por.fator <- summarize(HC$dEuclid, llist(Source, N, D, tau), quant)
names(quantis.por.fator) <- c( "Source", "N", "D", "tau", "0.1%", "1%", "5%", "10%")
#=======
dEuclid.quantiles = data.frame()
##
# q <- c(1/1000, 10/1000, 50/1000, 100/1000)
# 
# for(s in 1:length(levels(Source))) {
#   for(n in 1:length(levels(N))) {
#     for(d in 1:length(levels(D))) {
#       for(t in 1:length(levels(tau))) {
#         HC.current <- subset(HC, 
#                                   Source==levels(Source)[s] & 
#                                   N==levels(N)[n] & 
#                                   D==levels(D)[d] & 
#                                   tau==levels(tau)[t]
#                                   )
#         dEuclid.range <- range(HC.current$dEuclid)
#         print(dEuclid.range)
#         dEuclid.quantiles <- rbind(dEuclid.quantiles, 
#                                c(dEuclid.range[1], 
#                                  quantile(HC.current$dEuclid, q, type=4),
#                                  dEuclid.range[2],
#                                  levels(Source)[s], levels(N)[n], levels(D)[d], levels(tau)[t]
#                                )
#         )
#       }  
#     }
#   }
# }
##


dEuclid.quantiles = data.frame()
>>>>>>> Stashed changes

quantis.por.fator

<<<<<<< Updated upstream
=======
for(s in 1:length(levels(Source))) {
  for(n in 1:length(levels(N))) {
    for(d in 1:length(levels(D))) {
      for(t in 1:length(levels(tau))) {
        HC.current <- subset(HC, 
                             Source==levels(Source)[s] & 
                               N==levels(N)[n] & 
                               D==levels(D)[d] & 
                               tau==levels(tau)[t]
        )
        dEuclid.range <- range(HC.current$dEuclid)
        print(dEuclid.range)
        dEuclid.quantiles <- rbind(dEuclid.quantiles, 
                                   c(dEuclid.range[1], 
                                     quantile(HC.current$dEuclid, q, type=4),
                                     dEuclid.range[2],
                                     levels(Source)[s], levels(N)[n], levels(D)[d], levels(tau)[t]
                                    )
                                   )
      }  
    }
  }
}


names(dEuclid.quantiles) <- c("min", "1/1000", "10/1000", "50/1000", "100/1000", "max", "Source", "N", "D", "tau")
>>>>>>> Stashed changes
