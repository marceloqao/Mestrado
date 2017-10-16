# Global visualization of scatterplots

ggplot(data=subset(HC, N=="50k" & Source=="M-T"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="50k" & Source=="Radio")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="50k" & Source=="Radio")$C))) +
  facet_grid(tau ~ D) +
  theme_light()



# Acrescentando a distância euclidiana ao data.frame
HC$dEuclid = sqrt((HC$H-1)^2 + HC$C^2)



# Reducing data for testing
#HC_samp <- HC[sample(nrow(HC), 50000), ]


# Global visualization of scatterplots

ggplot(data=subset(HC, Source="Quantum"), aes(x=H, y=C)) + 
  geom_point() +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(minH, 1)) +
  scale_y_continuous(limits = c(0, maxC)) +
  facet_grid(tau ~ D) +
  theme_light()


### Todos juntos: sem sentido
ggplot(data=HC, aes(x=H, y=C)) + 
  geom_point(colour=cores[HC_samp$Source], alpha=0.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(minH, 1)) +
  scale_y_continuous(limits = c(0, maxC)) +
  facet_grid(tau ~ D) +
  theme_light()

# Visualização por grupos

D.current <- 3
tau.current <- 10
HC_subset <- subset(HC, D==D.current & tau==tau.current)
Cinf <- subset(inf, D==D.current)
Csup <- subset(sup, D==D.current)

# Interesting sorted distances
N <- length(HC_subset$dEuclid)
interesting <- round(c(1, .001*N, .01*N, .05*N, .1*N, N))

dsort <- sort(HC_subset$dEuclid, index.return=TRUE)
H.interesting <- HC_subset$H[dsort$ix[interesting]]
C.interesting <- HC_subset$C[dsort$ix[interesting]]
interesting.data <- data.frame(H.interesting, C.interesting)

minH <- min(HC_subset$H)
maxC <- max(HC_subset$C)

ggplot(data=HC_subset, aes(x=H, y=C)) + 
  geom_point(aes(colour = dEuclid)) +
  scale_colour_gradient(low = "yellow", high = "black") +
  geom_line(data = Cinf, aes(x=H, y=Cinf)) +
  geom_line(data = Csup, aes(x=H, y=Cmax)) +
  #scale_x_continuous(limits = c(minH, 1)) +
  #scale_y_continuous(limits = c(10^-6, 1.1*maxC)) +
  scale_x_continuous(trans="log", limits = c(minH, 1)) +
  scale_y_continuous(trans="log", limits = c(10^-6, 1.1*maxC)) +
  theme_light() +
  geom_point(data=interesting.data, aes(H.interesting, C.interesting), colour="red") +
  geom_segment(data=interesting.data, aes(x=H.interesting, y=10^-6, xend=H.interesting, yend=C.interesting), colour="red", alpha=.3) + 
  geom_segment(data=interesting.data, aes(x=H.interesting, y=C.interesting, xend=1, yend=C.interesting), colour="red", alpha=.3) 

