require(ggplot2)
require(ggthemes)
require(reshape2)

load(file='../Data/HC_All.gzip')
load(file='../Data/inf.gzip')
load(file='../Data/sup.gzip')

# Determinação de limite
minH <- min(HC_All$H)
maxC <- max(HC_All$C)

# Acrescentando a distância euclidiana ao data.frame
attach(HC_All)
HC_All$D <- as.factor(D)
HC_All$dEuclid = sqrt((H-1)^2 + JS^2)
detach(HC_All)

# Visualização global, com transparência
ggplot(data=HC_All, aes(x=H, y=C)) + 
  geom_point(alpha=0.01) +
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
HC_subset <- subset(HC_All, D==D.current & tau==tau.current)
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
  scale_x_continuous(limits = c(minH, 1)) +
  scale_y_continuous(limits = c(10^-6, 1.1*maxC)) +
  #scale_x_continuous(trans="log", limits = c(minH, 1)) +
  #scale_y_continuous(trans="log", limits = c(10^-6, 1.1*maxC)) +
  theme_light() +
  geom_point(data=interesting.data, aes(H.interesting, C.interesting), colour="red") +
  geom_segment(data=interesting.data, aes(x=H.interesting, y=10^-6, xend=H.interesting, yend=C.interesting), colour="red", alpha=.3) + 
  geom_segment(data=interesting.data, aes(x=H.interesting, y=C.interesting, xend=1, yend=C.interesting), colour="red", alpha=.3) 

# Histogramas das distâncias

data <- melt(HC_All, measure.vars = "dEuclid")

# Todos os histogramas de distâncias
ggplot(data=data, aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous(name="Distâncias ao Ponto de Referência") +
  scale_y_continuous(name="Proporções suavizadas") + 
  facet_wrap(~ D) +
  theme_light()
  
# Visualização de histogramas por grupos

ggplot(data=subset(data, D==3), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous(trans="log") +
  theme_light()

ggplot(data=subset(data, D==4), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous(trans="log") +
  theme_light()

ggplot(data=subset(data, D==5), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous(trans="log") +
  theme_light()

ggplot(data=subset(data, D==6), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous(trans="log") +
  theme_light()
