require(ggplot2)
require(ggthemes)

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
HC_D3tau1 <- subset(HC_All, D==3 & tau==1)
Cinf <- subset(inf, D==3)
Csup <- subset(sup, D==3)

minH <- min(HC_D3tau1$H)
maxC <- subset(Csup$Cmax, HC_D3tau1$H == min(HC_D3tau1$H))

ggplot(data=HC_D3tau1, aes(x=H, y=C)) + 
  geom_point(aes(colour = dEuclid)) +
  scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = Cinf, aes(x=H, y=Cinf)) +
  geom_line(data = Csup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(minH, 1)) +
  scale_y_continuous(limits = c(0, .01)) +
  theme_light()


HC_D6tau1 <- subset(HC_All, D==6 & tau==1)

Hmin <- min(HC_D6tau1$H)
ggplot(data=HC_D6tau1, aes(x=H, y=C)) + 
  geom_point(aes(colour = dEuclid)) +
  scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = subset(inf, D==6), aes(x=H, y=Cinf)) +
  geom_line(data = subset(sup, D==6), aes(x=H, y=Cmax)) +
  coord_cartesian(xlim = c(Hmin, 1)) +
  theme_light()

