## Global visualization of scatterplots
# N=1000

ggplot(data=subset(HC, N=="1000" & Source=="Radio"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="1000" & Source=="Radio")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="1000" & Source=="Radio")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
  #labs(caption = "N=1000, Radio")

ggplot(data=subset(HC, N=="1000" & Source=="Quantum"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="1000" & Source=="Quantum")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="1000" & Source=="Quantum")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
#labs(caption = "N=1000, Random")

ggplot(data=subset(HC, N=="1000" & Source=="M-T"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="1000" & Source=="M-T")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="1000" & Source=="M-T")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
#labs(caption = "N=1000, Mersenne Twister")

#######
## N=50k

ggplot(data=subset(HC, N=="50k" & Source=="Radio"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="50k" & Source=="Radio")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="50k" & Source=="Radio")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
#labs(caption = "N=1000, Radio")

ggplot(data=subset(HC, N=="50k" & Source=="Quantum"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="50k" & Source=="Quantum")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="50k" & Source=="Quantum")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
#labs(caption = "N=50k, Quantum")

ggplot(data=subset(HC, N=="50k" & Source=="M-T"), aes(x=H, y=C)) + 
  geom_point(alpha=.01) +
  #geom_point(aes(colour = dEuclid)) +
  #scale_colour_gradient(low = "white", high = "black") +
  geom_line(data = inf, aes(x=H, y=Cinf)) +
  geom_line(data = sup, aes(x=H, y=Cmax)) +
  scale_x_continuous(limits = c(min(subset(HC, N=="50k" & Source=="M-T")$H), 1)) +
  scale_y_continuous(limits = c(0, max(subset(HC, N=="50k" & Source=="M-T")$C))) +
  facet_grid(tau ~ D) +
  theme_light()
  # labs(caption = "N=50k, Mersenne Twister")


#######
# 
# ## Bloco comentado por Marcelo em  20/10/17)
# # Acrescentando a distância euclidiana ao data.frame
# # (Já acrescentado em DataSetup.R) - Marcelo
# HC$dEuclid = sqrt((HC$H-1)^2 + HC$C^2)
# 
# 
# 
# # Reducing data for testing
# #HC_samp <- HC[sample(nrow(HC), 50000), ]
# 
# 
# # Global visualization of scatterplots
# 
# ggplot(data=subset(HC, Source="Quantum"), aes(x=H, y=C)) + 
#   geom_point() +
#   #geom_point(aes(colour = dEuclid)) +
#   #scale_colour_gradient(low = "white", high = "black") +
#   geom_line(data = inf, aes(x=H, y=Cinf)) +
#   geom_line(data = sup, aes(x=H, y=Cmax)) +
#   scale_x_continuous(limits = c(minH, 1)) +
#   scale_y_continuous(limits = c(0, maxC)) +
#   facet_grid(tau ~ D) +
#   theme_light()
# 
# 
# ### Todos juntos: sem sentido
# ggplot(data=HC, aes(x=H, y=C)) + 
#   geom_point(colour=cores[HC_samp$Source], alpha=0.01) +
#   #geom_point(aes(colour = dEuclid)) +
#   #scale_colour_gradient(low = "white", high = "black") +
#   geom_line(data = inf, aes(x=H, y=Cinf)) +
#   geom_line(data = sup, aes(x=H, y=Cmax)) +
#   scale_x_continuous(limits = c(minH, 1)) +
#   scale_y_continuous(limits = c(0, maxC)) +
#   facet_grid(tau ~ D) +
#   theme_light()
# 
# ## Fim do bloco comentado

# Visualização por grupos
# Com os pontos de interesse para os quantis


# Removing MT from HC Dataframe (Marcelo em 22/10)
HC_no_MT <- subset(HC, Source!="M-T")

N.current <- "50k"   # 1000 ou 50k
D.current <- "6"
tau.current <- "50"
# HC_subset <- subset(HC, D==D.current & tau==tau.current) #(Marcelo em 22/10)
HC_subset <- subset(HC_no_MT, N==N.current & D==D.current & tau==tau.current)
Cinf <- subset(inf, D==D.current)
Csup <- subset(sup, D==D.current)
minH <- min(HC_subset$H)
maxC <- max(HC_subset$C)


# Interesting sorted distances
N <- length(HC_subset$dEuclid)
interesting <- round(c(.999*N, .99*N, .95*N, .9*N))
# interesting <- round(c(.001*N, .01*N, .05*N, .1*N))

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
  # scale_x_continuous(trans="log", limits = c(minH, 1)) +
  # scale_y_continuous(trans="log", limits = c(10^-6, 1.1*maxC)) +
  theme_light() +
  geom_point(data=interesting.data, aes(H.interesting, C.interesting), colour="red") +
  geom_segment(data=interesting.data, aes(x=H.interesting, y=10^-6, xend=H.interesting, yend=C.interesting), colour="red", alpha=.3) +
  geom_segment(data=interesting.data, aes(x=H.interesting, y=C.interesting, xend=1, yend=C.interesting), colour="red", alpha=.3)

 ggsave("scatter50kD6t50.png", plot = last_plot(), device = "png", path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)
  