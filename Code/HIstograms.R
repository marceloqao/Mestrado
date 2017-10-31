# Histogramas das Distâncias Euclidianas

## N=1000 D=6 Tau=1
ggplot(data=subset(HC, N=="1000" & D=="6" & tau=="1"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  # facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_1k_t1.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

## N=1000 D=6 Tau=50
ggplot(data=subset(HC, N=="1000" & D=="6" & tau=="50"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  # facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_1k_t50.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

## N=50k D=6 Tau=1
ggplot(data=subset(HC, N=="1000" & D=="6" & tau=="1"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  # facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_50k_t1.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

## N=50k D=6 Tau=1
ggplot(data=subset(HC, N=="1000" & D=="6" & tau=="50"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  # facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_50k_t50.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)
################################################################################

## Grid dos histogramas de distâncias ######
## N=1000 aleatórias  ######

ggplot(data=subset(HC_no_MT, N=="1000"), aes(x=dEuclid)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous() +
  # facet_grid(~D) +
  facet_wrap(~D) +
  xlab("Distância ao Ponto de Referência") +
  ylab("Proporções Suavizadas") +
  theme_light()

ggsave(paste0("HistoDistanciasAleat.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

#### Histogramas Grid N #########
# Tau=1
ggplot(data=subset(HC, D=="6" & tau=="1"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_t1.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

# Tau=50
ggplot(data=subset(HC, D=="6" & tau=="50"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~N) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_t50.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

#### Histogramas Grid N #######

#### Histogramas Grid D
# Todos os histogramas de distâncias
ggplot(data=subset(HC, N=="1000" & tau=="1"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~D) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_t1_1k.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

# Todos os histogramas de distâncias
ggplot(data=subset(HC, N=="50k" & tau=="50"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~D) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_t50_50k.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

#### Histogramas Grid Tau #######

#####

#### Histogramas Grid Tau #########
# N=1000
ggplot(data=subset(HC, N=="1000" & D=="6"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~tau) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_1k.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

# N=50k
ggplot(data=subset(HC, N=="50k" & D=="6"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~tau) +
  #facet_wrap(~tau) +
  theme_light()

ggsave(paste0("Hist_D6_50k.png"), plot = last_plot(), device = "png",
       path = "../newPlots", scale = 1, dpi = 300, limitsize = TRUE)

#### Histogramas Grid Tau #######

# Visualização de histogramas por grupos

ggplot(data=subset(data, D==4), aes(x=dEuclid)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous() +
  theme_light()

ggplot(data=subset(data, D==5), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous() +
  theme_light()

ggplot(data=subset(data, D==6), aes(x=value)) +
  geom_density(aes(fill=tau), alpha=.5) +
  scale_x_continuous() +
  theme_light()
