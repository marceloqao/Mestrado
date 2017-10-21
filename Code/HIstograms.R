# Histogramas das distâncias

# Todos os histogramas de distâncias
ggplot(data=subset(HC, D=="6" & N=="1000"), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  facet_grid(~tau) +
  #facet_wrap(~tau) +
  theme_light()

# Visualização de histogramas por grupos

ggplot(data=subset(data, D==4), aes(x=value)) +
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
