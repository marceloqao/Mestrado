# Histogramas das distâncias


# Todos os histogramas de distâncias
ggplot(data=subset(HC, N=="50k" & D==6 & tau==50), aes(x=dEuclid)) +
  geom_density(aes(fill=Source), alpha=.5) +
  scale_x_continuous() +
  #facet_wrap(D~ tau) +
  theme_light()
ks.test(x=subset(HC, N=="50k" & D==6 & tau==1 & Source=="Quantum")$dEuclid,
        y=subset(HC, N=="50k" & D==6 & tau==1 & Source=="Radio")$dEuclid)

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
