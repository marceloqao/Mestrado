results <- read.csv("~/Documents/Alunos/Marcelo Queiroz/Article/Data/results.csv")

results <- data.frame(results[,c(2,3,4,7,8,9)])

results$TN <- as.factor(results$TN)
results$D <- as.factor(results$D)
results$K <- as.factor(results$K)

summary(results)

require(ggplot2)
require(ggthemes)
require(ggsci)
require(extrafont)
require(wesanderson)
require(jcolors)
font_import()
loadfonts()

require(ggfortify)

### Exploratory analysis

### Analisar variações
### Acrescentar curvas dos limites HxC quando cada "facet" seja para o mesmo D

ggplot(results, aes(x=H, y=C, col=K)) + 
  geom_point(size=.7) + 
  facet_grid(D ~ TN) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) +
  labs(colour=expression(italic(K))) +
  theme_igray() +
  theme(
    text=element_text(size=14, 
                      family="Times New Roman")) + 
  guides(colour = guide_legend(override.aes = list(size=3)))


### Análise de subconjuntos

ggplot(subset(results, K==0), 
       aes(x=H, y=C)) + 
  geom_point(size=.7) + 
  facet_grid(D~TN) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) +
  labs(colour=expression(italic(K))) +
  scale_color_jcolors("pal5") +
  theme_igray() +
  theme(
    text=element_text(size=14, 
                      family="Times New Roman")) + 
  guides(colour = guide_legend(override.aes = list(size=3)))


### NÃO LER A PARTIR DESTE PONTO
### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)
