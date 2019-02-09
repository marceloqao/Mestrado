results <- read.table("../Data/result.txt", head=TRUE)

summary(results)
results <- results[,-c(4,5)]
summary(results)

results$D <- as.factor(results$D)
results$TN <- as.factor(results$TN)
results$K <- as.factor(results$K)

summary(results)

require(ggplot2)
require(ggthemes)
require(ggfortify)

### Exploratory analysis

ggplot(
  results,
  aes(x=H, y=C, col=D)
) + geom_point() + facet_grid(K ~ TN) 

### ESTRANHO!!!
ggplot(
  subset(results, TN==5e5),
  aes(x=H, y=C, col=D)
) + geom_point() + facet_grid(~ K) 

### Verificando se há algum problema

## Seleciono um tamanho de vetor, uma dimensão e dois K
subset12 <- subset(results, TN==1000 & D==3 & (K==0 | K==2))

## Desenho os valores selecionados
ggplot(subset12, aes(x=H, y=C)) + geom_point() + facet_grid(~K)

### NÃO VEJO DIFERENÇA!!!

### NÃO LER A PARTIR DESTE PONTO
### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)
