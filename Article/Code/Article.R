results <- read.table("../Data/result.txt", head=TRUE)

results <- results[,-4]
results$D <- as.factor(results$D)
results$TN <- as.factor(results$TN)
results$K <- as.factor(results$K)

summary(results)

require(ggplot2)
require(ggthemes)
require(ggfortify)

### Exploratory analysis

ggplot(
  subset(results, 
         TN==levels(results$TN[1]) & 
         K==levels(results$K)[6] & 
         D==levels(results$D)[1])) +
  geom_point(aes(x=H, y=C))

### Ver, a partir de este gráfico, quais fatores podem ser considerados relevantes
### Analisar TN=1000, D=6 e ver fazer regressões de C ~ H com K como fator; o fator é relevante?

ggplot(
  results,
  aes(x=H, y=C, col=D)
) + geom_point() + facet_grid(K ~ TN) 

### ESTRANHO!!!
ggplot(
  subset(results, TN==1000),
  aes(x=H, y=C, col=D)
) + geom_point() + facet_grid(~ K) 

### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)
