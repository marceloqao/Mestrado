results <- read.csv("~/Documents/Alunos/Marcelo Queiroz/Article/Data/results.csv")

results <- data.frame(results[,c(2,3,4,7,8,9)])

results$TN <- as.factor(results$TN)
results$D <- as.factor(results$D)
results$K <- as.factor(results$K)

summary(results)

require(ggplot2)
require(ggthemes)
require(ggfortify)

### Exploratory analysis

ggplot(
  results,
  aes(x=H, y=C, col=D)
  ) + geom_point() + 
  facet_grid(K ~ TN) +
  theme_minimal()



### NÃO LER A PARTIR DESTE PONTO
### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)
