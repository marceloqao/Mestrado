results <- read.table("../Data/result.txt", head=TRUE)
summary(results)

results <- results[,-4]
results$D <- as.factor(results$D)

require(ggplot2)
require(ggthemes)
require(ggfortify)

df3 <- subset(results, D == 3)[-3]
df4 <- subset(results, D == 4)[-3]
df5 <- subset(results, D == 5)[-3]
df6 <- subset(results, D == 6)[-3]

ggplot(subset(df3, TN=1000), aes(x=H, y=C)) + geom_point(aes(color=K), alpha=0.1)

### Exploratory analysis

df_D3_TN1000_K0 <- subset(results, D==3 & TN==1000 & K==0, )

ggplot(df_D3_TN1000_K0, aes(x=H, y=C)) + geom_point() 

### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)
