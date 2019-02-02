results <- read.table("../Data/result.txt", head=TRUE)
summary(results)

results <- results[,-4]
results$D <- as.factor(results$D)

require(ggplot2)
require(ggthemes)

df3 <- subset(results, D == 3)[-3]
df4 <- subset(results, D == 4)[-3]
df5 <- subset(results, D == 5)[-3]
df6 <- subset(results, D == 6)[-3]

ggplot(subset(df3, TN=1000), aes(x=H, y=C)) + geom_point(aes(color=K), alpha=0.1)
