results <- read.csv("~/Documents/Alunos/Marcelo Queiroz/Article/Data/results.csv")
summary(results$X)

values <- data.frame(TN=as.factor(results$TN),
                     K=as.factor(results$K),
                     D=as.factor(results$D),
                     Rep=as.factor(results$REP),
                     Tau=as.factor(results$TAU),
                     H=results$H,
                     C=results$C,
                     JS=results$JS)

rm(results)

