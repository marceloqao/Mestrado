# Working dir setup (Marcelo)
setwd("/Users/Marceloqao/Documents/Mestrado/thesis/Code/")

# Acrescentando a distância euclidiana ao data.frame
HC$dEuclid = sqrt((HC$H-1)^2 + HC$C^2)
###########################
## 


KS_Quantum_x_Radio_1k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_Radio_1k <- rbind(KS_Quantum_x_Radio_1k, 
                          ks.test(x=subset(HC, D==i & tau==j & N=="1000" & Source=="Quantum")$dEuclid, 
                                  y=subset(HC, D==i & tau==j & N=="1000" & Source=="Radio")$dEuclid)$p.value)
   }  
 }


KS_Quantum_x_Radio_50k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_Radio_50k <- rbind(KS_Quantum_x_Radio_50k, 
                                  ks.test(x=subset(HC, D==i & tau==j & N=="50k" & Source=="Quantum")$dEuclid, 
                                          y=subset(HC, D==i & tau==j & N=="50k" & Source=="Radio")$dEuclid)$p.value
                                  )
  }  
}


tt <- subset(HC, D=="6" & tau=="50" & N=="50k" & Source=="M-T")$dEuclid


tt

# KS = data.frame()
# 
# for(i in 3:6) {
#     for(j in c(1, 10, 30, 50)) {
#       KS <- KS,cbind(i, j))
#     }  
# }
# lD <- seq(3,6)
# ltau <- c(1,10,30,50)
# 
# KS.mat <- sapply(lD,function(j)
#   sapply(ltau,function(b) ks.test(x=subset(HC, lD & ltau & Source=="Quantum")$dEuclid,
#                                   y=subset(HC, lD & ltau & Source=="Radio")$dEuclid))$p.value)
# 
# ks.test(x=subset(HC, D==6 & tau==50 & Source=="Quantum")$dEuclid, 
#         y=subset(HC, D==6 & tau==50 & Source=="Radio")$dEuclid)$p.value
# 
# tapply((subset(HC, HC$D & HC$tau & Source=="Quantum")$dEuclid),
#         (subset(HC, HC$D & HC$tau & Source=="Radio")$dEuclid, ks.test)
#         