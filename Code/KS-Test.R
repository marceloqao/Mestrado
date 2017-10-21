# Working dir setup (Marcelo)
setwd("/Users/Marceloqao/Documents/Mestrado/thesis/Code/")

# Acrescentando a distância euclidiana ao data.frame
#HC$dEuclid = sqrt((HC$H-1)^2 + HC$C^2)

###########################
## KS-Test Quantum - Radio - MT - 1k 

KS_Quantum_x_Radio_1k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_Radio_1k <- rbind(KS_Quantum_x_Radio_1k, 
                          c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="1000" & Source=="Quantum")$dEuclid, 
                                  y=subset(HC, D==i & tau==j & N=="1000" & Source=="Radio")$dEuclid)$p.value))
   }  
 }
names(KS_Quantum_x_Radio_1k)=c("D", "tau", "p-valor") 

###########################

KS_Quantum_x_MT_1k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_MT_1k <- rbind(KS_Quantum_x_MT_1k, 
                              c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="1000" & Source=="Quantum")$dEuclid, 
                                  y=subset(HC, D==i & tau==j & N=="1000" & Source=="M-T")$dEuclid)$p.value))
  }  
}
names(KS_Quantum_x_MT_1k)=c("D", "tau", "p-valor") 

###########################

KS_Radio_x_MT_1k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Radio_x_MT_1k <- rbind(KS_Radio_x_MT_1k, 
                                   c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="1000" & Source=="Radio")$dEuclid, 
                                                   y=subset(HC, D==i & tau==j & N=="1000" & Source=="M-T")$dEuclid)$p.value))
  }  
}
names(KS_Radio_x_MT_1k)=c("D", "tau", "p-valor") 

###########################
## KS-Test Quantum - Radio - MT - 50k
###########################

KS_Quantum_x_Radio_50k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_Radio_50k <- rbind(KS_Quantum_x_Radio_50k, 
                                   c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="50k" & Source=="Quantum")$dEuclid, 
                                                   y=subset(HC, D==i & tau==j & N=="50k" & Source=="Radio")$dEuclid)$p.value))
  }  
}
names(KS_Quantum_x_Radio_50k)=c("D", "tau", "p-valor") 

###########################

KS_Quantum_x_MT_50k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Quantum_x_MT_50k <- rbind(KS_Quantum_x_MT_50k, 
                                 c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="50k" & Source=="Quantum")$dEuclid, 
                                                 y=subset(HC, D==i & tau==j & N=="50k" & Source=="M-T")$dEuclid)$p.value))
  }  
}
names(KS_Quantum_x_MT_50k)=c("D", "tau", "p-valor") 

###########################

KS_Radio_x_MT_50k = data.frame()
for(i in 3:6) {
  for(j in c(1,10,30,50)) {
    KS_Radio_x_MT_50k <- rbind(KS_Radio_x_MT_50k, 
                                 c(i, j, ks.test(x=subset(HC, D==i & tau==j & N=="50k" & Source=="Radio")$dEuclid, 
                                                 y=subset(HC, D==i & tau==j & N=="50k" & Source=="M-T")$dEuclid)$p.value))
  }  
}
names(KS_Radio_x_MT_50k)=c("D", "tau", "p-valor") 
