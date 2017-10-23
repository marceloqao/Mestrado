require(ggplot2)
require(ggthemes)
require(reshape2)


### Load the curvese of sup and inf HxC
load(file='../Data/inf.gzip')
load(file='../Data/sup.gzip')

## Amostras de tamanho 1000

### Load the observed points
load(file='../Data/HC_Quantum1000.zip')
HC_Quantum1000 <- HC_All
rm(HC_All)
HC_Quantum1000$Source <- as.factor("Quantum")
HC_Quantum1000$N <- as.factor(1000)

load(file='../Data/HC_Radio1000.zip')
HC_Radio1000 <- HC_All_Random
rm(HC_All_Random)
HC_Radio1000$Source <- as.factor("Radio")
HC_Radio1000$N <- as.factor(1000)

load(file='../Data/HC_MT1000.zip')
HC_MT1000 <- HC_All
rm(HC_All)
HC_MT1000$Source <- as.factor("M-T")
HC_MT1000$N <- as.factor(1000)

load(file='../Data/HC_Quantum50k.zip')
HC_Quantum50k$Source <- as.factor("Quantum")
HC_Quantum50k$N <- as.factor("50k")

load(file='../Data/HC_Radio50k.zip')
HC_Radio50k$Source <- as.factor("Radio")
HC_Radio50k$N <- as.factor("50k")

load(file='../Data/HC_MT50k.zip')
HC_MT50k$Source <- as.factor("M-T")
HC_MT50k$N <- as.factor("50k")

### Make a single data.frame with the proper size factor N
HC <- rbind(HC_Quantum1000, HC_Radio1000, HC_MT1000, HC_Quantum50k, HC_Radio50k, HC_MT50k)
rm(HC_Quantum1000, HC_Radio1000, HC_MT1000, HC_Quantum50k, HC_Radio50k, HC_MT50k)

# Determinação de limite
minH <- min(HC$H)
maxC <- max(HC$C)


# Limpando, e acrescentando a distância euclidiana ao data.frame 
HC$D <- as.factor(HC$D)
HC$dEuclid = sqrt((HC$H-1)^2 + HC$C^2)

# Removendo Mersenne-Twister do Dataframe
HC_no_MT <- subset(HC, Source!="M-T")


