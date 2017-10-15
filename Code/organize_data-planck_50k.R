##Organize all Planck data into a unique dataframe with columns "D" "Tau"

setwd("/Users/Marceloqao/Documents/Mestrado/code/Max-Planck/")
filenames <- list.files(path = "data/50k")

## Get names without ".DAT" and store in "names"
names <- substr(filenames, 1, 13)

##Loading data from files
for(i in names){
  filepath <- file.path(paste("data/50k/", i,".dat",sep=""))
  assign(i, read.table(filepath, sep = "", header=TRUE))
}

## Join Ds into the same dataframe
HC3_01 <- matrix(unlist(`PLANCK_D3-T01`), ncol = 3, byrow = TRUE)
HC3_10 <- matrix(unlist(`PLANCK_D3-T10`), ncol = 3, byrow = TRUE)
HC3_30 <- matrix(unlist(`PLANCK_D3-T30`), ncol = 3, byrow = TRUE)
HC3_50 <- matrix(unlist(`PLANCK_D3-T50`), ncol = 3, byrow = TRUE)

HC4_01 <- matrix(unlist(`PLANCK_D4-T01`), ncol = 3, byrow = TRUE)
HC4_10 <- matrix(unlist(`PLANCK_D4-T10`), ncol = 3, byrow = TRUE)
HC4_30 <- matrix(unlist(`PLANCK_D4-T30`), ncol = 3, byrow = TRUE)
HC4_50 <- matrix(unlist(`PLANCK_D4-T50`), ncol = 3, byrow = TRUE)

HC5_01 <- matrix(unlist(`PLANCK_D5-T01`), ncol = 3, byrow = TRUE)
HC5_10 <- matrix(unlist(`PLANCK_D5-T10`), ncol = 3, byrow = TRUE)
HC5_30 <- matrix(unlist(`PLANCK_D5-T30`), ncol = 3, byrow = TRUE)
HC5_50 <- matrix(unlist(`PLANCK_D5-T50`), ncol = 3, byrow = TRUE)

HC6_01 <- matrix(unlist(`PLANCK_D6-T01`), ncol = 3, byrow = TRUE)
HC6_10 <- matrix(unlist(`PLANCK_D6-T10`), ncol = 3, byrow = TRUE)
HC6_30 <- matrix(unlist(`PLANCK_D6-T30`), ncol = 3, byrow = TRUE)
HC6_50 <- matrix(unlist(`PLANCK_D6-T50`), ncol = 3, byrow = TRUE)

###################################
## Join Ds into the same dataframe
HC3_01 <- cbind(HC3_01, rep(x = 1, times=dim(HC3_01)[1]))
HC3_10 <- cbind(HC3_10, rep(x = 10, times=dim(HC3_10)[1]))
HC3_30 <- cbind(HC3_30, rep(x = 30, times=dim(HC3_30)[1]))
HC3_50 <- cbind(HC3_50, rep(x = 50, times=dim(HC3_50)[1]))

HC4_01 <- cbind(HC4_01, rep(x = 1, times=dim(HC4_01)[1]))
HC4_10 <- cbind(HC4_10, rep(x = 10, times=dim(HC4_10)[1]))
HC4_30 <- cbind(HC4_30, rep(x = 30, times=dim(HC4_30)[1]))
HC4_50 <- cbind(HC4_50, rep(x = 50, times=dim(HC4_50)[1]))

HC5_01 <- cbind(HC5_01, rep(x = 1, times=dim(HC5_01)[1]))
HC5_10 <- cbind(HC5_10, rep(x = 10, times=dim(HC5_10)[1]))
HC5_30 <- cbind(HC5_30, rep(x = 30, times=dim(HC5_30)[1]))
HC5_50 <- cbind(HC5_50, rep(x = 50, times=dim(HC5_50)[1]))

HC6_01 <- cbind(HC6_01, rep(x = 1, times=dim(HC6_01)[1]))
HC6_10 <- cbind(HC6_10, rep(x = 10, times=dim(HC6_10)[1]))
HC6_30 <- cbind(HC6_30, rep(x = 30, times=dim(HC6_30)[1]))
HC6_50 <- cbind(HC6_50, rep(x = 50, times=dim(HC6_50)[1]))

dim(rbind(HC3_01, HC3_10, HC3_30, HC3_50))
dim(rbind(HC4_01, HC4_10, HC4_30, HC4_50))
dim(rbind(HC5_01, HC5_10, HC5_30, HC5_50))
dim(rbind(HC6_01, HC6_10, HC6_30, HC6_50))

## D=3
HxCxJS_D3 <- data.frame(rbind(HC3_01, HC3_10, HC3_30, HC3_50), 
                        rep(3, times=dim(HC3_01)[1]+
                                        dim(HC3_10)[1] +
                                        dim(HC3_30)[1] +
                                        dim(HC3_50)[1]
                          )
                        )
names(HxCxJS_D3) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D3$tau <- as.factor(HxCxJS_D3$tau)
HxCxJS_D3$D <- as.factor(HxCxJS_D3$D)
summary(HxCxJS_D3)

## D=4
HxCxJS_D4 <- data.frame(rbind(HC4_01, HC4_10, HC4_30, HC4_50), 
                        rep(4, times=dim(HC4_01)[1]+
                              dim(HC4_10)[1] +
                              dim(HC4_30)[1] +
                              dim(HC4_50)[1]
                        )
)
names(HxCxJS_D4) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D4$tau <- as.factor(HxCxJS_D4$tau)
HxCxJS_D4$D <- as.factor(HxCxJS_D4$D)
summary(HxCxJS_D4)

## D=5
HxCxJS_D5 <- data.frame(rbind(HC5_01, HC5_10, HC5_30, HC5_50), 
                        rep(5, times=dim(HC5_01)[1]+
                              dim(HC5_10)[1] +
                              dim(HC5_30)[1] +
                              dim(HC5_50)[1]
                        )
)
names(HxCxJS_D5) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D5$tau <- as.factor(HxCxJS_D5$tau)
HxCxJS_D5$D <- as.factor(HxCxJS_D5$D)
summary(HxCxJS_D5)

## D=6
HxCxJS_D6 <- data.frame(rbind(HC6_01, HC6_10, HC6_30, HC6_50), 
                        rep(6, times=dim(HC6_01)[1]+
                              dim(HC6_10)[1] +
                              dim(HC6_30)[1] +
                              dim(HC6_50)[1]
                        )
)
names(HxCxJS_D6) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D6$tau <- as.factor(HxCxJS_D6$tau)
HxCxJS_D6$D <- as.factor(HxCxJS_D6$D)
summary(HxCxJS_D6)

#####
##Join all into a dataframe

HC_Quantum50k <- rbind(HxCxJS_D3, HxCxJS_D4, HxCxJS_D5, HxCxJS_D6)
summary(HC_Quantum50k)
save(HC_Quantum50k, file="HC_Quantum50k.zip", compress = TRUE)
