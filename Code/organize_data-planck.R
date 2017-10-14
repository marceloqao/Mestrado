##Organize all Planck data into a unique dataframe with columns "D" "Tau"

## Join Ds into the same dataframe
HC3_01 <- cbind(HC_Planck_3_01, rep(x = 1, times=dim(HC_Planck_3_01)[1]))
HC3_10 <- cbind(HC_Planck_3_10, rep(x = 10, times=dim(HC_Planck_3_10)[1]))
HC3_30 <- cbind(HC_Planck_3_30, rep(x = 30, times=dim(HC_Planck_3_30)[1]))
HC3_50 <- cbind(HC_Planck_3_50, rep(x = 50, times=dim(HC_Planck_3_50)[1]))

HC4_01 <- cbind(HC_Planck_4_01, rep(x = 1, times=dim(HC_Planck_4_01)[1]))
HC4_10 <- cbind(HC_Planck_4_10, rep(x = 10, times=dim(HC_Planck_4_10)[1]))
HC4_30 <- cbind(HC_Planck_4_30, rep(x = 30, times=dim(HC_Planck_4_30)[1]))
HC4_50 <- cbind(HC_Planck_4_50, rep(x = 50, times=dim(HC_Planck_4_50)[1]))

HC5_01 <- cbind(HC_Planck_5_01, rep(x = 1, times=dim(HC_Planck_5_01)[1]))
HC5_10 <- cbind(HC_Planck_5_10, rep(x = 10, times=dim(HC_Planck_5_10)[1]))
HC5_30 <- cbind(HC_Planck_5_30, rep(x = 30, times=dim(HC_Planck_5_30)[1]))
HC5_50 <- cbind(HC_Planck_5_50, rep(x = 50, times=dim(HC_Planck_5_50)[1]))

HC6_01 <- cbind(HC_Planck_6_01, rep(x = 1, times=dim(HC_Planck_6_01)[1]))
HC6_10 <- cbind(HC_Planck_6_10, rep(x = 10, times=dim(HC_Planck_6_10)[1]))
HC6_30 <- cbind(HC_Planck_6_30, rep(x = 30, times=dim(HC_Planck_6_30)[1]))
HC6_50 <- cbind(HC_Planck_6_50, rep(x = 50, times=dim(HC_Planck_6_50)[1]))

dim(rbind(HC3_01, HC3_10, HC3_30, HC3_50))
dim(rbind(HC4_01, HC4_10, HC4_30, HC4_50))
dim(rbind(HC5_01, HC5_10, HC5_30, HC5_50))
dim(rbind(HC6_01, HC6_10, HC6_30, HC6_50))

## D=3
HxCxJS_D3 <- data.frame(rbind(HC3_01, HC3_10, HC3_30, HC3_50), 
                        rep(3, times=dim(HC_Planck_6_01)[1]+
                                        dim(HC_Planck_5_10)[1] +
                                        dim(HC_Planck_6_30)[1] +
                                        dim(HC_Planck_6_50)[1]
                          )
                        )
names(HxCxJS_D3) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D3$tau <- as.factor(HxCxJS_D3$tau)
summary(HxCxJS_D3)

## D=4
HxCxJS_D4 <- data.frame(rbind(HC4_01, HC4_10, HC4_30, HC4_50), 
                        rep(4, times=dim(HC_Planck_4_01)[1]+
                              dim(HC_Planck_4_10)[1] +
                              dim(HC_Planck_4_30)[1] +
                              dim(HC_Planck_4_50)[1]
                        )
)
names(HxCxJS_D4) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D4$tau <- as.factor(HxCxJS_D4$tau)
summary(HxCxJS_D4)

## D=5
HxCxJS_D5 <- data.frame(rbind(HC5_01, HC5_10, HC5_30, HC5_50), 
                        rep(5, times=dim(HC_Planck_5_01)[1]+
                              dim(HC_Planck_5_10)[1] +
                              dim(HC_Planck_5_30)[1] +
                              dim(HC_Planck_5_50)[1]
                        )
)
names(HxCxJS_D5) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D5$tau <- as.factor(HxCxJS_D5$tau)
summary(HxCxJS_D5)

## D=6
HxCxJS_D6 <- data.frame(rbind(HC6_01, HC6_10, HC6_30, HC6_50), 
                        rep(6, times=dim(HC_Planck_6_01)[1]+
                              dim(HC_Planck_6_10)[1] +
                              dim(HC_Planck_6_30)[1] +
                              dim(HC_Planck_6_50)[1]
                        )
)
names(HxCxJS_D6) <- c("H", "C", "JS", "tau", "D")
HxCxJS_D6$tau <- as.factor(HxCxJS_D6$tau)
summary(HxCxJS_D6)

# HxCxJS_D4 <- data.frame(rbind(HC4_01, HC4_10, HC4_30, HC4_50))
# names(HxCxJS_D4) <- c("H", "C", "JS", "tau")
# HxCxJS_D4$tau <- as.factor(HxCxJS_D4$tau)
# summary(HxCxJS_D4)
# 
# HxCxJS_D5 <- data.frame(rbind(HC5_01, HC5_10, HC5_30, HC5_50))
# names(HxCxJS_D5) <- c("H", "C", "JS", "tau")
# HxCxJS_D5$tau <- as.factor(HxCxJS_D5$tau)
# summary(HxCxJS_D5)
# 
# HxCxJS_D6 <- data.frame(rbind(HC6_01, HC6_10, HC6_30, HC6_50))
# names(HxCxJS_D6) <- c("H", "C", "JS", "tau")
# HxCxJS_D6$tau <- as.factor(HxCxJS_D6$tau)
# summary(HxCxJS_D6)

#####
##Join all into a dataframe

HC_All <- rbind(HxCxJS_D3, HxCxJS_D4, HxCxJS_D5, HxCxJS_D6)
summary(HC_All)
save(HC_All, file="HC_All.gzip", compress = TRUE)

HC_All$D <- as.factor(HC_All$D)

####
## Load the "Banana" and save

save(sup, file="sup.gzip", compress = TRUE)
save(inf, file="inf.gzip", compress = TRUE)

##
require(ggplot2)

ggplot(data=HxCxJS_D3, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ .)
ggplot(data=HxCxJS_D4, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ .)
ggplot(data=HxCxJS_D5, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ .)
ggplot(data=HxCxJS_D6, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ .)

ggplot(data=HC_All, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ D)
