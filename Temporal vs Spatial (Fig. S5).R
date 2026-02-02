#install.packages("gridExtra")
library(gridExtra)
library(lattice)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)


par(mfrow = c(2, 2))


# Basic scatter plot 2022

correlation2022 = read.csv("Estimatedvsobserved2022.csv")
correlation2022<- na.omit(correlation2022)


plot2022 <- xyplot(correlation2022[,3]~correlation2022[,4], prepanel = function(x, y) prepanel.loess(x, y),
                   xlab = list(label="Observed spatial co-occurrence", cex=3), ylab = list(label="Estimated temporal overlap", cex=3), 
                   main=list(label="2022", cex =3), scales=list(cex=2.5),
                   panel = function(x, y) {
                     panel.xyplot(x, y, pch = 1, cex = 1.5)
                     panel.loess(x, y, span=1.5, col ="red", lwd = 2)
                   })


cor(correlation2022[,3],correlation2022[,4], method = "spearman")



# Basic scatter plot 2015

correlation2015 = read.csv("Estimatedvsobserved2015.csv")
correlation2015 <- na.omit(correlation2015)


plot2015 <- xyplot(correlation2015[,3]~correlation2015[,4], prepanel = function(x, y) prepanel.loess(x, y),
                   xlab = list(label="Observed spatial co-occurrence", cex=3), ylab = list(label="Estimated temporal overlap", cex=3), 
                   main=list(label="2015", cex =3), scales=list(cex=2.5),
                   panel = function(x, y) {
                     panel.xyplot(x, y, pch = 1, cex = 1.5)
                     panel.loess(x, y, span=1.5, col ="red", lwd = 2)
                   })

cor(correlation2015[,3],correlation2015[,4], method = "spearman")


ggarrange(plot2015,plot2022, ncol = 2, labels = c("A)","B)"), font.label = list(size = 29, color = "black" ))

