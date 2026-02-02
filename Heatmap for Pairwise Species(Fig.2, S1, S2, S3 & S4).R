library(readxl)
library(stats)
library(circlize)
library(gplots)
library(RColorBrewer)
library(ggplot2)

setwd("~/Desktop/Davies Lab article for publishing/Pairwise species/")

### All Artificial 2015-2022'


data.all <- read.csv('all artificial matrix.csv', header = TRUE)
head(data.all)
rownames(data.all) <- data.all$X
data2<-data.all[c(2:12)]
data2 <- sapply(data2, as.numeric)
rownames(data2) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 
colnames(data2) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 

mycol <- colorRampPalette(brewer.pal(8, "PRGn"))(25)


pdf(file = "~/Desktop/Co-occurance Species/2015-2022 All artificial updated2.pdf", 
    width = 21, 
    height =27)
png(filename = "~/Desktop/Co-occurance Species/2015-2022 All artificial updated3.png",
    width = 1400, height = 1400, pointsize = 22)

heatmap.2(data2, scale = "none", col=mycol, na.color = "grey",
          trace = "none", density.info = "none", 
          key =FALSE, Rowv = FALSE, Colv = FALSE, margins = c(17,21), 
          cexRow = 1.9, cexCol = 1.9)
title( main="All Artificial Sites Pairwise Species Overlap
      Differences Between 2015 and 2022", cex.main = 2.5, line = -2)
dev.off()



### DLP,KWA, NWAS 2015-2022'

data.overlap <- read.csv('DLPKWANWAS matrix.csv', header = TRUE)
rownames(data.overlap) <- data.overlap$X
data3<-data.overlap[c(2:12)]
data3 <- sapply(data3, as.numeric)
rownames(data3) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 
colnames(data3) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 


png(filename = "~/Desktop/Co-occurance Species/2015-2022 All artificial overlapping.png",
    width = 1400, height = 1400, pointsize = 22)

heatmap.2(data3, scale = "none", col=mycol, na.color = "grey",
          trace = "none", density.info = "none",
          key = FALSE, Rowv = FALSE, Colv = FALSE, margins = c(17,21), 
          cexRow = 1.9, cexCol = 1.9 )


dev.off()


### DLP 2015-2022'

data.dlp <- read.csv('DLP matrix.csv', header = TRUE)
rownames(data.dlp) <- data.dlp$X
data4<-data.dlp[c(2:12)]
data4 <- sapply(data4, as.numeric)
rownames(data4) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 
colnames(data4) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 



pdf(file = "~/Desktop/Co-occurance Species/2015-2022 DLP.pdf", 
    width = 14, 
    height =12)

png(filename = "~/Desktop/Co-occurance Species/2015-2022 All artificial DLP.png",
    width = 1400, height = 1400, pointsize = 22)

heatmap.2(data4, scale = "none", col=mycol, na.color = "grey",
          trace = "none", density.info = "none",
          key = FALSE, Rowv = FALSE, Colv = FALSE,
          margins = c(17,21), 
          cexRow = 1.9, cexCol = 1.9)

dev.off()


### KWA 2015-2022'

data.kwa <- read.csv('KWA matrix.csv', header = TRUE)
rownames(data.kwa) <- data.kwa$X
data5<-data.kwa[c(2:12)]
data5 <- sapply(data5, as.numeric)
rownames(data5) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 
colnames(data5) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog")  



pdf(file = "~/Desktop/Co-occurance Species/2015-2022 KWA.pdf", 
    width = 14, 
    height =12)

png(filename = "~/Desktop/Co-occurance Species/2015-2022 All artificial KWA.png",
    width = 1400, height = 1400, pointsize = 22)

heatmap.2(data5, scale = "none", col=mycol, na.color = "grey",
          trace = "none", density.info = "none", 
          key = FALSE, Rowv = FALSE, Colv = FALSE, margins = c(17,21), 
          cexRow = 1.9, cexCol = 1.9)

dev.off()


### NWAS 2015-2022'

data.nwas <- read.csv('NWAS matrix.csv', header = TRUE)
rownames(data.nwas) <- data.nwas$X
data6<-data.nwas[c(2:12)]
data6 <- sapply(data6, as.numeric)
rownames(data6) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog") 
colnames(data6) <- c("Giraffe","Kudu","Elephant","Impala", "White Rhinoceros",
                     "Buffalo", "Baboon", "Zebra", "Hyena", "Lion", "Warthog")   


pdf(file = "~/Desktop/Co-occurance Species/2015-2022 NWAS.pdf", 
    width = 14, 
    height =12)

png(filename = "~/Desktop/Co-occurance Species/2015-2022 All artificial NWAS.png",
    width = 1400, height = 1400, pointsize = 22)

heatmap.2(data6, scale = "none", col=mycol, na.color = "grey",
          trace = "none", density.info = "none", 
          key = FALSE, Rowv = FALSE, Colv = FALSE,
          margins = c(17,21), 
          cexRow = 1.9, cexCol = 1.9)

dev.off()
