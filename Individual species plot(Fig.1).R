library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(ggpubr)

##Impala

df <- data.frame(impala15)
colnames(df)[1] <- "2015"
impala15 <- melt(df)



df2 <- data.frame(impala22)
colnames(df2)[1] <- "2022"
impala22 <- melt(df2)

impala15_22 <- rbind(impala15, impala22)
colnames(impala15_22)[1] <- "Year"

# Make the density plot


plot1 <- ggplot(impala15_22, aes(x = value, fill=Year)) +
  geom_density(alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Impala")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  scale_fill_manual( values = c("green","purple"))

plot1

##Giraffe

df <- data.frame(giraffe15)
colnames(df)[1] <- "2015"
giraffe15 <- melt(df)



df2 <- data.frame(giraffe22)
colnames(df2)[1] <- "2022"
giraffe22 <- melt(df2)

giraffe15_22 <- rbind(giraffe15, giraffe22)
colnames(giraffe15_22)[1] <- "Year"

# Make the density plot


plot2 <- ggplot(giraffe15_22, aes(x = value, fill=Year)) +
  geom_density(alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Giraffe")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  scale_fill_manual( values = c("green","purple"))+
  scale_color_manual( values = c("green","purple"))

plot2

##Elephant

df <- data.frame(elephant15)
colnames(df)[1] <- "2015"
elephant15 <- melt(df)



df2 <- data.frame(elephant22)
colnames(df2)[1] <- "2022"
elephant22 <- melt(df2)

elephant15_22 <- rbind(elephant15, elephant22)
colnames(elephant15_22)[1] <- "Year"

# Make the density plot


plot3 <- ggplot(elephant15_22, aes(x = value, fill=Year)) +
  geom_density(alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Elephant")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  scale_fill_manual( values = c("green","purple"))
plot3

##Kudu

df <- data.frame(kudu15)
colnames(df)[1] <- "2015"
kudu15 <- melt(df)



df2 <- data.frame(kudu22)
colnames(df2)[1] <- "2022"
kudu22 <- melt(df2)

kudu15_22 <- rbind(kudu15, kudu22)
colnames(kudu15_22)[1] <- "Year"

# Make the density plot


plot4 <- ggplot(kudu15_22, aes(x = value, fill=Year)) +
  geom_density(alpha=0.5) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 0.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Kudu")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  scale_fill_manual( values = c("green","purple"))
plot4

##White Rhinoceros

df <- data.frame(Wrhino15)
colnames(df)[1] <- "2015"
Wrhino15 <- melt(df)



df2 <- data.frame(Wrhino22)
colnames(df2)[1] <- "2022"
Wrhino22 <- melt(df2)

Wrhino15_22 <- rbind(Wrhino15, Wrhino22)
colnames(Wrhino15_22)[1] <- "Year"

# Make the density plot

plot5 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(Wrhino15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.7) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("White Rhinoceros")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(Wrhino15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 0.05)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot5

##Buffalo

df <- data.frame(buffalo15)
colnames(df)[1] <- "2015"
buffalo15 <- melt(df)



df2 <- data.frame(buffalo22)
colnames(df2)[1] <- "2022"
buffalo22 <- melt(df2)

buffalo15_22 <- rbind(buffalo15, buffalo22)
colnames(buffalo15_22)[1] <- "Year"

# Make the density plot


plot6 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(buffalo15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.55) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Buffalo")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(buffalo15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 0.45)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot6

##Zebra

df <- data.frame(zebra15)
colnames(df)[1] <- "2015"
zebra15 <- melt(df)



df2 <- data.frame(zebra22)
colnames(df2)[1] <- "2022"
zebra22 <- melt(df2)

zebra15_22 <- rbind(zebra15, zebra22)
colnames(zebra15_22)[1] <- "Year"

# Make the density plot

plot7 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(zebra15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.7) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Zebra")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(zebra15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 5)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot7

##Hyena

df <- data.frame(hyena15)
colnames(df)[1] <- "2015"
hyena15 <- melt(df)



df2 <- data.frame(hyena22)
colnames(df2)[1] <- "2022"
hyena22 <- melt(df2)

hyena15_22 <- rbind(hyena15, hyena22)
colnames(hyena15_22)[1] <- "Year"

# Make the density plot


plot8 <- ggplot() + 
  geom_density(
    aes(x = value, color= '2015'),
    data = subset(hyena15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.2) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Hyena")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(hyena15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 2)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot8

##Lion

df <- data.frame(lion15)
colnames(df)[1] <- "2015"
lion15 <- melt(df)



df2 <- data.frame(lion22)
colnames(df2)[1] <- "2022"
lion22 <- melt(df2)

lion15_22 <- rbind(lion15, lion22)
colnames(lion15_22)[1] <- "Year"

# Make the density plot


plot9 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(lion15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.6) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Lion")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(lion15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 1.3)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot9

##Warthog

df <- data.frame(warthog15)
colnames(df)[1] <- "2015"
warthog15 <- melt(df)



df2 <- data.frame(warthog22)
colnames(df2)[1] <- "2022"
warthog22 <- melt(df2)

warthog15_22 <- rbind(warthog15, warthog22)
colnames(warthog15_22)[1] <- "Year"

# Make the density plot


plot10 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(warthog15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 0.7) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Warthog")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(warthog15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 1.7)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))

plot10

##Baboon

df <- data.frame(baboon15)
colnames(df)[1] <- "2015"
baboon15 <- melt(df)



df2 <- data.frame(baboon22)
colnames(df2)[1] <- "2022"
baboon22 <- melt(df2)

baboon15_22 <- rbind(baboon15, baboon22)
colnames(baboon15_22)[1] <- "Year"

# Make the density plot


plot11 <- ggplot() + 
  geom_density(
    aes(x = value, color = '2015'),
    data = subset(baboon15_22, Year == 2015),
    fill = 'green', alpha = .5,
    adjust = 24) +
  scale_x_continuous(breaks = seq(0, 2*pi, by = pi/2),
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 2*pi)) +
  ylim(0, 5.4)+
  labs(x = "Time (hours)", y = "Density")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Baboon")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.text=element_text(size=18))+
  theme(legend.title=element_text(size=18))+
  geom_density(
    aes(x = value, color = '2022'),
    data = subset(baboon15_22, Year == 2022),
    fill = 'purple', alpha = .5,
    adjust = 0.7)+
  scale_color_manual(name = "Year", labels = c('2015','2022'), values = c('black','black'))+
  guides(color = guide_legend(override.aes = list(fill = c('green', 'purple'))))


plot11
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11, ncol = 4, nrow = 3, labels = "AUTO", 
          font.label = list(size = 20))