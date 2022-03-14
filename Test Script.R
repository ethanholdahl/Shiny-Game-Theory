install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

S1 = 2
S2 = 2

gameinfo = GenerateGame(S1,S2)

# GAME TABLE GRAPHIC

tabledata = MakeTableData(S1,S2, gameinfo[[1]])
p = ggplot()+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3))

p

# IEDS TABLE GRAPHIC

elimdata = IEDSTableData(S1, S2, gameinfo[[2]])
ggplot()+
  ggtitle("Game after IEDS")+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 15))+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3)) +
  annotate("rect", xmin = elimdata[[1]], xmax = elimdata[[2]], ymin = elimdata[[3]], ymax = elimdata[[4]], alpha = .6, fill = "black")

BRStars = BRTableData(gameinfo[[1]])


elimdata = IEDSTableData(S1, S2, gameinfo[[2]])
ggplot()+
  ggtitle("Game after IEDS")+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 15))+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3)) +
  annotate("text", x = BRStars$x, y = BRStars$y, size = 10, label = "*", color = BRStars$color)+
  annotate("rect", xmin = elimdata[[1]], xmax = elimdata[[2]], ymin = elimdata[[3]], ymax = elimdata[[4]], alpha = .6, fill = "black")



MakeExpectedPayoffGraphs(gameinfo)
gameinfo[[1]][gameinfo[[6]],gameinfo[[7]],]
