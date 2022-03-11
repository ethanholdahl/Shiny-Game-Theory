install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

S1 = 4
S2 = 3

gameinfo = GenerateGame(S1,S2)

#GAME TABLE GRAPHIC

tabledata = MakeTableData(S1,S2, gameinfo[[1]])

ggplot()+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3))
