install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

S1 = 5
S2 = 5

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


#IEDS TABLE GRAPHIC

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
  annotate("rect", xmin = elimdata[[2]], xmax = elimdata[[3]], ymin = elimdata[[4]], ymax = elimdata[[5]], alpha = .6, fill = "black")




#Debugging
game = array(c(9,16,-6,-5,14,1,20,15,-2,14,16,-10,3,2,8,4,11,9,19,-8,1,1,8,-3,9,2,6,6,5,10,-6,-10,12,13,-8,-8), dim =c(6,3,2))
game
#Takes number of Player 1 and Player 2 strategies as inputs
#Returns a normal form game with randomly generated payoffs, list of dominated strategies, best response analysis, list of pure NE, list of all NE (in Nx2 or 2xN games)
game = MakeGame(S1, S2)

#game = array(c(0, 0, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2))
#game = array(c(5, -1, -10, 3, -5, 3, -6, 19, 2, -2, 9, 11, -5, -1, 14, -4, 8, 4), dim = c(3, 3, 2))
#game = array(c(3, 10, 12, 4, 15, 17, 7, 6, 10, 4, 4, 8), dim = c(2, 3, 2))

IEDSresult = IEDS(game)

#IEDS animation: color row/column bright red + green for dominated + dominant rows/columns. Then bright green fade back to normal and bright red fade to dark red.

dominated = IEDSresult[[1]]
dominators = IEDSresult[[2]]
#game
game[-dominated[[1]],-dominated[[2]], ]


BRAresult = BRA(game, dominated)
p1BRs = BRAresult[[1]]
p2BRs = BRAresult[[2]]
remain1 = BRAresult[[3]]
remain2 = BRAresult[[4]]
elim1 = BRAresult[[5]]
elim2 = BRAresult[[6]]

NE = FindPureNE(game, p1BRs, p2BRs, remain2)
PureNE = NE
if (length(remain1) == 2 & length(remain2) == 2) {
  MixedNE = Find2x2MixedNE(game, remain1, remain2)
  MixedNE
  NE = c(NE, list(MixedNE))
} else {
  if (length(remain1) == 2 | length(remain2) == 2) {
    MixedNE = FindNx2MixedNE(game, remain1, remain2, p1BRs, p2BRs)
    NE = c(NE, MixedNE)
  }
}

NE = NE[which(!(sapply(NE, is.null)))]

NE

ContinuousNE = FindContinuous(NE)

ContinuousNE
