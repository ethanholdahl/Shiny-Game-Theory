library(ggplot2)



#2x2 game
game = makegame(2,2)
ggplot()+
  geom_vline(xintercept = seq(0,2))+
  geom_hline(yintercept = seq(0,2))+
  theme_void() +
  annotate("text", x = .25, y = 1.25, size = 15, label = game[1,1,1], color = "red")+
  annotate("text", x = 1.25, y = 1.25, size = 15, label = game[1,2,1], color = "red")+
  annotate("text", x = .25, y = .25, size = 15, label = game[2,1,1], color = "red")+
  annotate("text", x = 1.25, y = .25, size = 15, label = game[2,2,1], color = "red")+
  annotate("text", x = .75, y = 1.75, size = 15, label = game[1,1,2], color = "blue")+
  annotate("text", x = 1.75, y = 1.75, size = 15, label = game[1,2,2], color = "blue")+
  annotate("text", x = .75, y = .75, size = 15, label = game[2,1,2], color = "blue")+
  annotate("text", x = 1.75, y = .75, size = 15, label = game[2,2,2], color = "blue")+
  annotate("text", x = -.2, y = 1.5, size = 10, label = paste("Up"), color = "red")+
  annotate("text", x = -.2, y = .5, size = 10, label = paste("Down"), color = "red")+
  annotate("text", x = .5, y = 2.2, size = 10, label = paste("Left"), color = "blue")+
  annotate("text", x = 1.5, y = 2.2, size = 10, label = paste("Right"), color = "blue")+
  coord_cartesian(xlim =c(-.3, 2), ylim = c(0,2.3))

IEDSresult = IEDS(game)
dominated = IEDSresult[[1]]
BRAresult = BRA(game, dominated)
p1BRs = BRAresult[[1]]
p2BRs = BRAresult[[2]]
remain1 = BRAresult[[3]]
remain2 = BRAresult[[4]]
elim1 = BRAresult[[5]]
elim2 = BRAresult[[6]]
NE = FindPureNE(game, p1BRs, p2BRs, remain2)
if(length(remain1)==2 & length(remain2)==2){
  MixedNE = Find2x2MixedNE(game, remain1, remain2)
  MixedNE
  NE = c(NE,MixedNE)
}
dominated[[1]][dominated[[1]]<100]
dominated[[2]][dominated[[2]]<100]
NE




#3x5 game
game = makegame(3,5)
ggplot()+
  geom_vline(xintercept = seq(0,5))+
  geom_hline(yintercept = seq(0,3))+
  theme_void() +
  annotate("text", x = .25, y = 1.25, size = 15, label = game[2,1,1], color = "red")+
  annotate("text", x = 1.25, y = 1.25, size = 15, label = game[2,2,1], color = "red")+
  annotate("text", x = .25, y = .25, size = 15, label = game[3,1,1], color = "red")+
  annotate("text", x = 1.25, y = .25, size = 15, label = game[3,2,1], color = "red")+
  annotate("text", x = .75, y = 1.75, size = 15, label = game[2,1,2], color = "blue")+
  annotate("text", x = 1.75, y = 1.75, size = 15, label = game[2,2,2], color = "blue")+
  annotate("text", x = .75, y = .75, size = 15, label = game[3,1,2], color = "blue")+
  annotate("text", x = 1.75, y = .75, size = 15, label = game[3,2,2], color = "blue")+
  annotate("text", x = 2.25, y = 1.25, size = 15, label = game[2,3,1], color = "red")+
  annotate("text", x = 3.25, y = 1.25, size = 15, label = game[2,4,1], color = "red")+
  annotate("text", x = 2.25, y = .25, size = 15, label = game[3,3,1], color = "red")+
  annotate("text", x = 3.25, y = .25, size = 15, label = game[3,4,1], color = "red")+
  annotate("text", x = 2.75, y = 1.75, size = 15, label = game[2,3,2], color = "blue")+
  annotate("text", x = 3.75, y = 1.75, size = 15, label = game[2,4,2], color = "blue")+
  annotate("text", x = 2.75, y = .75, size = 15, label = game[3,3,2], color = "blue")+
  annotate("text", x = 3.75, y = .75, size = 15, label = game[3,4,2], color = "blue")+
  annotate("text", x = 4.25, y = 1.25, size = 15, label = game[2,5,1], color = "red")+
  annotate("text", x = 4.25, y = .25, size = 15, label = game[3,5,1], color = "red")+
  annotate("text", x = 4.75, y = 1.75, size = 15, label = game[2,5,2], color = "blue")+
  annotate("text", x = 4.75, y = .75, size = 15, label = game[3,5,2], color = "blue")+
  annotate("text", x = .25, y = 2.25, size = 15, label = game[1,1,1], color = "red")+
  annotate("text", x = 1.25, y = 2.25, size = 15, label = game[1,2,1], color = "red")+
  annotate("text", x = 2.25, y = 2.25, size = 15, label = game[1,3,1], color = "red")+
  annotate("text", x = 3.25, y = 2.25, size = 15, label = game[1,4,1], color = "red")+
  annotate("text", x = 4.25, y = 2.25, size = 15, label = game[1,5,1], color = "red")+
  annotate("text", x = .75, y = 2.75, size = 15, label = game[1,1,2], color = "blue")+
  annotate("text", x = 1.75, y = 2.75, size = 15, label = game[1,2,2], color = "blue")+
  annotate("text", x = 2.75, y = 2.75, size = 15, label = game[1,3,2], color = "blue")+
  annotate("text", x = 3.75, y = 2.75, size = 15, label = game[1,4,2], color = "blue")+
  annotate("text", x = 4.75, y = 2.75, size = 15, label = game[1,5,2], color = "blue")+
  annotate("text", x = -.3, y = 2.5, size = 10, label = paste("S1_1"), color = "red")+
  annotate("text", x = -.3, y = 1.5, size = 10, label = paste("S1_2"), color = "red")+
  annotate("text", x = -.3, y = .5, size = 10, label = paste("S1_3"), color = "red")+
  annotate("text", x = .5, y = 3.2, size = 10, label = paste("S2_1"), color = "blue")+
  annotate("text", x = 1.5, y = 3.2, size = 10, label = paste("S2_2"), color = "blue")+
  annotate("text", x = 2.5, y = 3.2, size = 10, label = paste("S2_3"), color = "blue")+
  annotate("text", x = 3.5, y = 3.2, size = 10, label = paste("S2_4"), color = "blue")+
  annotate("text", x = 4.5, y = 3.2, size = 10, label = paste("S2_5"), color = "blue")+
  coord_cartesian(xlim =c(-.4, 5), ylim = c(0,3.3))


IEDSresult = IEDS(game)
dominated = IEDSresult[[1]]
dominated[[1]][dominated[[1]]<100]
dominated[[2]][dominated[[2]]<100]
