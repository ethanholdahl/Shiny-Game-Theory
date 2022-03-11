install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

S1 = 6
S2 = 4

game = makegame(S1, S2)

#game = array(c(0, 0, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2))
#game = array(c(5, -1, -10, 3, -5, 3, -6, 19, 2, -2, 9, 11, -5, -1, 14, -4, 8, 4), dim = c(3, 3, 2))
#game = array(c(3, 10, 12, 4, 15, 17, 7, 6, 10, 4, 4, 8), dim = c(2, 3, 2))

IEDSresult = IEDS(game)

#IEDS animation: color row/column bright red + green for dominated + dominant rows/columns. Then bright green fade back to normal and bright red fade to dark red.

dominated = IEDSresult[[1]]
dominators = IEDSresult[[2]]
game
game[-dominated[[1]],-dominated[[2]], ]


BRAresult = BRA(game, dominated)
p1BRs = BRAresult[[1]]
p2BRs = BRAresult[[2]]
remain1 = BRAresult[[3]]
remain2 = BRAresult[[4]]
elim1 = BRAresult[[5]]
elim2 = BRAresult[[6]]

NE = FindPureNE(game, p1BRs, p2BRs, remain2)

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



#GAME TABLE GRAPHIC

S1 = 5
S2 = 3
game = makegame(S1,S2)

straty = c(.5-(1:S1), rep(.2, each = S2))
stratx = c(rep(-.2, each = S1), (1:S2)-.5)
stratnames = c(paste0("s",1:S1), paste0("s",1:S2))
stratcolors = c(rep("red", each = S1), rep("blue", each = S2))

payoffs = as.vector(game)
payoffsy =  c(.25-rep(1:S1, times = S2), .75-rep(1:S1, times = S2))
payoffsx = c(rep(1:S2, each = S1)-.75, rep(1:S2, each = S1)-.25)
payoffcolors = rep(c("red","blue"), each = S1*S2)

ggplot()+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = stratx, y = straty, size = 10, label = stratnames, color = stratcolors)+
  annotate("text", x = payoffsx, y = payoffsy, size = 10, label = payoffs, color = payoffcolors)+
  theme_void()+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3))

