library(tidyverse)

game = makegame(7,5)

IEDSresult = IEDS(game)

#IEDS animation: color row/column bright red + green for dominated + dominant rows/columns. Then bright green fade back to normal and bright red fade to dark red.

dominated = IEDSresult[[1]]
dominators = IEDSresult[[2]]
game
game[-dominated[[1]], -dominated[[2]],]


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
} else {
  if (length(remain1) == 2 | length(remain2) == 2){
    MixedNE = FindNx2MixedNE(game, remain1, remain2, p1BRs, p2BRs)
    NE = c(NE, MixedNE)
  }
}

NE

