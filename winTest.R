library(tidyverse)
library(GGally)
library(caret)
library(WRCHSVM)

source('commonFuns.R')

wine <- read_csv('Data/Wine.csv')

names(wine) <- c('type',
                 'alcohol',
                 'malicAcid',
                 'ash',
                 'alcAsh',
                 'mg',
                 'phenols',
                 'flav',
                 'nonflav',
                 'proanth',
                 'color',
                 'hue',
                 'code',
                 'proline')

smWine <- wine %>% select(type,alcohol,flav,mg)
smWinePlot <- smWine
smWinePlot$type <- as.factor(smWinePlot$type)
ggpairs(smWinePlot,columns=2:ncol(smWinePlot),mapping=ggplot2::aes(colour = type), upper = "blank")
ggplot(smWinePlot, aes(x=alcohol, y = flav, color = type)) + geom_point()
smWine[2:4] <- as.tibble(lapply(smWine[2:4], normalize))

out <- WSVM(smWine, kernel="monomial")


#####Test new B
ptsList <- lapply(1:nrow(smWine), function(n) as.double(c(smWine[n,2],smWine[n,3],smWine[n,4])))
pred <- sapply(ptsList, function(pt) {
  out$predict(pt)
})

confusion <- confusionMatrix(pred, smWine$type)
confusion
