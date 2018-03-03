library(tidyverse)
library(ggplot2)
library(WRCHSVM)
library(fpCompare)
library(fivethirtyeight)
library(ggthemes)

five38Mod <- theme_fivethirtyeight() + theme(legend.position = "right",
                                             legend.direction = 'vertical',
                                             axis.title = element_text())
theme_set(five38Mod)

set.seed(123)

genRand <- function() { c(runif(1, -1, 1), runif(1, -1, 1)) }

poop <- as.data.frame(as.list(c(genRand() / 2, 1)))
for (i in 1:200) {
  poop <- rbind(poop, as.list(c(genRand() / 2, 1)))
}
colnames(poop) <- c('x', 'y', 'class')


topCirc <- function(x) { sqrt(-(x) ^ 2 + .4 ^ 2) }
botCirc <- function(x) {-sqrt(-(x) ^ 2 + .4 ^ 2) }
topCirc2 <- function(x) { sqrt(-(x) ^ 2 + .15 ^ 2) }
botCirc2 <- function(x) {-sqrt(-(x) ^ 2 + .15 ^ 2) }
poop <- as.tibble(poop)
todel <- 1
for (i in 1:nrow(poop)) {
  margin <- .04
  if (poop$x[i] > -0.4 & poop$x[i] < .4) {
    if (poop$y[i] < topCirc(poop$x[i]) & poop$y[i] > botCirc(poop$x[i])) {
      poop$class[i] <- -1
    }
    if (abs(poop$y[i] - topCirc(poop$x[i])) < margin | abs(poop$y[i] - botCirc(poop$x[i])) < margin) {
      todel <- c(todel, i)
    }
  }
}



poop <- poop[-todel,]

for (i in 1:nrow(poop)) {
  margin <- .2
  if (poop$x[i] > -0.15 & poop$x[i] < .15) {
    if (poop$y[i] < topCirc2(poop$x[i]) & poop$y[i] > botCirc2(poop$x[i])) {
      poop$class[i] <- -2
    }
    if (abs(poop$y[i] - topCirc2(poop$x[i])) < margin | abs(poop$y[i] - botCirc2(poop$x[i])) < margin) {
      todel <- c(todel, i)
    }
  }
}

ggplot(poop, aes(x = x, y = y)) +
  geom_point(aes(color = as.character(class)), size=2) +  xlim(-.5, .5) + ylim(-.5, .5)

tester <- poop[, c(3, 1, 2)]

out <- WSVM(tester, kernel = "monomial", mu = 1, max_its = 20000)

correct <- 0
for (i in 1:nrow(tester)) {
  if (out$predict(c(tester$x[i], tester$y[i])) == as.character(tester$class[i])) {
    correct <- correct + 1
  }
}
print (correct/nrow(tester))

#PLOT IT
step <- .005
dtInit <- as.data.frame(list(x = 1, y = 1, w = "1"))
xVal <- -0.5
while (xVal %<=% 0.50) {
  yVal <- -0.5
  while (yVal %<=% 0.50) {
    dtInit <- rbind(dtInit, as.data.frame(list(x = xVal, y = yVal, w = out$predict(c(xVal, yVal)))))

    yVal <- yVal + step
  }
  xVal <- xVal + step
}

ggplot(dtInit[-1,], aes(x = x, y = y)) +
  scale_fill_manual(values=c("#619CFF","salmon","#00BA38"))+
  geom_raster(aes(fill = w)) +
  scale_colour_manual(values = c("red", "green", "blue")) +
  geom_point(data = poop, aes(x = x, y = y, color = as.character(class)), size=2) + labs(fill = 'w-b')
