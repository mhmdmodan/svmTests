library(tidyverse)
library(ggplot2)
library(fivethirtyeight)
library(ggthemes)
library(plot3D)
library(rgl)
library(plot3Drgl)

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
      poop$class[i] <- -.5
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

orig <- poop[, c(3, 1, 2)]
ind <- 0
for (i in seq(0,1,by=.01)) {
  ind <- ind+1
  curFrame <- orig
  curFrame$z <- i*8*(orig$x^2 + orig$y^2)
  png(paste0("plto/plot-",ind,".png"), width=700, height = 700)
  scatter3D(curFrame$x, curFrame$y, curFrame$z, 
            colvar = curFrame$class, col = c("#00BA38","salmon","#619CFF"),
            pch=19,
            zlim=c(0,2),
            surface=TRUE,
            scale.y=4,
            theta = 360*i,
            phi = 0,
            bty = "b2")
  dev.off()
}