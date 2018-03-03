library(tidyverse)
library(WRCHSVM)
library(caret)
library(imager)
library(e1071)

imgsMat <- matrix(ncol = 785, nrow = 0)
root <- "mnist_png/training/"
for (i in 0:9) {
  imgList <- list.files(paste0(root,i))
  for (imgName in imgList[1:200]) {
    img <- load.image(paste0(root,i,"/",imgName))
    imgsMat <- rbind(imgsMat, c(i,img %>% grayscale() %>% as.vector()))
  }
  print(paste0("finished ",i))
}

##############
testMat <- matrix(ncol = 785, nrow = 0)
root <- "mnist_png/training/"
for (i in 0:9) {
  imgList <- list.files(paste0(root,i))
  for (imgName in imgList[201:220]) {
    img <- load.image(paste0(root,i,"/",imgName))
    testMat <- rbind(testMat, c(i,img %>% grayscale() %>% as.vector()))
  }
  print(paste0("finished ",i))
}
#####

imgsDF <- as.data.frame(imgsMat)
imgsDF$V1 <- as.character(imgsDF$V1)

out <- WRCHSVM::WSVM(df = imgsDF, kernel="rbf", gamma=.05, mu=.2, max_its = 200000)
#####

pred <- vector(mode="numeric")
for (i in 1:nrow(testMat)) {
  pred <- c(pred, out$predict(as.vector(testMat[i,-1])))
}

confusion <- confusionMatrix(pred, testMat[,1])
confusion

out$predict(load.image("test1.png") %>% grayscale() %>% as.vector())
