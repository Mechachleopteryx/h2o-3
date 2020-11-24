setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../scripts/h2o-r-test-setup.R")

test.GAM.binomial <- function() {
  train <- h2o.importFile(locate("smalldata/glm_test/binomial_20_cols_10KRows.csv"))
  train$C21 <- h2o.asfactor(train$C21)
  train$C10 <- h2o.asfactor(train$C10)
  x = 10:11
  y = 21
  params                  <- list()
  params$missing_values_handling <- 'MeanImputation'
  params$training_frame <- train
  params$x <- x
  params$y <- y
  params$family <- "binomial"
  params$gam_columns <- c(c("c12"), c("C13", "C14"), c("C15", "C16", "C17", "C18"))
  params$bs <- c(1,1,1,0)
  params$scale <- c(0.001, 0.001, 0.001, 0.001)
  browser()
  modelAndDir<-buildModelSaveMojoGAM(params) # build the model and save mojo
  filename = sprintf("%s/in.csv", modelAndDir$dirName) # save the test dataset into a in.csv file.
  h2o.downloadCSV(htest[1:100, x], filename)
  twoFrames<-mojoH2Opredict(modelAndDir$model, modelAndDir$dirName, filename, col.types=c("enum", "numeric", "numeric")) # perform H2O and mojo prediction and return frames
  h2o.downloadCSV(twoFrames$h2oPredict, sprintf("%s/h2oPred.csv", modelAndDir$dirName))
  h2o.downloadCSV(twoFrames$mojoPredict, sprintf("%s/mojoOut.csv", modelAndDir$dirname))
  twoFrames$h2oPredict[,1] <- h2o.asfactor(twoFrames$h2oPredict[,1])
  compareFrames(twoFrames$h2oPredict,twoFrames$mojoPredict, prob=1, tolerance = 1e-6)
}

doTest("GAM Test: binomial GAM Mojo test with TP and CS smoothers for binomials", test.GAM.binomial)
