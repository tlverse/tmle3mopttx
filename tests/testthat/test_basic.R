context("Test ")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./")
  setwd("..")
  install("tstmle", build_vignettes = FALSE, dependencies = FALSE)
}

library(testthat)
library(sl3)
library(tstmle)
library(origami)

set.seed(1234)

data("sim_ts_s1_n50.rda")
data("sim_ts_s1.rda")

#Set library:
Q_library=list("Lrnr_mean", "Lrnr_glm_fast", "Lrnr_glmnet","Lrnr_randomForest","Lrnr_xgboost")
g_library=list("Lrnr_mean", "Lrnr_glm_fast", "Lrnr_glmnet","Lrnr_randomForest","Lrnr_xgboost")
blip_library=list("Lrnr_glm_fast", "Lrnr_glmnet","Lrnr_randomForest","Lrnr_xgboost", "Lrnr_nnls")

test_that("Single Intervention Context-Specific OPT with default parameters works", {

  res<-tstmleOPT(sim_ts_s1, Cy=5, Ca=5, V=10, Q_library, g_library, blip_library,
                stratifyAY = TRUE, gbounds=c(1e-4,1-1e-4), Qbounds=c(1e-4,1-1e-4), maxIter=1000)

  expect_equal(res$tmlePsi[1,], 0.5427367, tolerance = 0.01)
  expect_equal(res$tmleSD[1,], 0.1068668, tolerance = 0.01)
  expect_equal(res$steps, 1, tolerance = 0.01)

})
