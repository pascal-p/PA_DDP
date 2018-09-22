
library(caret)
library(C50)
library(e1071)
library(gbm)
library(randomForest)
library(RWeka)

setSeed <- function(s=20180915) {
  set.seed(s)
}

setDataDir <- function(dir="data") {
  (!file.exists(dir)) && { dir.create(dir) }
  return(dir)
}

loadDS <- function (url_prefx, file, dir=setDataDir(), fsep=',', head=T) {
  fpath <- paste(dir, file, sep='/')
  if (!file.exists(fpath)) {
    download.file(url=paste(url_prefx, file, sep='/'), method="curl", destfile=fpath)
  }
  if (length(grep('\\.arff$', c(fpath), perl=T, value=F)) > 0) {
    d_f <- read.arff(fpath)
  }
  else {
    d_f <- read.table(fpath, na.strings=c('NA', ''), header=head, sep=fsep)
  }
  return(d_f)
}

cleanDS <- function(d_f, col2rm=c(1:6)) {
  # 1 - eliminate variable with low variance
  res <- nearZeroVar(d_f, saveMetrics=T)
  d_f_reduced <- d_f[ , !res$nzv]
  if (length(col2rm) > 0) {
    d_f_reduced <- d_f_reduced[, -col2rm]
  } 
  return(d_f_reduced)
}

ratioNA <- function(col) {
  if (length(col) == 0) { return(0.0) }
  sum(is.na(col)) / length(col)
}

cleanDS_fromNA <- function(d_f, ratio=0.9) {
  #  cols containing > ratio NA values
  cols_wo_na <- c()
  for (cname in (colnames(d_f))) {
    if (ratioNA(d_f[[cname]]) < ratio) {
      cols_wo_na <- c(cols_wo_na, cname)
    }
  }
  return(d_f[names(d_f) %in% cols_wo_na])
}

setClassVar <- function(d_f, colname, classname='Class') {
  names(d_f)[names(d_f) == colname] <- classname
  return(d_f)
}

splitDS <- function(d_f, objective='Class', cut=0.7) {
  mySet <- createDataPartition(d_f[[objective]], p=cut, list=F)
  trainSet <- d_f[mySet, ]
  validSet <- d_f[-mySet, ]
  return(list(trainSet, validSet))
}

prepDS <- function(d_f, col2rm=c(), ratio=0.9) {
  d_f <- cleanDS(d_f, col2rm=col2rm)
  d_f <- cleanDS_fromNA(d_f, ratio)
  return(d_f)
}

trCtrl <- function(meth="repeatedcv", k=10, rep=3) {
  if (meth == "repeatedcv") {
    trainControl(method=meth, number=k, repeats=rep, # classProbs=T,
                 verboseIter=F, savePredictions='final')
  } else if (meth == "cv") {
    trainControl(method=meth, number=k,
                 verboseIter=F, savePredictions=F)
  } else {
    # fallback repeatedcv
    trainControl(method=meth, number=k, repeats=rep, # classProbs=T,
                 verboseIter=F, savePredictions='final')
  }
}

# Assumption objective (the var we want to predict) is always named Class

predRes <- function(model, testSet) {
  pred <- predict(model, newdata=testSet)
  
  # Eval out-of-sample-error == oose
  resCM <- confusionMatrix(testSet$Class, pred)
  oose <- 1 - resCM$overall[[1]]
  
  return(list(model, pred, resCM, oose))
}

# legendFn <- function() {
#   list(list("algorithm", "Algorithm used"), 
#        list("acc_train", "Accurary on training set"), 
#        list("kappa_train", "Kappa on training set"),
#        list("acc_test",   "Accurary on testing set"), 
#        list("kappa_test", "Kappa on testing set"),
#        list("oose", "Out-Of Sample Error, calculated on testing set"), 
#        list("utime", "User time taken (in seconds)")
#        )
# }

legendFn <- function() {
  list(list('algorithm:', "Algorithm used", "acc_train:", "Accurary on training set"), 
       list("kappa_train:", "Kappa on training set", "acc_test:", "Accurary on testing set"), 
       list("kappa_test:", "Kappa on testing set", "oose:", "Out-Of Sample Error, calculated on testing set"), 
       list("utime:", "User time taken (in seconds)", "", "")
  )
}

resDF <- function(df=NULL, model=model, label=label, resCM=res, oose=oose) {
  ix_max      <- which.max(model$results$Accuracy)
  algorithm   <- c(label) # "DT"
  acc_train   <- c(model$results$Accuracy[[ix_max]])
  kappa_train <- c(model$results$Kappa[[ix_max]])
  
  acc_test   <- c(resCM$overall[[1]])
  kappa_test <- c(resCM$overall[[2]])
  oose       <- c(oose)
  utime      <- model$times$everything[[1]]
  
  ndf <- data.frame(algorithm, acc_train, kappa_train, acc_test, kappa_test, oose, utime)
  if (exists("df")) { rbind(df, ndf) }
  else { ndf }
}

#
# Boosting
#

modC50fn <- function(trainSet, testSet, trctrl=trCtrl("cv", k=5), metric="Accuracy") {
  setSeed()
  print(paste0("== DEBUG: model C5.0...", "metric: ", metric, " Be patient..."))
  model <- train(Class ~ ., data=trainSet, method="C5.0", metric=metric,
                 trControl=trctrl)
  predRes(model, testSet)
}

modGBMfn <- function(trainSet, testSet, trctrl=trCtrl("cv", k=5), metric="Accuracy") {
  setSeed()
  print(paste0("== DEBUG: model GBM...", "metric: ", metric, " Be patient..."))
  model <- train(Class ~ ., data=trainSet, method="gbm", metric=metric,
                 verbose=F, trControl=trctrl)
  predRes(model, testSet)
}

#
# Bagging
#

modBCARTfn <- function(trainSet, testSet, trctrl=trCtrl("cv", k=5), metric="Accuracy") {
  setSeed()
  print(paste0("== DEBUG: model Bagged CART...", "metric: ", metric, " Be patient..."))
  model <- train(Class ~ ., data=trainSet, method="treebag", metric=metric,
                 trControl=trctrl)
  predRes(model, testSet)
}

modBRFfn <- function(trainSet, testSet, trctrl=trCtrl("cv", k=5), metric="Accuracy") {
  setSeed()
  print(paste0("== DEBUG: model Random Forest...", "metric: ", metric, " Be patient..."))
  model <- train(Class ~ ., data=trainSet, method="rf", metric=metric, 
                 verbose=F, trControl=trctrl)
  predRes(model, testSet)
}

#
# wrapper for convenience
#
modFunWrapper <- function(ds_train, ds_test, sum_df=NULL, myfun=modC50fn, label="C5.0", 
                          trctrl=trCtrl("cv", k=5), metric="Accuracy") {
  all <- myfun(ds_train, ds_test, trctrl=trctrl, metric)
  mod  <- all[[1]]
  pred <- all[[2]]
  res  <- all[[3]]
  oose <- all[[4]]
  sum_df <- resDF(sum_df, model=mod, label=label, resCM=res, oose=oose)
  return(sum_df)
}

indexOfBest <- function(df, cname="acc_test") {
  which.max(df[, cname])
}

preLoad <- function(fpath="cache/sum_df_ionosphere.RData") {
  if (file.exists(fpath)) {
    print(paste0("== DEBUG: OK... file ", fpath, " exists? ", file.exists(fpath)))
    load(fpath)
    return(sum_df)
  } 
  else {
    print(paste0("DEBUG: KO... file ", fpath, " does NOT exist"))
    NULL
  }
}
