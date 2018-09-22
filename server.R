#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# order matters
source("./support/fun.R")
source("./support/plot.R")
source("./support/serverHlpr.R")

## Define server logic required to draw a histogram
shinyServer(function(input, output) {
  c_objective <- 'Class'
  c_metric    <- 'Accuracy'
  url         <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/'
  dir         <- './cache'
  algoLst     <- c("C5.0", "GBM", "BCART", "RF")

  ## First time  use the default - load from cache
  if (!exists("first_time")) {
    first_time <- TRUE
    c_cvType       <- 'cv'
    c_trainSetPerc <- 0.7
    dsname         <- "ionosphere"
    r <- loadFromCache(paste0(dir, "/", "sum_df_ionosphere.data_Accuracy_70p_cv.RData"),
                       url, dsname, c(2), "V35", c_objective, c_trainSetPerc)
    print(paste0("==> DEBUG: LoadFromCache completed for dataset: ", dsname))
    sum_df   <- r[[1]]
    ds_train <- r[[2]]
    ds_test  <- r[[3]]
    nds      <- r[[4]]
  }

  ## On dataset change, re-load new dataset, re-calculate partition ds_train, ds_test (from nds)
  react_ds <- reactive({
    if (input$dataSet == "ionosphere") {
      url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/'
      print("==> DEBUG: LoadAndPrepDS for dataset: ionosphere.data")
      nds <- loadAndPrepDS(url, 'ionosphere.data', "V35", c(2), c_objective)
    }
    else if (input$dataSet == "phishing") {
      url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00379/'
      print("==> DEBUG: LoadAndPrepDS for dataset: PhishingData.arff")
      nds <- loadAndPrepDS(url, 'PhishingData.arff', "Result", c(), c_objective)
    }

    if (dsname != input$dataSet || input$trainSetPerc != c_trainSetPerc) {
      all_ds    <- splitDS(nds, objective=c_objective, cut=input$trainSetPerc)
      rds_train <- all_ds[[1]] # == 247x33
      rds_test  <- all_ds[[2]] # == 104x33
      rm(all_ds)
      lst <- list(rds_train, rds_test, nds)
    }
    else {
      lst <- list(ds_train, ds_test, nds)
    }
    lst
  })

  ## Pre-compute once, within reactive conductor
  react_dfs <- reactive({
    ds_train     <- react_ds()[[1]]
    ds_test      <- react_ds()[[2]]
    dataset      <- input$dataSet
    percTrainSet <- input$trainSetPerc
    cvType       <- input$cvType
    metric       <- input$metric

    print(paste0("==> DEBUG: Precompute or cache? ", dataset , " WAS: ", dsname, " - cv: ", cvType, " - perc tr: ", percTrainSet, " - metric: ", metric))
    print(paste0("==> DEBUG: Precompute or cache? c_cv: ", c_cvType, " - c_perc tr: ", c_trainSetPerc, " - c_metric: ", c_metric))

    if (!exists("sum_df") || percTrainSet != c_trainSetPerc || dataset != dsname ||
        cvType != c_cvType || metric != c_metric) {
      ##
      ## change in the UI => recompute, but first check if model is already computed (cached)
      ##
      if (dataset == "ionosphere" && percTrainSet %in% c(.7, .8, .9)) {
        print("==> DEBUG: CACHE: ionosphere")
        url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/'
        r <- loadFromCache(paste0("../../cache/sum_df_", dataset, ".data_", metric, "_",
                                  percTrainSet * 100, "p_", cvType, ".RData"),
                           url, dataset, c(2), "V35", c_objective, percTrainSet)
        sum_df <- r[[1]]
        ds_train <- r[[2]] # update "external" cached value
        ds_test  <- r[[3]] # update "external" cached value
        nds <- r[[4]]
        dsname <- dataset
        c_trainSetPerc <- percTrainSet
        sum_df
      }
      else if (dataset == "phishing" && percTrainSet %in% c(.7, .8, .9)) {
        print("==> DEBUG: CACHE: phishing")
        url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00379/'
        r <- loadFromCache(paste0("../../cache/sum_df_PhishingData.arff_", metric, "_",
                                  percTrainSet * 100, "p_", cvType, ".RData"),
                           url, "PhishingData", c(), "Result", c_objective, percTrainSet, suffx='.arff')
        sum_df   <- r[[1]]
        ds_train <- r[[2]] # update "external" cached value
        ds_test  <- r[[3]] # update "external" cached value
        nds      <- r[[4]]
        dsname <- dataset
        c_trainSetPerc <- percTrainSet
        sum_df
      }
      else if (percTrainSet != c_trainSetPerc || cvType != c_cvType || input$dataSet != dsname) {
        print("==> DEBUG: NO CACHE")
        ## Cache value does not exist compute!
        res <- modFunWrapper(ds_train, ds_test, sum_df=NULL, myfun=modC50fn, label="C5.0", trctrl=trCtrl(meth=cvType, k=5), metric=metric)
        res <- modFunWrapper(ds_train, ds_test, sum_df=res, myfun=modGBMfn, label="GBM", trctrl=trCtrl(meth=cvType, k=5), metric=metric)
        res <- modFunWrapper(ds_train, ds_test, sum_df=res, myfun=modBCARTfn, label="BCART", trctrl=trCtrl(meth=cvType, k=5), metric=metric)
        res <- modFunWrapper(ds_train, ds_test, sum_df=res, myfun=modBRFfn, label="RF", trctrl=trCtrl(meth=cvType, k=5), metric=metric)
        #
        ## update external cached values
        c_trainSetPerc <- percTrainSet
        c_cvType <- cvType
        dsname   <- input$dataSet
        sum_df
      }
    }
    else {
      sum_df # as pre-loaded
    }
  })

  ## Select/Unselect algos
  react_res <- reactive({
    sum_df   <- react_dfs()
    selAlgos <- input$algorithms
    
    for (algo in algoLst) {
      sum_df <- toggleSel(algo, selAlgos, sum_df)
    }
   list(sum_df, sum(sum_df$utime))
  })

  ## output portion

  ## Main tab - Summary dataset
  output$dataset <- renderText({
    dsname <- input$dataSet
    dim_tr <- dim(react_ds()[[1]])
    dim_te <- dim(react_ds()[[2]])
    dim_ds <- dim(react_ds()[[3]])
    datasetHlpr(dsname, input$trainSetPerc, input$cvType, dim_ds, dim_tr, dim_te)
  })

  ## DataSet tab
  output$datasetHead <- renderDataTable({react_ds()[[3]]})

  ## Main tab - Results
  output$resultTable <- renderText({
    if (length(input$algorithms) > 0) {
      paste0("<h4>Result Table<h4>")
    }
  })
  output$result <- renderTable({
    if (length(input$algorithms) > 0) {
      react_res()[[1]]
    }
    }, striped=T, hover=T, bordered=T, digits=4,
       spacing="s")

  ## Main tab - Legend
  output$legendtitle <- renderText({
    if (length(input$algorithms) > 0) {
      paste0("<h5>Legend:</h5>")
    }
  })

  output$legend <- renderTable({
    if (length(input$algorithms) > 0) {
      legendHlpr()
    }},
    striped=F, hover=F, bordered=F, width="650px",
    colnames=F, align="l", spacing="s")

  ## Main tab - Selected Algorithms 
  output$selection <- renderText({
    if (length(input$algorithms) > 0) {
      jx <- indexOfBest(react_res()[[1]])
      algoSelHlpr(input$algorithms, jx, round(react_res()[[2]], 3))
    }
  })

  ## Main tab - Best Algorithm (summary) according to metric on given dataset
  output$bestSelection <- renderText({
    if (length(input$algorithms) > 0) {
      bestHlpr(react_res()[[1]], input$metric)
    } else {
      ""
    }
  })

  ## Plot tab 
  output$plot1 <- renderPlot({
    print("==> DEBUG: Plotting called...1")
    if (input$withPlots == 'yes' && length(input$algorithms) > 1) {
      gpArrange(gpWrapper(react_res()[[1]]))
    }
    else {
      ''
    }
  }, width=800, height=600, res=72)

  ## About tab
  output$about <- renderText({aboutHlpr()})

})
