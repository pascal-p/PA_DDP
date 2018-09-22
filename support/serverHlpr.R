#
# serverHlpr.R helper functions
#

loadFromCache <- function(cacheEntry, url, dsname, c2rm, attr, objective,
                          trainSetPerc, suffx='.data') {
  sum_df <- preLoad(cacheEntry)
  nds <- loadDS(url, paste0(dsname, suffx), head=F)
  nds <- setClassVar(nds, attr, objective)
  nds <- prepDS(nds, col2rm=c2rm, ratio=0.9) # == 351x35
  #
  all_ds   <- splitDS(nds, objective=objective, cut=trainSetPerc) # default 70% for training
  ds_train <- all_ds[[1]]
  ds_test  <- all_ds[[2]]
  return(list(sum_df, ds_train, ds_test, nds))
}

loadAndPrepDS <- function(url, fname, attr, c2rm, objective) {
  nds <- loadDS(url, fname, head=F)
  nds <- setClassVar(nds, attr, objective)
  nds <- prepDS(nds, col2rm=c2rm)
}

toggleSel <- function(algo, algoLst, sum_df) {
  if (algo %in% algoLst) {
    sum_df
  }
  else {
    sum_df <- subset(sum_df, sum_df$algorithm != algo)
  }
  return(sum_df)
}

datasetHlpr <- function(dsName, perc, cvt, dim_ds, dim_tr, dim_te) {
  if (cvt == "cv") {
    cvt <- "simple k-fold cross validation, k=10"
  } else {
    cvt <- "3-repeated k-fold cross validation, k=10"
  }
  paste(
    '<div>
        <h4>DataSet Selected</h4>',
        '<p><i>', dsName, '</i> - dim: ', dim_ds[[1]], 'x',  dim_ds[[2]],
    ' - split: ', perc * 100, '%  - using: ', cvt, '</p>',
    '<ul>',
    '<li>Training dataset dim: ', dim_tr[[1]], 'x',  dim_tr[[2]], '</li>',
    '<li>Test dataset dim: ', dim_te[[1]], 'x',  dim_te[[2]], '</li>',
    '</ul>',
    '</div>', sep=" "
  )
}

# legendHlpr <- function() {
#   l <- legendFn()
#   l <- sapply(l, function(sl) { paste0('<li><b>', sl[[1]], '</b>: ', sl[[2]], '</li>', collapse='') },
#               simplify="array")
#   s <- paste0(l, collapse='')
#   paste0(
#     '<div>
#        <h5>Legend: </h5>',
#       '<ul>', s, '</ul>',
#     '</div>', collapse=''
#   )
# }

legendHlpr <- function() {
  l  <- legendFn()
  df <- data.frame(l[1],  stringsAsFactors=F)
  names(df) <- NULL
  df <- rbind(df, l[[2]])
  df <- rbind(df, l[[3]])
  df <- rbind(df, l[[4]])
}

algoSelHlpr <- function(lst, ix, totutime=0.0) {
  algoStr <- paste(lst, collapse = ",")

  if (nchar(algoStr) == 0) {
    algoStr <- paste0('<ul><li>', c('None'), '</li></ul>')
  }
  else {
    ary <- sapply(lst, function(s) { paste0('<li>', s, '</li>') },
                  simplify="array")
    ary[ix] <- paste0('<b>', ary[ix], '</b>') # mark best
    algoStr <- paste0(ary, collapse='')
    algoStr <- paste0('<ol>', algoStr, '</ol>', collapse='')
  }
  paste0(
    '<div>
         <h4>Algorithm(s) chosen <i>(total user time: ', totutime, ' sec.)</i></h4>',
    algoStr,
    '</div>'
  )
}

bestHlpr <- function(df, statistic) {
  jx <- indexOfBest(df)

  str <- paste0("<li>", "Algorithm: <b>", df[jx, 'algorithm'], "</b></li>")
  if (statistic == "Accuracy") {
    str <- paste0(str, "<li>", "Accuracy: <b>", round(df[jx, 'acc_test'], 4), "</b></li>")
  } else {
    # otherwise Kappa
    str <- paste0(str, "<li>", "Kappa: <b>", round(df[jx, 'kappa_test'], 4), "</b></li>")
  }
  str <- paste0(str, "<li>", "Oose: <b>", round(df[jx, 'oose'], 4), "</b></li>")
  str <- paste0(str, "<li>", "Execution time: <b><i>", round(df[jx, 'utime'], 4), " sec. </i></b></li>")

  paste0(
    '<div>
         <h4> Best Algorithm based on ', statistic, ' measured on <i>testing set</i></h4>',
    '<ul>', str, '</ul>',
    '</div>'
  )
}

.li <- function(l=list()) {
  s <- paste0(l, collapse=' ')
  paste0("<li>", s, "</li>")
}

.i <- function(l=list()) {
  s <- paste0(l, collapse=' ')
  paste0("<i>", s, "</i>")
}

.p <- function(l=list()) {
  s <- paste0(l, collapse=' ')
  paste0("<p>", s, "</p>")
}

aboutHlpr <- function() {
  # Ionosphere; Multivariate; Classification, Integer, Real, 351x34; 1989
  paste0("<h3>Another Toy Shiny Demo [<b>あとしで</b>]</h3>",
         "<h4>Short usage:</h4>",
      "<div><ul>",
      .li("Select the dataset (or keep default)"),
      .li("Select the percentage <em>p</em> to split dataset into two sets: Training <em>p%</em> and test <em>(100-p)%</em> <i>(or keep default 0.7 == 70%)</i>"),
      .li(list("Select one or several algorithms",  
               .i("(can be all)"))),
      .li(list("Select the cross validation strategy <br>", 
               .i("bear in mind that repeated cv is more CPU intensive and will require more time, defautl is cv)"))),
      .li(list("Select the metric for choosing the 'best' performing algorithm", 
              .i("(default Accuracy)"))),
      .li(list("Once at least one algorithm is chosen, you can select the basic plot option",
               .i("(displayed in the tab 'Plots', default is no)"))),
      .li("Et voila!"),
      "</ul><div>",

      "<h4>Notes:</h4>",
      "<div>",
      .p("As you can anticipate, re-running the algorithms will consume CPU, and require time. <i>By design</i> all the algoritms are re-run even if you have selected only one, likelihood is that you want to compare several of them."),
      .p("The aim here was to play with the idea of selecting and comparing a few algorithms (kept the list short to avoid waiting for too long) on some small datasets for classification and compare their performance in term of <i>accuracy</i> or <i>kappa</i>."),
      .p("In order to keep some fluidity, I then added caching on a subset of the results, using a specific seed."),
      "</div>",
      
      "<h4>Todo:</h4>",
      .p(list("There is a lot of room for improvment! to name a few", 
              .i("(on the assumption, that this could be useful): "))),
      "<ul>",
      .li("Add more datasets, and algorithms,"),
      .li("Allow for selecting a specific seed,"),
      .li("Allow for other cross validation strategies by also selecting the sub parameters (k for k-fold),"),
      .li("Allow for tuning of chosen algorithms,"),
      .li("Save results for a given set of parameters"),
      .li("Allow comparison among the saved results"),
      .li("Add prediction..."),
      "</ul>",
      "</div>",

      "<div style='display:block;float:right;width:50%;margin-left:10px;'>",
      "Pascal, September 2018 ", .i('(Corto Inc)'), 
      "</div>"
  )
}
