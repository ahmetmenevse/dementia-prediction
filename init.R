my_packages = c('shiny', 'dplyr', 'randomForest', 'e1071', 'pROC', 'ggplot2', 'caret', 'shinythemes', 'VIM', 'DT')

install_if_missing = function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p, repos='http://cran.rstudio.com/')
  }
}

invisible(sapply(my_packages, install_if_missing))
