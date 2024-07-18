my_packages = c('shiny', 'dplyr', 'randomForest', 'e1071', 'pROC', 'ggplot2', 'caret', 'shinythemes', 'VIM', 'DT')

install_if_missing = function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p, repos='http://cran.rstudio.com/')
  } else {
    message(paste("Package", p, "is already installed."))
  }
}

install_package_with_retry = function(p) {
  tryCatch(
    {
      install_if_missing(p)
    },
    error = function(e) {
      message(paste("Error installing package:", p))
      message("Retrying...")
      install.packages(p, repos='http://cran.rstudio.com/')
    }
  )
}

invisible(sapply(my_packages, install_package_with_retry))
