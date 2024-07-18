my_packages = c('shiny', 'dplyr', 'randomForest', 'e1071', 'pROC', 'ggplot2', 'caret', 'shinythemes', 'VIM', 'DT', 'devtools')

install_if_missing = function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p, repos='http://cran.rstudio.com/')
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

# Matrix paketinin belirli bir sürümünü yükleyin
devtools::install_version("Matrix", version = "1.5-1", repos = "http://cran.rstudio.com/")

# Diğer paketleri yükleyin
invisible(sapply(my_packages, install_package_with_retry))

