tryCatch(
  {
    remotes::install_github("jeffersonfparil/violinplotter")
    library(violinplotter)
  }, error=function(e){
    library(violinplotter)
  }
)
