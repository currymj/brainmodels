library(rio)
library(tidyverse)
library(Matrix)
download_data <- function() {
  dir.create("data", showWarnings = FALSE)
  system("cd data; bash ../download.sh")
}

import_data <- function() {
  matfiles <- list.files("data", pattern = "*.mat")
  raw_adjacency_matrices <- map(matfiles, ~ rio::import(file.path("data",.), "MAT")$fibergraph)
  return(raw_adjacency_matrices)
}
process_single_matrix <- function(cmat) {
  res <- cmat > 0
  res <- res | t(res)
  diag(res) <- 0
  return(as(res, "dgCMatrix"))
}

process_data <- function(to_process) {

  return(map(to_process, process_single_matrix))
}
