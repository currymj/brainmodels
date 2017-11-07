library(tidyverse)
library(reticulate)
library(abind)
library(pbapply)

library(reticulate)

source("R/getdata.R")

use_condaenv("factorial")
lsm <- import("lsm")
lsm.utils <- import("lsm.utils")
np <- import("numpy")

model_prep <- function(model, allAs, masks) {
  for (i in 1:N) {
    model$add_data(allAs[i,,], mask=masks[,,i])
  }
}

make_sampler <- function(model) {
  function(sample_num) {
    model$resample()

    if(hasName(model, "hs")) {
      model_list <- list(
        hs=model$hs,
        edgeprobs=map(seq(0L,length(model$As)-1L), model$edge_probabilities)
      )} else {
        model_list <- list(
          edgeprobs=map(seq(0L,length(model$As)-1L), model$edge_probabilities)
        )
      }
    list(
      model=model_list,
      df=data.frame(
        name=model$name,
        iteration=sample_num,
        log_prior=model$log_prior(),
        log_likelihood=model$log_likelihood(),
        hll=model$heldout_log_likelihood()
      ))
  }
}

fit_model <- function(model, n_iters = 50, progress_bar = TRUE) {
  sampler <- make_sampler(model)
  if (progress_bar) {
    result <- pblapply(1:n_iters, sampler)
  } else {
    result <- map(1:n_iters, sampler)
  }

  result_models <- map(result, ~ .$model)
  result_df <- map(result, ~ .$df) %>% bind_rows

  list(
    model=result_models,
    df=result_df
  )
}

edgeprob_matrix <- function(model_output) {
  last_model <- tail(model_output$model, n=1)[[1]]

}
