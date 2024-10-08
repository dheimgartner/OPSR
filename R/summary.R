summary.opsr <- function(object, ...) {
  model <- object
  ## https://github.com/joemolloy/fast-mixed-mnl/blob/master/R/model_stats.R

  ## for regular varcov => see notes and Xinyi's example

  robvarcov <- sandwich::sandwich(model)
}
