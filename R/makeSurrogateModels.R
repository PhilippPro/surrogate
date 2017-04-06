#' Create surrogate models for different tasks
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.ids [\code{numeric}] ids of the tasks
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return surrogate model
makeSurrogateModels = function(measure.name, learner.name, task.ids, tbl.results, tbl.hypPars, tbl.metaFeatures, param.set){
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  for(i in seq_along(task.ids)) {
    task.idi = task.ids[i]
    task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures)
    mlr.task.measure = makeRegrTask(id = as.character(task.idi), subset(task.data, task.id == task.idi, select =  c("measure.value", names(param.set$pars))), target = "measure.value")
    mlr.lrn = makeLearner("regr.randomForest")
    mlr.mod.measure[[i]] = train(mlr.lrn, mlr.task.measure)
  }
  return(mlr.mod.measure)
}

