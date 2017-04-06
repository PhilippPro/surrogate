#' Merge results, hyperpars and features tables and prepare for mlr.task input
#' @param measure.name.filter What measure to analyse
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMetaFeatures()
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL){
  measure.name.filter = measure.name
  learner.name.fiter = learner.name
  if (!is.null(tbl.metaFeatures)) {
    bot.table = tbl.results %>% 
      filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
      inner_join(., tbl.metaFeatures, by = "task.id") %>%
      inner_join(., tbl.hypPars, by = "run.id") %>%
      select(., -measure.name, -flow.name, -flow.id, -run.id, -data.id, -name,
        -setup.id, -data.name, -upload.time, -flow.version, -learner.name) %>%
      spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE)
    bot.table$user.time = tbl.results[tbl.results$measure.name == "usercpu.time.millis" & tbl.results$learner.name == learner.name.fiter, "measure.value"]
    bot.table$measure.value = as.numeric(bot.table$measure.value)
    bot.table$user.time = as.numeric(bot.table$user.time)
  } else {
    bot.table = tbl.results %>% 
      filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
      inner_join(., tbl.hypPars, by = "run.id") %>%
      select(., -measure.name, -flow.name, -flow.id, -setup.id, -data.name, -upload.time, -flow.version, -learner.name, -flow.source) %>%
      spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE) %>%
      select(., -run.id)
  }
  bot.table = convertDataFrameCols(bot.table, chars.as.factor = TRUE)
  return(bot.table)
}

#' Create table for a specific task with random hyperpars
#' @param task.id.filter What task to use
#' @param learner.name What learner to use
#' @param lrn.ps.sets What learner to use
#' @param n Number of rows to create
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for exploring surrogate model 
makeRandomTable = function(task.id.filter, learner.name, lrn.ps.sets, n, tbl.metaFeatures){
  
  for(i in lrn.ps.sets){
    if(paste0("mlr.", i$learner$id) == learner.name){
      rnd.ps = generateRandomDesign(n, i$param.set)
    }
  }
  if(!exists("rnd.ps")){
    stop(paste("No learner in lrn.ps.sets with id:", sub("mlr.", "", learner.name)))
  }
  rnd.table = tbl.metaFeatures[tbl.metaFeatures$task.id == task.id.filter,][rep(1,n), ]
  rnd.table = cbind(rnd.table, rnd.ps) 
  rnd.table = rnd.table %>% select(., -data.id, -name) 
  return(rnd.table)
}

#' Create random hyperpars for a flow and predict the measure and time for a given task.
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.id ID of the task
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param n Number of points to create
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] df with hyperpars of flow and matching predictions for measure value and time
makeMeasureTimePrediction = function(measure.name, learner.name, task.id, lrn.par.set, n, 
  tbl.results, tbl.hypPars, tbl.metaFeatures, time = FALSE){
  
  #train mlr model on full table for measure and runtime
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures)
  mlr.task.measure = makeRegrTask("measure.value", subset(task.data, select = -user.time), target = "measure.value")
  mlr.task.time = makeRegrTask("user.time", subset(task.data, select = -measure.value), target = "user.time")
  mlr.lrn = makeLearner("regr.randomForest")
  mlr.mod.measure = train(mlr.lrn, mlr.task.measure)
  mlr.mod.time = train(mlr.lrn, mlr.task.time)
  
  #predict new table for measure & runtime
  pred.data = makeRandomTable(task.id, learner.name, lrn.par.set, n, tbl.metaFeatures)
  mlr.pred.measure = predict(mlr.mod.measure, newdata = pred.data)
  mlr.pred.time = predict(mlr.mod.time, newdata = pred.data)
  pred.data$pred.measure.value = mlr.pred.measure$data$response
  pred.data$pred.time = mlr.pred.time$data$response
  rownames(pred.data) = NULL
  
  return(pred.data[,12:ncol(pred.data)])
}

#' Create surrogate models for different tasks
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.ids [\code{numeric}] ids of the tasks
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return surrogate model
makeSurrogateModel = function(measure.name, learner.name, task.ids, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, param.set){
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
