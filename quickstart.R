library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

create("surrogate")

overview = getMlrRandomBotOverview("botV1")
print(overview)

tbl.results = getMlrRandomBotResults("botV1")
print(tbl.results)

tbl.hypPars = getMlrRandomBotHyperpars("botV1")
print(tbl.hypPars)

task.data = makeBotTable(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.glmnet", 
  tbl.results = tbl.results, tbl.hypPars = tbl.hypPars, tbl.metaFeatures = NULL)

task.ids = unique(tbl.results$task.id)
surr = makeSurrogateModel(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.glmnet", 
  task.id = task.ids, tbl.results = tbl.results, tbl.hypPars = tbl.hypPars, param.set = lrn.par.set$classif.glmnet.set$param.set)

rnd.points = generateRandomDesign(10000, lrn.par.set$classif.glmnet.set$param.set)
preds = matrix(NA, nrow(rnd.points), length(surr))
for(i in seq_along(surr)) {
  preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
}

# Best default
average_preds = apply(preds, 1, mean)
average_preds[average_preds == max(average_preds)]
rnd.points[average_preds == max(average_preds), ]

# Tunability overall

# Tunability hyperparameter specific

# Interactions


