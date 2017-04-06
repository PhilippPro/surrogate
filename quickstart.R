library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
tag = "mlrRandomBot"
numRuns = 140000
results = do.call("rbind", 
  lapply(0:floor(numRuns/10000), function(i) {
    return(listOMLRuns(tag = tag, limit = 10000, offset = (10000 * i) + 1))
  })
)
table(results$flow.id, results$task.id)
table(results$uploader)

res = do.call("rbind", 
  lapply(0:floor(nrow(results)/100), function(i) {
    return(listOMLRunEvaluations(run.id = results$run.id[((100*i)+1):(100*(i+1))]))
  })
)
# dauert ewig
df = res %>% 
  mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
    learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]"))
as.data.frame.matrix(table(df$learner.name, df$data.name))

# -----------------------------------------------------------------------------------------------------------

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



