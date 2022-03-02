

h20_gbm_tune <- function(path_data, status, type){
  library(h2o)
  
  h2o.init(enable_assertions = FALSE, nthreads = -1)
  nhanes_nchs_df <- h2o.importFile(path = path_data)
  
  ## pick a response for the supervised problem
  response <- status
  if(type == "binary"){
    ## the response variable is an integer, we will turn it into a categorical/factor for binary classification
    nhanes_nchs_df[[response]] <- as.factor(nhanes_nchs_df[[response]])
    
  }
  ## the response variable is an integer, leave as is
  nhanes_nchs_df[[response]] <- nhanes_nchs_df[[response]]
  ## use all other columns (except for the name) as predictors
  predictors <- setdiff(names(nhanes_nchs_df), c(response, "name"))
  
  
  splits <- h2o.splitFrame(
    data = nhanes_nchs_df,
    ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
    destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
  )
  train <- splits[[1]]
  valid <- splits[[2]]
  test  <- splits[[3]]
  
  
  ## Depth 10 is usually plenty of depth for most datasets, but you never know
  hyper_params = list( max_depth = seq(1,16,2) )
  #hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets
  grid <- h2o.grid(
    ## hyper parameters
    hyper_params = hyper_params,
    ## full Cartesian hyper-parameter search
    search_criteria = list(strategy = "Cartesian"),
    ## which algorithm to run
    algorithm="gbm",
    ## identifier for the grid, to later retrieve it
    grid_id="depth_grid",
    ## standard model parameters
    x = predictors,
    y = response,
    training_frame = train,
    validation_frame = valid,
    ## more trees is better if the learning rate is small enough
    ## here, use "more than enough" trees - we have early stopping
    ntrees = 10000,
    ## smaller learning rate is better
    ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
    learn_rate = 0.05,
    ## learning rate annealing: learning_rate shrinks by 1% after every tree
    ## (use 1.00 to disable, but then lower the learning_rate)
    learn_rate_annealing = 0.99,
    ## sample 80% of rows per tree
    sample_rate = 0.8,
    ## sample 80% of columns per split
    col_sample_rate = 0.8,
    ## fix a random number generator seed for reproducibility
    seed = 1234,
    ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
    stopping_rounds = 5,
    stopping_tolerance = 1e-4,
    stopping_metric = "MSE",
    ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
    score_tree_interval = 10
  )
  ## sort the grid models by decreasing RMSE
  sortedGrid <- h2o.getGrid("depth_grid", sort_by="MSE", decreasing = TRUE)
  ## find the range of max_depth for the top 5 models
  topDepths = sortedGrid@summary_table$max_depth[1:5]
  minDepth = min(as.numeric(topDepths))
  maxDepth = max(as.numeric(topDepths))
  
  hyper_params = list(
    ## restrict the search to the range of max_depth established above
    max_depth = seq(minDepth,maxDepth,1),
    ## search a large space of row sampling rates per tree
    sample_rate = seq(0.2,1,0.01),
    ## search a large space of column sampling rates per split
    col_sample_rate = seq(0.2,1,0.01),
    ## search a large space of column sampling rates per tree
    col_sample_rate_per_tree = seq(0.2,1,0.01),
    ## search a large space of the number of min rows in a terminal node
    min_rows = 2^seq(0,log2(nrow(train))-1,1),
    ## search a large space of the number of bins for split-finding for continuous and integer columns
    nbins = 2^seq(4,10,1),
    ## search a large space of the number of bins for split-finding for categorical columns
    nbins_cats = 2^seq(4,12,1),
    ## search a few minimum required relative error improvement thresholds for a split to happen
    min_split_improvement = c(0,1e-8,1e-6,1e-4),
    ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
    histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
  )
  search_criteria = list(
    ## Random grid search
    strategy = "RandomDiscrete",
    ## limit the runtime to 60 minutes
    max_runtime_secs = 3600,
    ## build no more than 100 models
    max_models = 100,
    ## random number generator seed to make sampling of parameter combinations reproducible
    seed = 1234,
    ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
    stopping_rounds = 5,
    stopping_metric = "MSE",
    stopping_tolerance = 1e-3
  )
  
  
  grid <- h2o.grid(
    ## hyper parameters
    hyper_params = hyper_params,
    ## hyper-parameter search configuration (see above)
    search_criteria = search_criteria,
    ## which algorithm to run
    algorithm = "gbm",
    ## identifier for the grid, to later retrieve it
    grid_id = "final_grid",
    ## standard model parameters
    x = predictors,
    y = response,
    training_frame = train,
    validation_frame = valid,
    ## more trees is better if the learning rate is small enough
    ## use "more than enough" trees - we have early stopping
    ntrees = 10000,
    ## smaller learning rate is better
    ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
    learn_rate = 0.05,
    ## learning rate annealing: learning_rate shrinks by 1% after every tree
    ## (use 1.00 to disable, but then lower the learning_rate)
    learn_rate_annealing = 0.99,
    ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
    max_runtime_secs = 3600,
    ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE",
    ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
    score_tree_interval = 10,
    ## base random number generator seed for each model (automatically gets incremented internally for each model)
    seed = 1234
  )
  
  
  sortedGrid <- h2o.getGrid("final_grid", sort_by = "mse", decreasing = TRUE)
  
  
  gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
  gbm_parameters <- gbm@parameters
  cat(paste0("The MSE on the test data is ", h2o.mse(h2o.performance(gbm, newdata = test))))
  h2o.shutdown(prompt = FALSE)
  return(gbm_parameters)
  
}

