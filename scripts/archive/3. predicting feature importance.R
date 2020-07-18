####################################################################################################
####################################################################################################
####################################################################################################
library(dplyr)
library(xgboost)
library(data.table)
library(ggplot2)
require(ggrepel)
extrafont::loadfonts(device="win")
library(viridis)
library(gridExtra)
####################################################################################################
####################################################################################################

# predictors - training
rgr_xbg_dtrain_predictor <- as.matrix(rgr.msh.train[,6:ncol(rgr.msh.train)])
# repsonse - training basal growth rate
rgr_xbg_dtrain_response_bai_gr <-  rgr.msh.train$BAI_GR
# repsonse - training basal incriment growth rate
rgr_xbg_dtrain_response_bioi_gr <-  rgr.msh.train$BIOI_GR
# repsonse - training biomass growth rate
rgr_xbg_dtrain_response_biosh_gr <-  rgr.msh.train$BIOSH_GR

rgr_xbg_dtrain_bai_gr <- xgb.DMatrix(data = rgr_xbg_dtrain_predictor,
                                    label = rgr_xbg_dtrain_response_bai_gr)
rgr_xbg_dtrain_bioi_gr <- xgb.DMatrix(data = rgr_xbg_dtrain_predictor,
                                     label = rgr_xbg_dtrain_response_bioi_gr)
rgr_xbg_dtrain_biosh_gr <- xgb.DMatrix(data = rgr_xbg_dtrain_predictor,
                                     label = rgr_xbg_dtrain_response_biosh_gr)

# repsonse - training
rgr_xbg_dtest_predictor <- as.matrix(rgr.msh.test[,6:ncol(rgr.msh.test)])
# repsonse - testing basal growth rate
rgr_xbg_dtest_response_bai_gr <-  rgr.msh.test$BAI_GR
# repsonse - testing basal incriemnt growth rate
rgr_xbg_dtest_response_bioi_gr <-  rgr.msh.test$BIOI_GR
# repsonse - testing biomass growth rate
rgr_xbg_dtest_response_biosh_gr <-  rgr.msh.test$BIOSH_GR

rgr_xbg_dtest_bai_gr <- xgb.DMatrix(data = rgr_xbg_dtest_predictor,
                                    label = rgr_xbg_dtest_response_bai_gr)
rgr_xbg_dtest_bioi_gr <- xgb.DMatrix(data = rgr_xbg_dtest_predictor,
                                     label = rgr_xbg_dtest_response_bioi_gr)
rgr_xbg_dtest_biosh_gr <- xgb.DMatrix(data = rgr_xbg_dtest_predictor,
                                      label = rgr_xbg_dtest_response_biosh_gr)

bai_xbg_watchlist <- list(train = rgr_xbg_dtrain_bai_gr, eval = rgr_xbg_dtest_bai_gr)
bioi_xbg_watchlist <- list(train = rgr_xbg_dtrain_bioi_gr, eval = rgr_xbg_dtest_bioi_gr)
biosh_xbg_watchlist <- list(train = rgr_xbg_dtrain_biosh_gr, eval = rgr_xbg_dtest_biosh_gr)
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

############ BAI #######################
# which paramerters should I use?

# create hyperparameter grid
hyper_grid_bai <- expand.grid(
  eta = c(0.01, 0.05, 0.1,0.3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.65, 0.8, 1), 
  colsample_bytree = c(0.8, 0.9, 1),
  optimal_trees = 0,               # a place to dump results
  min_MERROR = 0  
)


# grid search 
for(i in 1:nrow(hyper_grid_bai)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid_bai$eta[i],
    max_depth = hyper_grid_bai$max_depth[i],
    min_child_weight = hyper_grid_bai$min_child_weight[i],
    subsample = hyper_grid_bai$subsample[i],
    colsample_bytree = hyper_grid_bai$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = rgr_xbg_dtrain_predictor,
    label = rgr_xbg_dtrain_response_bai_gr,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,                # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid_bai$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid_bai$min_MERROR[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid_bai %>%
  dplyr::arrange(min_MERROR) %>%
  head(10)
####################################################################################################
####################################################################################################

# now to set gamma, I will use the paramerters above
set.seed(100)

xgb_bai_cv <- xgb.cv(
  data = rgr_xbg_dtrain_predictor,
  label = rgr_xbg_dtrain_response_bai_gr,
  params = list(max_depth = 7, eta = 0.05, min_child_weigh = 1,
                subsample = 0.65, colsample_bytree = 1,
                verbose = 0, gamma = 15,
                objective = "reg:squarederror"),# try changing gamma
  nrounds = 5000,
  verbose = 0,
  nfold = 5,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)


# plot error vs number trees
ggplot(xgb_bai_cv$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")+
  ylim(0,2)


xgb_bai_cv$evaluation_log %>%
  arrange(train_rsme_mean) %>%
  head(10)
# if the training cv is much larger than test, I need to increase gamma, from 0 
# try starting at 10. If train/test cv is super close, then controlled way too much 
# the complexity of xgboost, and the model can't grow trees without pruning them 
# (due to the loss threshold not reached thanks to Gamma)
####################################################################################################
####################################################################################################
####################################################################################################

param_bai <- list(max_depth = 7, eta = 0.05, min_child_weigh = 1,
                  subsample = 0.65, colsample_bytree = 1,
                  verbose = 0, gamma = 15,
                  objective = "reg:squarederror")

bai_xbg_model_train <- xgb.train(param_bai, rgr_xbg_dtrain_bai_gr,
                                       nrounds = 5000,
                                       early_stopping_rounds = 10,
                                       watchlist = bai_xbg_watchlist)
# [63]	train-rmse:0.835824	eval-rmse:1.018252
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

############ bioi #######################
# which paramerters should I use?

# create hyperparameter grid
hyper_grid_bioi <- expand.grid(
  eta = c(0.01, 0.05, 0.1,0.3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.65, 0.8, 1), 
  colsample_bytree = c(0.8, 0.9, 1),
  optimal_trees = 0,               # a place to dump results
  min_MERROR = 0  
)


# grid search 
for(i in 1:nrow(hyper_grid_bioi)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid_bioi$eta[i],
    max_depth = hyper_grid_bioi$max_depth[i],
    min_child_weight = hyper_grid_bioi$min_child_weight[i],
    subsample = hyper_grid_bioi$subsample[i],
    colsample_bytree = hyper_grid_bioi$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = rgr_xbg_dtrain_predictor,
    label = rgr_xbg_dtrain_response_bioi_gr,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,                # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid_bioi$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid_bioi$min_MERROR[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid_bioi %>%
  dplyr::arrange(min_MERROR) %>%
  head(10)

####################################################################################################
####################################################################################################
####################################################################################################

####################################################################################################
####################################################################################################

# now to set gamma, I will use the paramerters above
set.seed(100)

xgb_bioi_cv <- xgb.cv(
  data = rgr_xbg_dtrain_predictor,
  label = rgr_xbg_dtrain_response_bioi_gr,
  params = list(max_depth = 5, eta = 0.05, min_child_weigh = 1,
                subsample = 0.65, colsample_bytree = 1,
                verbose = 0, gamma = 20,
                objective = "reg:squarederror"),# try changing gamma
  nrounds = 5000,
  verbose = 0,
  nfold = 5,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)


# plot error vs number trees
ggplot(xgb_bioi_cv$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")+
  ylim(0,3)


xgb_bioi_cv$evaluation_log %>%
  arrange(train_rmse_mean) %>%
  head(10)
# if the training cv is much larger than test, I need to increase gamma, from 0 
# try starting at 10. If train/test cv is super close, then controlled way too much 
# the complexity of xgboost, and the model can't grow trees without pruning them 
# (due to the loss threshold not reached thanks to Gamma)
####################################################################################################
####################################################################################################
####################################################################################################

param_bioi <- list(max_depth = 5, eta = 0.05, min_child_weigh = 1,
                   subsample = 0.65, colsample_bytree = 1,
                   verbose = 0, gamma = 35,
                   objective = "reg:squarederror")

bioi_xbg_model_train <- xgb.train(param_bioi, rgr_xbg_dtrain_bioi_gr,
                                  nrounds = 5000,
                                  early_stopping_rounds = 10,
                                  watchlist = bioi_xbg_watchlist)
#[96]	train-rmse:1.283951	eval-rmse:2.420790
####################################################################################################
####################################################################################################
####################################################################################################

############ biosh #######################
# which paramerters should I use?

# create hyperparameter grid
hyper_grid_biosh <- expand.grid(
  eta = c(0.01, 0.05, 0.1,0.3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.65, 0.8, 1), 
  colsample_bytree = c(0.8, 0.9, 1),
  optimal_trees = 0,               # a place to dump results
  min_MERROR = 0  
)


# grid search 
for(i in 1:nrow(hyper_grid_biosh)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid_biosh$eta[i],
    max_depth = hyper_grid_biosh$max_depth[i],
    min_child_weight = hyper_grid_biosh$min_child_weight[i],
    subsample = hyper_grid_biosh$subsample[i],
    colsample_bytree = hyper_grid_biosh$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = rgr_xbg_dtrain_predictor,
    label = rgr_xbg_dtrain_response_biosh_gr,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,                # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid_biosh$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid_biosh$min_MERROR[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid_biosh %>%
  dplyr::arrange(min_MERROR) %>%
  head(10)
###################################################################################################################
####################################################################################################
####################################################################################################

# now to set gamma, I will use the paramerters above
set.seed(100)

xgb_biosh_cv <- xgb.cv(
  data = rgr_xbg_dtrain_predictor,
  label = rgr_xbg_dtrain_response_biosh_gr,
  params = list(max_depth = 5, eta = 0.1, min_child_weigh = 7,
                subsample = 0.65, colsample_bytree = 1,
                verbose = 0, gamma = 20,
                objective = "reg:squarederror"),# try changing gamma
  nrounds = 5000,
  verbose = 0,
  nfold = 5,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)


# plot error vs number trees
ggplot(xgb_biosh_cv$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")+
  ylim(0,1500)


xgb_biosh_cv$evaluation_log %>%
  arrange(train_rmse_mean) %>%
  head(10)
# if the training cv is much larger than test, I need to increase gamma, from 0 
# try starting at 10. If train/test cv is super close, then controlled way too much 
# the complexity of xgboost, and the model can't grow trees without pruning them 
# (due to the loss threshold not reached thanks to Gamma)
####################################################################################################
####################################################################################################
####################################################################################################

param_biosh <- list(max_depth = 5, eta = 0.1, min_child_weigh = 7,
                    subsample = 0.65, colsample_bytree = 1,
                    verbose = 0, gamma = 25,
                    objective = "reg:squarederror")

biosh_xbg_model_train <- xgb.train(param_biosh, rgr_xbg_dtrain_biosh_gr,
                                   nrounds = 5000,
                                   early_stopping_rounds = 10,
                                   watchlist = biosh_xbg_watchlist)
#[34]	train-rmse:315.913177	eval-rmse:769.533813

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#___________________________________ GAIN _________________________________________________________#
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# now, we can visialize the relative importance of each of these predictors
# the big question is, do we see agreement amount the response variables?
importance_matrix_bai <- xgb.importance(model = bai_xbg_model_train)
importance_matrix_bioi <- xgb.importance(model = bioi_xbg_model_train)
importance_matrix_biosh <- xgb.importance(model = biosh_xbg_model_train)

# now, let's see if their is argeement in the top 20 predictors 
xgb.plot.importance(importance_matrix_bai, top_n = 20, measure = "Gain")
xgb.plot.importance(importance_matrix_bioi, top_n = 20, measure = "Gain")
xgb.plot.importance(importance_matrix_biosh, top_n = 20, measure = "Gain")

source("scripts/archive/1. functions.R")
Labels <- data.frame(read.csv( "data/labels.csv"))

importance_matrix_bai_merged <- merge(importance_matrix_bai,Labels, by ="Feature")
importance_matrix_bai_merged <- importance_matrix_bai_merged[order(importance_matrix_bai_merged$Gain, decreasing = TRUE),]
importance_matrix_bioi_merged <- merge(importance_matrix_bioi,Labels, by ="Feature")
importance_matrix_bioi_merged <- importance_matrix_bioi_merged[order(importance_matrix_bioi_merged$Gain, decreasing = TRUE),]
importance_matrix_biosh_merged <- merge(importance_matrix_biosh,Labels, by ="Feature")
importance_matrix_biosh_merged <- importance_matrix_biosh_merged[order(importance_matrix_biosh_merged$Gain, decreasing = TRUE),]
#######################################################################################################
#######################################################################################################
gain_colors <- inferno(3)
# Plotting
Plot.XGB.Gain.BAI <- 
  ggplot(importance_matrix_bai_merged[1:10,], aes(x = reorder(stringr::str_wrap(Label,20), Gain), y = Gain))+
  geom_bar(stat="identity", width = 0.75, fill = gain_colors[1], color = gain_colors[1])+
  labs(title = "Basal Area Increment",
       subtitle  = LETTERS[1])+ #title
  xlab(" ") +
  ylab("Gain\n")+
  ylim(0, 0.39)+
  coord_flip()+
  theme_special()

Plot.XGB.Gain.BIOI <- 
  ggplot(importance_matrix_bioi_merged[1:10,], aes(x = reorder(stringr::str_wrap(Label,20), Gain), y = Gain))+
  geom_bar(stat="identity", width = 0.75, fill = gain_colors[2], color = gain_colors[2])+
  labs(title = "Biomass Increment",
       subtitle  = LETTERS[2])+ #title
  xlab(" ") +
  ylab("Gain\n")+
  ylim(0, 0.39)+
  coord_flip()+
  theme_special()

Plot.XGB.Gain.BIOSH <- 
  ggplot(importance_matrix_biosh_merged[1:10,], aes(x = reorder(stringr::str_wrap(Label,20), Gain), y = Gain))+
  geom_bar(stat="identity", width = 0.75, fill = gain_colors[3], color = gain_colors[3])+
  labs(title = "Biomass Scaled to Height",
       subtitle  = LETTERS[3])+ #title
  xlab(" ") +
  ylab("Gain\n")+
  ylim(0, 0.39)+
  coord_flip()+
  theme_special()


##########################################################################################################
png("scripts/figures/gain_xgboost.png", 
    width = 19 , height = 16, units = 'in', res = 600)
grid.arrange(grobs = list(Plot.XGB.Gain.BAI, Plot.XGB.Gain.BIOI, Plot.XGB.Gain.BIOSH),
             nrow = 3)
dev.off()
