######################################################################################################
########################################### METRIC REDUCTION  ########################################
######################################################################################################
######################################################################################################
######################################################################################################
library(vegan)
library(readxl)
library(corrplot)
library(RColorBrewer)
library(data.table)
library(vegan)
library(dplyr)
######################################################################################################
######################################################################################################


# Which metrics should we remove? within each metric category, we want to keep correlations below 0.7 # 
# import data#
rgr.msh.raw <- data.frame(read.csv( "data/RGR_MSH.csv"))[,-1]
rownames(rgr.msh.raw) <- seq(1,3000,5)[1:nrow(rgr.msh.raw)]
######################################################################################################
######################################################################################################
# prepare data for correlation analyses
MB <- rgr.msh.raw[,3:100]
# change NAs to zeros
MB[is.na(MB)] <- 0
# save correlation results
M <- cor(MB)

MTest <- M
corcut <- c(0.8, -0.8)
MTest[MTest>= corcut[1]] <- "_"
# not much of a concern with multicolineaity
######################################################################################################
######################################################################################################
# removing data with NAs
missing.response <- which(is.na(rgr.msh.raw$BIOSH_GR))
rgr.msh <- rgr.msh.raw[-missing.response, ]
missing.predictors <- unname(which((apply(rgr.msh, 2, anyNA))))
rgr.msh <- rgr.msh[,-missing.predictors]
rgr.msh <- data.frame(rgr.msh[,3:72])
removecolumns <- c("Tree.Height", "Tree.Age", "Tree.Height_imp", "Root.Wood.Density_imp",
                   "Biomass5_imp", "pcent.max.Bio5_imp")
remove_columns <- which(colnames(rgr.msh)%in%removecolumns)
######################################################################################################
######################################################################################################
# randomly select 4000 wetlands per permenance class
set.seed(145)
train.test <- as.integer(as.character(sample(rownames(rgr.msh), round(nrow(rgr.msh)*0.7))))
train.test<- which(rownames(rgr.msh)%in%train.test)
rgr.msh.train <- rgr.msh[train.test,-remove_columns]
rgr.msh.test <- rgr.msh[-train.test,-remove_columns]

write.csv(rgr.msh.train,"data/rgr_msh_train.csv")
write.csv(rgr.msh.test,"data/rgr_msh_test.csv")

