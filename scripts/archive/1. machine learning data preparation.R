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
######################################################################################################
######################################################################################################


# Which metrics should we remove? within each metric category, we want to keep correlations below 0.7 # 
# import data#
rgr.msh.raw <- data.frame(read.csv( "data/RGR_MSH.csv"))[,-1]

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
rgr.msh <- data.frame(decostand(rgr.msh[,3:5], method = "max"),rgr.msh[,6:ncol(rgr.msh)])
######################################################################################################
######################################################################################################
# randomly select 4000 wetlands per permenance class
set.seed(145)
rgr.msh.train <- sample_n(rgr.msh, round(nrow(rgr.msh)*0.7))

rgr.msh.test <- rgr.msh[-as.integer(as.character(rownames(rgr.msh.train))),]
