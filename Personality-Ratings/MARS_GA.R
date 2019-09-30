# if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
library(ggplot2)
library(lattice)
# if(!require(caret)) install.packages("caret",repos = "http://cran.us.r-project.org")
# library(ggplot2)
library(caret)
library(doParallel) # parallel processing
library(dplyr) # Used by caret
# library(earth)

# setwd("/mnt/vdb1/")
run_Mars <- function(filename,result_file_name,prediction_file_name,n_components=0, perform_pca=FALSE,transformation=FALSE){
  #load csv
  cat("reading file from: ",filename)
  
  df_features <- read.csv(filename)
  
  drops <- c('user_id','VP','age',
             'sessionNr','App','Size','UserId', 'Session', 'Handedness_Score','Age','X')
  df_features <- df_features[, !(names(df_features) %in% drops)]
  
  # 0 variance columns should not be transformed
  df_features<- df_features[ ,apply(df_features, 2, var) != 0]
  
  
  df_results <- read.csv(result_file_name,check.names = FALSE)
  
  # df_features<-df_features[df_features$App=="Spell",]
  nums <- unlist(lapply(df_features, is.numeric))
  df_features <- df_features[,nums]
  targets =c('Neuroticism', 'Extraversion', 
             'Openness', 'Agreeableness','Conscientiousness')
  
  if (transformation==TRUE){
    normality_test_features_path ='/mnt/vdb1/Personality-Ratings/NormalityCheck/combined_univariate_normality_test_features_mahalanobis_transformed.csv'
    mahalanobis <- read.csv(normality_test_features_path)
    mahalanobis <- mahalanobis %>% na.omit()
    columns <-  as.vector(mahalanobis[mahalanobis$Normality==TRUE,'Features'])
  }
  
  for (i in 1:length(targets)){
    cat("\nPerforming predictions on: ",targets[i])
    
    
    # drops <- c('user_id','VP','age',
    #            'sessionNr','App','Size','UserId', 'Session', 'Handedness_Score','Age',
    #            'Neuroticism', 'Extraversion', 
    #            'Openness', 'Agreeableness','Conscientiousness')
    # df_features_neuro <-df_features[ , !(names(df_features) %in% drops)]
    df_features_temp <- df_features
    if(transformation==TRUE){
      df_features_temp <- df_features_temp[, (names(df_features_temp) %in% columns)]
      # df_features_temp[,targets[i]] <- df_features[,targets[i]]
    }
    else{
      drops <- c('user_id','VP','age',
                 'sessionNr','App','Size','UserId', 'Session', 'Handedness_Score','Age',
                 'Neuroticism', 'Extraversion',
                 'Openness', 'Agreeableness','Conscientiousness','X')
      df_features_temp <- df_features_temp[, !(names(df_features_temp) %in% drops)]
      # df_features_temp[,targets[i]] <- df_features[,targets[i]]
    }
      
    # remove highly correlated features
    cor_matrix <- cor(df_features_temp)
    cor_matrix[!lower.tri(cor_matrix)] <- 0
    
    dim(df_features_temp[,!apply(cor_matrix,2,function(x) any(x >= 0.80))])
    
    df_features_temp <- df_features_temp[,!apply(cor_matrix,2,function(x) any(x >= 0.80))]
    
    df_features_temp[,targets[i]] <- df_features[,targets[i]]
    
    X <- df_features_temp[,-dim(df_features_temp)[2]]
    cat("Shape of the data: ",dim(X))
    y<- df_features_temp[,targets[i]]
    # val_index <- createDataPartition(df_features_pq$PQ, p=0.70, list=FALSE)
    # testData <- df_features_pq[-val_index,]
    # trainData <- df_features_pq[val_index,]
    # X_train <- trainData[,-dim(testData)[2]]
    # y_train <- trainData$PQ
    # X_test <- testData[,-dim(testData)[2]]
    # y_test <- testData$PQ
    resultObj = perform_evaluation(X,y,NULL,NULL,results_file_name,
                                n_components=n_components,perform_pca=perform_pca)
    
    result <- resultObj[c(-length(resultObj),-(length(resultObj)-1),-(length(resultObj)-2))]
    predictions <- resultObj[c(length(resultObj)-2,length(resultObj)-1,length(resultObj))]
    
    result<- data.frame(result,check.names = FALSE)
    result$Target <- targets[i]
    result$Algorithm <- 'MARS'
    
    # final_df[,1:13] <- pq[1,]
    # final_df[,'Target']<- 'PQ'
    # final_df[,'Algorithm']<-'MARS'
    
    predictions<- data.frame(predictions,check.names = FALSE)
    
    cbind.fill <- function(...){
      nm <- list(...) 
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow)) 
      do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))) 
    }
    if(i==1){
      
      residual_table <- predictions
    }
    else{
      residual_table <- data.frame(cbind.fill(residual_table,predictions))
    }
    
    names(residual_table)[names(residual_table) == "Original"] <- paste('Original_',targets[i],sep="")
    names(residual_table)[names(residual_table) == "Prediction"] <- paste('Prediction_',targets[i],sep="")
    names(residual_table)[names(residual_table) == "Residuals"] <- paste('Residuals_',targets[i],sep="")
    
    # TODO: append the results to the appropiate file
    if(file.exists(result_file_name)){
      df_results <- rbind(df_results,result)
      
      write.csv(df_results,file=result_file_name,sep=",",row.names = FALSE)
     
    }
    else{
      write.csv(df_results,file=result_file_name,sep=",",row.names = FALSE)
    }
    
    
  }
  
  library("xlsx")
  if(file.exists(prediction_file_name)){
    cat("Trying to write the residuals in file")
    write.xlsx(residual_table, prediction_file_name, sheetName="MARS", 
               row.names=FALSE,append=TRUE)
    cat("\nFile saved successfully")
  }
}



perform_evaluation <- function(X_train,y_train,X_test,y_test,results_file_name,n_components,perform_pca=FALSE){
  result<- data.frame(matrix(ncol=11,nrow=1))
  # colnames(result)<- c("#Features","Adjusted R2(Validation)","Features","R2(Test)","R2(Train)","R2(Validation)",
  #                      "RMSE(Test)","RMSE(Train)","RMSE(Validation)","StandardError(Validation)",
  #                      "MAE(Test)","MAE(Train)","MAE(Validation)",'AIC(Validation)','AIC(Test)','BIC(Validation)','BIC(Test)',
  #                      'MAPE(Validation)','MAPE(Test)')
  
  colnames(result)<- c("#Features","Adjusted R2(Validation)","Features",
                       # "R2(Test)",
                       "R2(Train)","R2(Validation)",
                       # "RMSE(Test)",
                       "RMSE(Train)","RMSE(Validation)","StandardError(Validation)",
                       # "MAE(Test)",
                       "MAE(Train)","MAE(Validation)",
                       'MAPE(Validation)'
                       # 'MAPE(Test)'
  )
  if(perform_pca==TRUE){
    cat("\n Number of principal components:",n_components)
    # scale the data and apply pca
    #X_train<- X_train[ , apply(X_train, 2, var) != 0]
    set.seed(235)
    if(n_components!=3){
      
      p <- preProcess(X_train, method = "pca", thresh = n_components)
      cat("\n Number of principal components got :",p$numComp)
      study1.pca <- prcomp(X_train, center = TRUE,scale. = TRUE, rank.=p$numComp)
    }
    else {
      study1.pca <- prcomp(X_train, center = TRUE,scale. = TRUE, rank.=n_components)
    }
    
    s<-summary(study1.pca)
    #library(devtools)
    # install_github("vqv/ggbiplot",force = TRUE)
    #library(ggbiplot)
    
    #ggbiplot(study1.pca)
    X_train<- study1.pca$x 
    # X_test <- predict(study1.pca, newdata=X_test)
  }
  # apply GA to find best features/PCs
  mars_ga_ctrl <- gafsControl(functions = rfGA, method = "cv", number=5,
                              #genParallel=TRUE, # Use parallel programming
                              #allowParallel = TRUE,
                              verbose=TRUE
  )
  
  set.seed(100)
  mars_ga_search <- gafs(x = X_train, y = y_train,
                         iters = 2, # 100 generations of algorithm
                         popSize = 20, # population size for each generation
                         #levels = lev,
                         gafsControl = mars_ga_ctrl,
                         # now options to `train` for caretGA
                         method='earth',
                         trControl = trainControl(method = "cv", allowParallel = TRUE,number=5))
  summary(mars_ga_search)
  
  plot(mars_ga_search) + theme_bw()
  
  final <- mars_ga_search$ga$final # Get features selected by GA
  X2_train <- X_train[,final] # training data: selected features
  # X2_test <- X_test[,final]
  
  
  # #Train the model
  #create a tuning grid
  param_grid <- expand.grid(
    degree = 1:3, 
    nprune = seq(5, 25, length.out = 10) %>% floor()
  )
  set.seed(253)
  mars.tune <- train(
    x = X_train,
    y = y_train,
    method = "earth",
    metric = "Rsquared",
    trControl = trainControl(method = "cv", number = 10),
    tuneGrid = param_grid
  )
  
  # best model
  mars.tune$bestTune
  summary(mars.tune)
  
  rmse_val <- mean(mars.tune$resample$RMSE)
  r2_val<-mean(mars.tune$resample$Rsquared)
  mae_val<- mean(mars.tune$resample$MAE)
  rmse_train<- getTrainPerf(mars.tune)$TrainRMSE
  r2_train<- getTrainPerf(mars.tune)$TrainRsquared
  mae_train <- getTrainPerf(mars.tune)$TrainMAE
  adjusted_r2<- 1-(1-r2_val)*(length(y_train)-1)/(length(y_train)-length(final)-1) 
  se<- sd(mars.tune$resample$Rsquared)
  cat(se)
  
  library(MLmetrics)
  # calculate AIC/BIC/MAPE
  y_pred_train <- predict(mars.tune$finalModel,X_train)
  mape_val <- MAPE(y_train,y_pred_train)*100
  
  # plot results
  ggplot(mars.tune)
  
  # predict on unseen data
  # mars.pred <- predict(mars.tune$finalModel,X_test)
  # mars.pred.summary<- postResample(pred = mars.pred,obs=y_test)
  # rmse_test<- mars.pred.summary[1]
  # r2_test<- mars.pred.summary[2]
  # mae_test<- mars.pred.summary[3]
  # 
  # # calculate AIC/BIC/MAPE
  # mape_test <- MAPE(y_test,mars.pred)*100
  
  # calculate residuals
  residual = y_train - y_pred_train
  colnames(residual)<- NULL
  
  # result[1,]<- c(length(final),adjusted_r2,paste(final,collapse=","),
  #                # r2_test,
  #                r2_train,r2_val,
  #                # rmse_test,
  #                rmse_train,rmse_val,
  #                NA, #standard error
  #                # mae_test,
  #                mae_train,mae_val,
  #                # NA,NA,NA,NA,
  #                mape_val
  #                # mape_test
  # )
  result$`#Features`<- length(final)
  result$`Adjusted R2(Validation)` <- adjusted_r2
  result$Features <- paste(final,collapse=",")
  # result$`R2(Test)`<- r2_test
  result$`R2(Train)`<-r2_train
  result$`R2(Validation)`<- r2_val
  # result$`RMSE(Test)`<- rmse_test
  result$`RMSE(Train)`<- rmse_train
  result$`RMSE(Validation)`<- rmse_val
  result$`StandardError(Validation)`<- se
  # result$`MAE(Test)`<- mae_test
  result$`MAE(Train)`<-mae_train
  result$`MAE(Validation)`<- mae_val
  # result$`AIC(Validation)`<- NA
  # result$`AIC(Test)`<- NA
  # result$`BIC(Validation)`<-NA
  # result$`BIC(Test)`<- NA
  result$`MAPE(Validation)`<- mape_val
  # result$`MAPE(Test)`<- mape_test
  
  predictions_table <- data.frame(matrix(nrow = dim(X_train)[1],ncol=3))
  colnames(predictions_table)<- c('Original','Prediction','Residuals')
  predictions_table$Original<- y_train
  
  predictions_table$Prediction <- y_pred_train
  predictions_table$Residuals <- residual
  
  resultList <- c(result,predictions_table)
  
  return (resultList)
}

# evaluate model on original distribution features
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_alltargets.csv"
n_components <-0
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_predictions.xlsx"
run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=FALSE,transformation=FALSE)


# evaluate model on original distribution after PCA with 95% explained variance PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_alltargets_PCA_0.95PC.csv"
n_components <-0.95
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_PCA_0.95_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=FALSE)

# evaluate model on original distribution after PCA with 80% explained variance PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_alltargets_PCA_0.80PC.csv"
n_components <-0.80
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_PCA_0.8_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=FALSE)

# evaluate model on original distribution after PCA with 3 PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_alltargets_PCA_3PC.csv"
n_components <-3
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_PCA_3_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=FALSE)






#evaluate model on transformed distribution
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis_transformedDistributions.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_transformed_alltargets.csv"
n_components <-0
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_transformed_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=FALSE,transformation=TRUE)

# evaluate model on transformed distribution after PCA with 95% explained variance PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis_transformedDistributions.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_transformed_alltargets_PCA_0.95PC.csv"
n_components <-0.95
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_transformed_PCA_0.95_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=TRUE)

# evaluate model on transformed distribution after PCA with 80% explained variance PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis_transformedDistributions.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_transformed_alltargets_PCA_0.80PC.csv"
n_components <-0.80
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_transformed_PCA_0.8_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=TRUE)

# evaluate model on transformed distribution after PCA with 3 PCs
filename <- "/mnt/vdb1/datasets/files_generated/Personality/combined_features_data_out_mahalanobis_transformedDistributions.csv"
result_file_name<-"/mnt/vdb1/Personality-Ratings/Tables/feature_selection_mahalanobis_transformed_alltargets_PCA_3PC.csv"
n_components <-3
prediction_file_name <- "/mnt/vdb1/Personality-Ratings/Residuals/feature_selection_alltargets_mahalanobis_transformed_PCA_3_predictions.xlsx"
#run_Mars(filename,result_file_name,prediction_file_name,n_components, perform_pca=TRUE,transformation=TRUE)





