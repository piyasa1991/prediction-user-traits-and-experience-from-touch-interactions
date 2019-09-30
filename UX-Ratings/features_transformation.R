"This script is used to transform the features into normal distribution
It also test the new features using normality test based on Kolmogorov-Smirnov test
"

#import libaries
library(bestNormalize)
library(ggplot2)
library(moments)


perform_transformation <- function(df_features,normalityTestFile,transformedFile){
  
  # select only numeric columns
  nums <- unlist(lapply(df_features, is.numeric))
   
  #initialize matrix to store the results
  result<- matrix(ncol=6,nrow=length(names(df_features[,nums])))
  colnames(result)<- c("Features","Skewness","Kurtosis","Pearson P /df","p-value","Normality")
  
  # create seperate dataframe 
  df_features_norm <- df_features
  
  drops <- c('user_id',
             'sessionNr','App','Size','UserId', 'Session', 
             'IconSize','Cond')
  df_features <- df_features[, !(names(df_features) %in% drops)]
  
  # 0 variance columns should not be transformed
  df_features<- df_features[ ,apply(df_features, 2, var) != 0]
  rownum=1
  
  
  
  
  for(i in names(df_features[,nums])){
    print(i)
    if (i!="id" && i!="User" && i!="user_id" && i!="appCode" && i!="sessionNr"
        && i!='UserId' && i!='Session' &&
        i!='PQ' && i!='ATT' && i!='HQ'&& i!="HQI" && i!="HQS" && i!="HQ" && i!='SEA'
        ){
      
      # mypath=file.path(getwd(),"UX-Ratings","Figures","Transformation","Features",paste(i,".png",sep=""))
      # png(mypath)
      par(mfrow = c(3,2))
      
      #original distributions
      MASS::truehist(as.vector(df_features[[i]]), main = "Original Distribution", nbins = 24, xlab=i)
      skew <- skewness(df_features[[i]])
      kurt <- kurtosis(df_features[[i]])
      sumStr <- paste('skew',format(skew,digits = 3),";","kurtosis",format(kurt,digits = 3))
      op <- par(mar = c(7,4,4,2) + 0.1)
      title(sub=sumStr,line=6)
      par(op)
      
      #arcsinh transformation
      (arcsinh_obj <- arcsinh_x(as.vector(df_features[[i]])))
      MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 24,xlab=i)
      skew <- skewness(arcsinh_obj$x.t)
      kurt <- kurtosis(arcsinh_obj$x.t)
      sumStr <- paste('skew',format(skew,digits = 3),";","kurtosis",format(kurt,digits = 3))
      
      op <- par(mar = c(7,4,4,2) + 0.1)
      title(sub=sumStr,line=6)
      par(op)
      
      #Box Cox transformation
      if(sum(as.vector(df_features[[i]])<=0)==0){
        (boxcox_obj <- boxcox(as.vector(df_features[[i]])))
        MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 24,xlab=i)
        skew <- skewness(boxcox_obj$x.t)
        kurt <- kurtosis(boxcox_obj$x.t)
        sumStr <- paste('skew',format(skew,digits = 3),";","kurtosis",format(kurt,digits = 3))
        
        op <- par(mar = c(7,4,4,2) + 0.1)
        title(sub=sumStr,line=6)
        par(op)
      }
      
      #Yeo-Johnson transformation
      (yeojohnson_obj <- yeojohnson(as.vector(df_features[[i]])))
      MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 24,xlab=i)
      skew <- skewness(yeojohnson_obj$x.t)
      kurt <- kurtosis(yeojohnson_obj$x.t)
      sumStr <- paste('skew',format(skew,digits = 3),";","kurtosis",format(kurt,digits = 3))
      
      op <- par(mar = c(7,4,4,2) + 0.1)
      title(sub=sumStr,line=6)
      par(op)
      
      #orderNorm transformation
      (orderNorm_obj <- orderNorm(as.vector(df_features[[i]])))
      MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 24, xlab=i)
      skew <- skewness(orderNorm_obj$x.t)
      kurt <- kurtosis(orderNorm_obj$x.t)
      sumStr <- paste('skewness=',format(skew,digits = 3),";","kurtosis=",format(kurt,digits = 3))
      
      op <- par(mar = c(7,4,4,2) + 0.1)
      title(sub=sumStr,line=6)
      par(op)
      
      # automatic selection
      (BNobject <- bestNormalize(standardize = TRUE,as.vector(df_features[[i]])))
      MASS::truehist(BNobject$x.t, 
                     main = paste("Best Transformation:", 
                                  class(BNobject$chosen_transform)[1]), nbins = 24,xlab=i)
      skew <- skewness(BNobject$x.t)
      kurt <- kurtosis(BNobject$x.t)
      stats <- ks.test(x=as.vector(BNobject$x.t),y='pnorm',alternative='two.sided')
      name_of_chosen_transform <- class(BNobject$chosen_transform)[1]
      pearsonP_df<-BNobject$norm_stats[[name_of_chosen_transform]]
      sumStr <- paste('skew',format(skew,digits = 3),";","kurtosis",format(kurt,digits = 3),
                      ";","norm stats=",format(BNobject$chosen_transform$norm_stat,digits = 3),
                      ";","p-value=",format(stats$p.value,digits=3))
      
      op <- par(mar = c(7,4,4,2) + 0.1)
      title(sub=sumStr,line=6)
      par(op)
      # par(mfrow=c(1,1))
      dev.off()
      
      print("******************Generating Report************************")
      print('bestNormalize stastics')
      print(pearsonP_df)
      print("performing normality test")
      
      #Kolmogorov-Smirnov test
      
      print(ks.test(x=as.vector(BNobject$x.t),y='pnorm',alternative='two.sided'))
      
      
      paste('skewness=',skew,";","kurtosis=",kurt)
      
      if(pearsonP_df<2 && stats$p.value>0.05){
        cat("updating",i,"..\n")
        df_features_norm[[i]]=BNobject$x.t
      }
      normality<- stats$p.value>0.05
      result[rownum,]<- c(i,round(skew,digits = 3),
                          round(kurt,digits=3),round(pearsonP_df,digits=3),
                          round(stats$p.value,digits=3),normality)
      rownum=rownum+1
      
    }
  }
  write.csv(result,file=normalityTestFile,sep=",") 
  write.csv(df_features_norm,file=transformedFile,sep=",",row.names = FALSE)
  
}


## Performing Mahanalobis distance

## study1 dataset

#option 1
# df_features <- read.csv(file="datasets/files_generated/UX/Transformation/study1_features_data_out_mahalanobis_descriptorsTransformedDistributions.csv", header=TRUE, sep=",")
# df_features<-df_features[df_features$App=="Spell",]
# normalityTestFile="UX-Ratings/tables/univariate_normality_test_features_mahanalobis_descriptors_transformed.csv"

# option 2
df_features <- read.csv(file="datasets/files_generated/UX/study1_features_data_out_mahalanobis.csv", header=TRUE, sep=",")
df_features<-df_features[df_features$App=="Spell",]
normalityTestFile <- "UX-Ratings/Tables/NormalityCheck/study1_univariate_normality_test_features_mahalanobis_transformed.csv"
transformedFile <-"datasets/files_generated/UX/study1_features_data_out_mahalanobis_transformedDistributions.csv"
perform_transformation(df_features,normalityTestFile,transformedFile)




### study2 dataset
df_features <- read.csv(file="datasets/files_generated/UX/study2_features_data_out_mahalanobis.csv", header=TRUE, sep=",")
df_features<-df_features[df_features$App=="Spell",]
normalityTestFile <- "UX-Ratings/Tables/NormalityCheck/study2_univariate_normality_test_features_mahalanobis_transformed.csv"
transformedFile <-"datasets/files_generated/UX/study2_features_data_out_mahalanobis_transformedDistributions.csv"
perform_transformation(df_features,normalityTestFile,transformedFile)



## combined dataset i.e. study1 and study2
df_features<- read.csv(file="datasets/files_generated/UX/combined_features_data_out_mahalanobis.csv", header=TRUE, sep=",")
df_features<-df_features[df_features$App=="Spell",]
normalityTestFile <- "UX-Ratings/Tables/NormalityCheck/combined_univariate_normality_test_features_mahalanobis_transformed.csv"
transformedFile <-"datasets/files_generated/UX/combined_features_data_out_mahalanobis_transformedDistributions.csv"
perform_transformation(df_features,normalityTestFile,transformedFile)

