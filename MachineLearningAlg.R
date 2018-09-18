library(plyr)
library(arules)
library(arulesViz)
library(R.utils)
library(data.table)
repos = readLines("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\temp.txt")

for(i in repos){
  i = gsub("/", "-", i)
  df <- read.csv(paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\fileCSV_toApriori\\", i, sep = ""),".csv", sep = ""))
  df_basket <- ddply(df,c("commitid", "date"), 
                     function(df1)paste(df1$filesUploaded, 
                                        collapse = ","))
  df_basket$commitid <- NULL
  df_basket$date <- NULL
  colnames(df_basket) <- c("files")
  
  if(nrow(df_basket) > 0){
    write.csv(df_basket, paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\fileCSV_pivoted\\",i, sep = ""),".csv", sep = ""), quote = FALSE, row.names = TRUE)
  }
}
for(i in repos){
  i = gsub("/", "-", i)
  txn = read.transactions(file= paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\fileCSV_pivoted_clean\\",i, sep = ""),".csv", sep = ""), rm.duplicates= TRUE, format="basket",sep=",",cols=1);
  txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
  res <- NULL;
  isGood <- TRUE;
  
  tryCatch({
    res <- withTimeout({
      basket_rules <- apriori(txn,parameter = list(sup = 0.1, conf = 0.5,target="rules"));
      df_rules <- as(basket_rules,"data.frame")
      if(nrow(df_rules) > 0){
        write.csv(df_rules, file = paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\file-rules\\",i, sep = ""),"-rules.csv", sep = ""))
      }
    }, timeout=20);
  }, TimeoutException=function(ex) {
    cat("skipping\n");
    isGood <- FALSE;
  })
}

txn = read.transactions(file= "C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\fileCSV_pivoted_clean\\Chorus-bdd-Chorus.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
res <- NULL;
basket_rules <- apriori(txn,parameter = list(sup = 0.02, conf = 0.05,target="rules"));
df_rules <- as(basket_rules,"data.frame")
if(nrow(df_rules) > 0){
  write.csv(df_rules, file = "C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\file-rules\\Chorus-bdd-Chorus-rules.csv", sep = "")
}





#For forest
library(randomForest)
library(ROCR)
library(pvclust)
library(ClustOfVar)
library(rms)
library(rfUtilities)
library(ggplot2)
library(reshape2)
#randomForest
variables <- c("testfiles_added", "stepdef_mod", "stepdef_added", "otherfiles_mod", "otherloc_added", "authorexp", "sourcefiles_mod", "testloc_added", "otherfiles_added", "sourceloc_added", "import_loc_added", "testloc_del", 
               "testfiles_del", "sourceloc_del", "sourcefiles_added", "methods_added", "import_loc_del", "otherfiles_del", "stepdef_loc_added", "sourcefiles_renamed", "methods_deleted",  "stepdef_renamed", "testfiles_renamed",
               "otherfiles_renamed", "stepdef_loc_del", "sourcefiles_del", "stepdef_del")
dat <- data.frame(character(27), numeric(27), stringsAsFactors = FALSE)
for(k in 1 : 27){
  aucList <- c()
  for(j in 1 : 10){
    df <- read.csv("C:\\Users\\happyuser\\Desktop\\to_model.csv")
    df <- (df[,c(3:33)])
    df <- df[, !names(df) %in% c("otherloc_del", "testfiles_mod", "date")]
    df <- df[, !names(df) %in% variables[k]]
    
    data_set_size <- floor(nrow(df)/10)
    indexes <- sample(1:nrow(df), size = data_set_size)
    training <- df[indexes,]
    validation1 <- df[-indexes,]
    rf_classifier = randomForest(bdd ~ ., data=training, ntree=100, mtry = 18, importance=TRUE)
    #show(rf_classifier)
    #varImpPlot(rf_classifier, main = 'Most Important Variables for feature co-change')
    #AUC for testing important variables
    prediction_for_roc_curve <- predict(rf_classifier,validation1[,-27],type="prob")
    pretty_colours <- c("#F8766D","#00BA38")
    classes <- levels(validation1$bdd)
    for (i in 1:2){
      # Define which observations belong to class[i]
      true_values <- ifelse(validation1[,27]==classes[i],1,0)
      # Assess the performance of classifier for class[i]
      pred <- prediction(prediction_for_roc_curve[,i],true_values)
      perf <- performance(pred, "tpr", "fpr")
      # Calculate the AUC and print it to screen
      auc.perf <- performance(pred, measure = "auc")
      #print(auc.perf@y.values)
      auc <- as.numeric(auc.perf@y.values)
    }
    aucList <- c(aucList, auc)
  }
  auc <- mean(aucList)
  difference <- 0.79904 - auc
  show(paste(variables[k],difference, sep = ","))
  dat[k,1] <- variables[k]
  dat[k,2] <- difference

}


df <- read.csv("C:\\Users\\happyuser\\Desktop\\AUC_importance.csv")
high.order <- order(df$AUC_difference)
df_ordered <- df[high.order,]
barplot(df_ordered$AUC_difference, names.arg=df$Variables, las=2, cex.names=0.5, border=NA)



#confusion table to check result
prediction_for_table <- predict(rf_classifier,validation1[,-30])
confusionTable <- table(observed=validation1[,30],predicted=prediction_for_table)
show(confusionTable)
write.csv(confusionTable, file = 'C:\\Users\\happyuser\\Desktop\\confusionMatrix.csv')

#AUC
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-28],type="prob")
pretty_colours <- c("#F8766D","#00BA38")
classes <- levels(validation1$bdd)
for (i in 1:2){
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,28]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
legend(0.7,0.8, legend = c("test 1", "test 2"), col = c("red", "green"), lty=1:1, cex=1.2, text.font = 4)




#correlation analysis

df <- read.csv("C:\\Users\\happyuser\\Desktop\\to_modelling.csv")
summary(df)

df <- (df[,c(3:31)])
df <- as.matrix(df)
redundency <- redun(clust , df)




clust <- varclus(df)
plot(clust, main = 'spearman')
abline(h=0.3, hty=2, col = "red")

clustPears <- varclus(df, similarity = "pearson")
plot(clustPears, main = 'pearson')
abline(h=0.3, hty=2, col = "red")

tree <- hclustvar(X.quanti=df)
plot(tree)

res.pv <- pvclust(df, method.dist="cor", 
                  method.hclust="average", nboot = 10)

plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv, alpha=.2)


#clustering
library(dtwclust)
library(cluster)
library(NbClust)
library(factoextra)
library(TSclust)

df_BDD <- read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\LOC_analysis\\allLOC_BDD_difference_toR.csv")
df_nonBDD    <- read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\LOC_analysis\\allLOC_nonBDD_difference_toR.csv")


gap_stat <- clusGap(df_BDD, kmeans, 20, 44, B = 100, verbose = TRUE)
plot(gap_stat, xlab = "Number of clusters k", main = 'Finding Optimal Number of Clusters')
abline(v = 18, lty = 2)


clust_BDD <- tsclust(df_BDD, type = "fuzzy",k=18,distance="dtw")
plot(clust_BDD)


clust_nonBDD <- tsclust(df_nonBDD, type = "fuzzy",k=18,distance="dtw")
plot(clust_nonBDD)
