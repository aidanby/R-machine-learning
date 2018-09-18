library(plyr)
library(arules)
library(arulesViz)
library(R.utils)
library(data.table)
repos1 = readLines("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\codeCSV_commitsWithFeature_names.txt")
#repos1 = readLines("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\code-toR.txt")
fileNames <- c()


for(i in repos1){
  tryCatch({
    i = gsub("/", "-", i)
    df <- read.csv(paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\codeCSV_commitsWithFeature_clean\\", i, sep = ""),".csv", sep = ""))
    #df <- read.csv(paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\codeCSV\\", i, sep = ""),".csv", sep = ""))
    
    
    
    
    df_basket <- ddply(df,c("commitID"), 
                       function(df1)paste(df1$modifications, 
                                          collapse = ","))
    
    df_basket$commitID <- NULL
    colnames(df_basket) <- c("code line")
    
    if(nrow(df_basket) > 2){
      write.csv(df_basket, paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\code-pivotedCSV-commitsWithFeature\\",i, sep = ""),".csv", sep = ""), quote = FALSE, row.names = TRUE)
      fileNames <- c(fileNames, i)
    }
  }, error = function(e){
    
    print(e)
  })
  
}

write(fileNames, "C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\code-toR-clean.txt")
repos2 = readLines("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\code-toR-clean.txt")

for(i in repos2){
  i = gsub("/", "-", i)
  txn = read.transactions(file= paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\code-pivotedCSV\\",i, sep = ""),".csv", sep = ""), rm.duplicates= TRUE, format="basket",sep=",",cols=1);
  txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
  res <- NULL;
  isGood <- TRUE;
  
  tryCatch({
    res <- withTimeout({
      basket_rules <- apriori(txn,parameter = list(sup = 0.1, conf = 0.5,target="rules"));
      df_rules <- as(basket_rules,"data.frame")
      if(nrow(df_rules) > 0){
        write.csv(df_rules, file = paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\code-rules\\",i, sep = ""),"-code-rules.csv", sep = ""))
      }
    }, timeout=1);
  }, TimeoutException=function(ex) {
    cat("skipping\n");
    isGood <- FALSE;
  })
  if(isGood == TRUE){
    fileNames <- c(fileNames, i)
  }
}

write(fileNames, "C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\code-ruleNames.txt")
repos3 = readLines("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\code-ruleNames.txt")

for(i in repos3){
  tryCatch({
    Subset <- read.csv(paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\code-rules\\",i, sep = ""),"-code-rules.csv", sep = ""))

    
    Subset$repository <- c(Subset$repository, i)
    
    #Subset <- subset(Subset, select = -c(X))
    

    write.csv(Subset, file = paste(paste("C:\\Users\\happyuser\\Desktop\\Github_Scraper\\1000000\\code-rules\\",i, sep = ""),"-code-rules.csv", sep = ""))
  }, error = function(e){
    
    #show(e)
  })
}
