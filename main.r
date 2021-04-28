library(data.table)
library(caret)
library(mlbench)
library(randomForest)
library(readxl)

gene<-read_excel("gene.xlsx", col_names = FALSE, 
                 skip = 1)

gene<-transpose(gene)
names(gene)<-gene[1,]
gene<-gene[-1,]
row.names(gene) <- NULL
gene[] <- lapply(gene, function(x) if(is.character(x)) as.numeric(x) else x)
gene$group<-factor(gene$group)

set.seed(1234)


# trim_rf()
# Drops least important variables in successive random forest models using backwards elimination with OOB error as comparison metric. 

# x: predictor columns
# y: response column
# drops: number of variables dropped per iteration

# returns optimal # of predictors (must sort all predictors by importance in base model)


trimRF <- function(x_data,
                   y_data,
                   drops)
{
  initial_rf<-randomForest(x_data,
                           y_data,
                           importance=TRUE)

  importances<-importance(initial_rf, type = 1)
  vars<-order(importances, decreasing = TRUE)

  i<-length(vars)
  j<-1
  best.oob<-1
  best.rf<-NULL
  oobs<-vector("list",length(vars))
  num_pred<-vector("list",length(vars))
  best.num<-0

  while((i-drops)>=1)
  {
    print(i)
    rf<-randomForest(x_data[vars[1:i]],
                     y_data)
    num_pred[j]<-i
    oobs[j]<-rf$err.rate[rf$ntree]
    print(rf$err.rate[rf$ntree])

    if(oobs[j]<best.oob)
    {
      best.num<-i
    }


    j<-j+1
    i<-i-drops
  }
  print("Optimal #:")
  print(best.num)
  
  num_pred<-num_pred[lengths(num_pred)!=0]
  oobs<-oobs[lengths(oobs)!=0]
  
  out <- do.call(rbind.data.frame, Map('c', num_pred, oobs))
  colnames(out)[1] <- "num_pred"
  colnames(out)[2] <- "oobs"
  
  return(out)
}


gene_x<-gene[,colnames(gene)!="group"]
gene_y<-gene$group



# Dataset (4 category):
# 1 - Control
# 2 - Non-drinkers
# 3 - Early drinkers
# 4 - Late drinkers


selection_1<-trimRF(gene_x,gene_y,500)
plot(selection_1$num_pred,
     selection_1$oobs,
     xlab="Number of Predictors",
     ylab="OOB Error",
     main="Predictor Count v. OOB Error (4 Category Dataset)")

full_rf1<-randomForest(gene_x,
                      gene_y,
                      importance=TRUE)

importances1<-importance(full_rf1, type = 1)
vars1<-order(importances1, decreasing = TRUE)
optimal_rf1<-randomForest(gene_x[vars1[1:674]],
                          gene_y)

# Call:
#   randomForest(x = gene_x[vars1[1:674]], y = gene_y) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 25
# 
# OOB estimate of  error rate: 41.67%
# Confusion matrix:
#   1 2 3 4 class.error
# 1 2 1 0 0   0.3333333
# 2 0 1 1 1   0.6666667
# 3 0 1 1 1   0.6666667
# 4 0 0 0 3   0.0000000


# Dataset (2 category):
# 1 - Control and non-drinkers
# 2 - Early drinkers and late drinkers

gene2<-gene
gene2$group<-gene2$group
gene2$group[gene2$group=="2"]<-"1"
gene2$group[gene2$group=="3"]<-"2"
gene2$group[gene2$group=="4"]<-"2"
levels(gene2$group)


gene2_x<-gene2[,colnames(gene)!="group"]
gene2_y<-gene2$group


selection_2<-trimRF(gene2_x,gene2_y,500)
plot(selection_2$num_pred,
     selection_2$oobs,
     xlab="Number of Predictors",
     ylab="OOB Error",
     main="Predictor Count v. OOB Error (2 Category Dataset)")

full_rf2<-randomForest(gene2_x,
                       gene2_y,
                       importance=TRUE)

importances2<-importance(full_rf2, type = 1)
vars2<-order(importances2, decreasing = TRUE)
optimal_rf2<-randomForest(gene2_x[vars2[1:674]],
                          gene2_y)

# Call:
#   randomForest(x = gene2_x[vars2[1:674]], y = gene2_y) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 25
# 
# OOB estimate of  error rate: 8.33%
# Confusion matrix:
#   1 2 class.error
# 1 5 1   0.1666667
# 2 0 6   0.0000000