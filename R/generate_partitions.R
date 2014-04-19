#' @title Generate partitions of a given data set 
#'
#' @param dataset : the dataset for which partitions 
#' should be created. 
#' @param algorithm : a string, that specifies the technique
#' for generating partitions :
#' "manyKmeans" : each partition is the result of a kmeans with
#' random initialization. The number of partitions is set by the user.
#' "EachFeature" : each partition is computed on each feature 
#' of the dataset. The number of partitions output is the number 
#' of features, so don't specify it when calling the function, use ",,"
#' @return a dataframe containing the partitions, 
#' which columns are labels. These columns are factors
#' @author M.GELGON
#' @keywords Partition
#' @export
#' @examples
#'  x <- data.frame(cbind(c(1,2,2,2),c(1,1,2,2)))
#'  df <- generatePartitions(x,"manyKmeans",3,2)
#

generatePartitions <- function(dataset,algorithm,number.partitions,number.clusters){
  
  #hey this is a comment
  
  if (algorithm=="eachFeature"){
    number.partitions <- ncol(dataset)
    classif <- generatePartitionsEachFeature(dataset,number.partitions,number.clusters)
    return(as.data.frame(lapply(classif,factor)))
  }
  else if (algorithm=="manyKmeans"){ 
    classif <-generatePartitionsManyKmeans(dataset,number.partitions,number.clusters)
    return(as.factor(as.vector(as.matrix(classif))))
  }
  
  print("Unknown algorithm name in generatePartition")
  stop()
}

#' @title Generate partitions of a given data set for each Feature
#'

generatePartitionsEachFeature <- function(dataset,number.partitions,number.clusters){
  classif <- data.frame(matrix(nrow=nrow(dataset), ncol=ncol(dataset)))
  for (k in 1:ncol(dataset)){
    cl <- kmeans(dataset[k],centers=number.clusters)
    classif[k]=cl$cluster
  }  
  return(classif)
}

#' @title Generate partitions of a given data set with many kmeans
#'


generatePartitionsManyKmeans <- function(dataset,number.partitions,number.clusters){
  classif <- data.frame(matrix(nrow=nrow(dataset), ncol=ncol(dataset)))
  for (k in 1:number.partitions){
    cl <- kmeans(dataset,centers=number.clusters)
    classif[k]=cl$cluster
  }  
  return(classif)
}
