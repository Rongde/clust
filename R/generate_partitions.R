#' @title Generate partitions of a given data set 
#'
#' @param dataset : the dataset for which partitions 
#' should be created. 
#' @param algorithm : a string, that specifies the technique
#' for generating partitions :R
#' \itemize{
#' \item "manyKmeans" : each partition is the result of a kmeans with
#' random initialization. The number of partitions is set by the user.
#' \item "EachFeature" : each partition is computed on each feature 
#' of the dataset. The number of partitions output is the number 
#' of features, so don't specify it when calling the function, use ",,"
#' }
#' @param number.partitions to be generated
#' @param number.clusters to be created
#' @return a dataframe containing the partitions, 
#' which columns are labels. These columns are factors
#' @author M.GELGON
#' @keywords Partition
#' @export
#' @examples
#' generate.partitions(
#' dataset=iris[1:20,-5],
#' algorithm="manyKmeans",
#' number.partitions=10,
#' number.cluster=3)

generate.partitions <- function(dataset,
                               algorithm,
                               number.partitions=2,
                               number.clusters)
  {
    
  if (algorithm=="eachFeature"){
    number.partitions <- ncol(dataset)
    classif <- generate.partition.for.each.feature(dataset,number.partitions,number.clusters)
    return(partition.set(as.data.frame(lapply(classif,factor))))
  }
  else if (algorithm=="manyKmeans"){ 
    classif <-generate.partitions.many.kmeans(dataset,number.partitions,number.clusters)
    class(classif)
    return(partition.set(classif))
  }
  
  print("Unknown algorithm name in generate.partition")
  stop()
}

#' @title Generate partitions of a given data set for each Feature
#'
#' @param dataset a data frame containing a dataset 
#' @param number.clusters how many clusters
#' @return a set of partitions 

generate.partition.for.each.feature <- function(dataset,number.clusters){
  classif <- data.frame(matrix(nrow=nrow(dataset), ncol=ncol(dataset)))
  for (k in 1:ncol(dataset)){
    cl <- kmeans(dataset[k],centers=number.clusters)
    classif[k]=cl$cluster
  }  
  return(classif)
}

#' @title Generate partitions of a given data set with many kmeans, using several random initializations.
#' @param dataset a data frame containing a dataset 
#' @param number.partitions how many partitions
#' @param number.clusters how many clusters
#' @return a set of partitions 

generate.partitions.many.kmeans <- function(dataset,number.partitions,number.clusters){
  classif <- data.frame(matrix(nrow=nrow(dataset), ncol=ncol(dataset)))
  for (k in 1:number.partitions){
    cl <- kmeans(dataset,centers=number.clusters)
    classif[k]=cl$cluster
  }  
  return(classif)
}
