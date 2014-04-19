#' Compute the pair-wise similarity matrix between all lines of a partition matrix
#' takes advantage of existing "daisy" function in the "cluster" package. 
#' where "gower" distance is "number of common labels"
#' @export 

dataPairWiseSimilarityMatrix <- function(partitions){
  return(daisy(partitions,metric="gower"))
}
