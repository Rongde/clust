library(cluster)

#' @title Compute the pair-wise similarity matrix between all lines of a partition matrix
#' @param partitions two partitions, what format ?
#' @return a similarity matrix, no clue
#' @author M.GELGON
#' @export 

dataPairWiseSimilarityMatrix <- function(partitions){
  return(daisy(partitions,metric="gower"))
}
