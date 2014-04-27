######################################################################
#
# information-theorical characterisations of one of several partitions
# and similarities between partitions 
#
######################################################################

#' @title Mutual information between two partitions
#'
#' @param partition1 
#' @param partition2
#' @return the mutual information between the input partitions
#' @author M.GELGON
#' @export 
#' 

MutualInformationTwoPartitions <-function(partition1,partition2){
  
  eps = 0.001 # To avoid numerical problems 
  
  # Contigency table between the two partitions (=co-association matrix)
  t <- table(partition1$labels,partition2$labels)
  m <- as.matrix(t)
  m[m==0] <- eps # to avoid problems later on. Should be improved.
  p.x1.x2 <- m/sum(m) # la table de co-occurrence en dimension 2
  inverse.p.x1 <- 1/rowSums(p.x1.x2)
  inverse.p.x2 <- 1/colSums(p.x1.x2)
  log.joint.over.marginals <- log(outer(inverse.p.x1,inverse.p.x2) * p.x1.x2) # this is element-by-element product
  mutual.info <- sum(log.joint.over.marginals*p.x1.x2)
  return(mutual.info)
}

#' @title Normalized Mutual information between two partitions
#' A similarity between partitions that
#' takes values between 0 (independent) and 1 (identical)
#' @param partition1 a first partition 
#' @param partition2 a first partition
#' @return the normalized mutual information
#' @author M.GELGON
#' @export 

normalizedMutualInformation <- function(partition1,partition2){
  normalized.mutual.information <- MutualInformationTwoPartitions(partition1,partition2)/sqrt(entropyOfPartition(partition1)*entropyOfPartition(partition2))
  return(normalized.mutual.information)
}

#' @title variationOfInformation
#' A similarity between partitions that has some forgotten advantage 
#' @param partition1 a first partition 
#' @param partition2 a first partition
#' @return the variation of information between partitions
#' @author M.GELGON
#' @export 

variationOfInformation <- function(partition1,partition2){
  variation.of.information <- entropyOfPartition(partition1) + entropyOfPartition(partition2) - MutualInformationTwoPartitions(partition1,partition2)
  return(variation.of.information)
}

#' @title dissimilarity
#' evaluate dissimilarity of the "class labels vectors" 
# associated to two data points 
# as the number of labels that differ between the two 

dataPointsSimilarity <- function(data1,data2){
  return (length(which(data1!=data2)))  
}


