######################################################################
#
# information-theorical characterisations of one of several partitions
# and similarities between partitions 
#
######################################################################

#' @title Entropy of a partition
#'

entropyOfPartition <- function(partition){
  populations=table(partition)
  probability.distribution=populations/sum(populations)
  return(entropy(probability.distribution))
}

#' @title Mutual information between two partitions
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
#' @keywords Generate
#' @export 
#' 

MutualInformationTwoPartitions <-function(partition1,partition2){
  
  eps = 0.001 # To avoid numerical problems 
  
  # Contigency table between the two partitions
  t <- table(partition1,partition2)
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

normalizedMutualInformation <- function(partition1,partition2){
  normalized.mutual.information <- MutualInformationTwoPartitions(partition1,partition2)/sqrt(entropyOfPartition(partition1)*entropyOfPartition(partition2))
  return(normalized.mutual.information)
}

#' @title variationOfInformation
#' A similarity between partitions that has.... advantage 

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


