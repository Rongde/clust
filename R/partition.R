library(entropy)



#' @title Create a partition from a vector of label
#'
#' @param labels 
#' @author M.GELGON
#' @keywords Partition
#' @export
#' @example
#' labels <- c(1,2,2)
#' my.partition <- partition(labels)


# constructor

partition <- function(labels) {
  
  a.partition <- list()                    # start to build the object with empty list 
  class(a.partition) <- "partition"
  a.partition$labels <- labels             # add the class labels  
  return(a.partition)
}


#' @title Entropy of a partition
#' @param partition : input partition. Any labelling scheme will do, hopefully.
#' @return Shannon entropy for the input partition. Not foolproof if partition is empty.
#' @author M.GELGON
#' @export

entropy.of.partition <- function(partition){
              populations=table(partition$labels)
              probability.distribution=populations/sum(populations)
              return(entropy(probability.distribution))
}




