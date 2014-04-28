
#' @title Create a partition from a \code{vector} of labels
#' @description The \code{partition} class and constructor hold a vector of labels indicating to which cluster each data point belongs. There hardly is any interest in the \code{partition} class if it only stores a vector of labels indicating classes. We hope to improve the internal representation and computations later and provide better access control, as well as a couple more (syntaxic sugar) fields, e.g. number of clusters.
#' @param labels labels that identify the clusters. They will typically be integers, where what matters only is whether two data points have the same label or not. These labels are considered as \code{factors}.
#' @author M.GELGON
#' @export
#' @examples
#' # Lets generate a very simple partition involving 4 data points and 3 clusters
#' my.labels <- c(1,2,2,4)        # create a vector
#' my.partition <- partition(my.labels)    # call the constructor

partition <- function(labels) {
  a.partition <- list()     # start building the object with empty list 
  class(a.partition) <- "partition"
  a.partition$labels <- labels             # add the class labels  
  return(a.partition)
}




