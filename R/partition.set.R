

#' @title Create a partition set from a data frame which columns are vector of class labels
#'
#' @param labels a label per data point that identifies the cluster it belongs to
#' @author M.GELGON
#' @export
#' @examples
#' clustering1 <- c(1,1,2)
#' clustering2 <- c(1,2,2)
#' df <- as.data.frame(cbind(clustering1,clustering2))
#' some.partitions.df <- partition.set(df) # 


# constructor from a dataframe

partition.set <- function(labels) {
  
  a.partition.set <- list()                    # start to build the object with empty list 
  class(a.partition.set) <- "partition.set"
  a.partition.set$labels <- labels             # add the class labels  
  return(a.partition.set)
}




