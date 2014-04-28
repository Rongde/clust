


#' @title Visualize a partition set 
#'
#' @param a.partition.set : a partition set 
#' @author M.GELGON
#' @export
#' @examples 
#' require(RColorBrewer)
#' require(rblocks)
#' require(plyr)
#' a.few.partitions <- generate.partitions(dataset=iris[1:20,-5],algorithm="manyKmeans",number.partitions=10,number.cluster=3)
#' visu.partition.set(a.few.partitions)

# TO DO : 
# display of names for rows/cols
# subsample if too many rows/cols and tell the user
# transpose if highly vertical grid, and inform user
#


visu.partition.set <- function(a.partition.set)
{
  palette <- brewer.pal(4, "BuPu")
  lab <- as.list(palette)
  lev <- 1:length(palette)
 
  w <- make_block(a.partition.set$labels)
  
  
list.of.lists <-lapply(a.partition.set$labels, function(x) 
 {
   palette <- brewer.pal(4, "BuPu")
   lab <- as.list(palette)
   lev <- 1:length(lab)
   mapvalues(x,from=lev,to=lab) 
 }
 )
 
 a.partition.set$colors <- as.data.frame(sapply(list.of.lists,rbind))
 
for (i in 1:ncol(a.partition.set$labels))
  for (j in 1:nrow(a.partition.set$labels))
    w[j,i] <- a.partition.set$colors[j,i]

print(w) #displays the graphical blocks
}
