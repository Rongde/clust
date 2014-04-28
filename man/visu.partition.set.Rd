\name{visu.partition.set}
\alias{visu.partition.set}
\title{Visualize a partition set}
\usage{
visu.partition.set(a.partition.set)
}
\arguments{
  \item{a.partition.set}{: a partition set}
}
\description{
Visualize a partition set
}
\examples{
require(RColorBrewer)
require(rblocks)
require(plyr)
a.few.partitions <- generate.partitions(iris[1:20,-5],"manyKmeans",10,3)
visu.partition.set(a.few.partitions)
}
\author{
M.GELGON
}

