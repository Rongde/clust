\name{partition.set}
\alias{partition.set}
\title{Create a partition set from a data frame which columns are vector of class labels}
\usage{
partition.set(labels)
}
\arguments{
  \item{labels}{a label per data point that identifies the
  cluster it belongs to}
}
\description{
Create a partition set from a data frame which columns are
vector of class labels
}
\examples{
clustering1 <- c(1,1,2)
clustering2 <- c(1,2,2)
df <- as.data.frame(cbind(clustering1,clustering2))
some.partitions.df <- partition.set(df) #
}
\author{
M.GELGON
}

