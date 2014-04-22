\name{generatePartitions}
\alias{generatePartitions}
\title{Generate partitions of a given data set}
\usage{
generatePartitions(dataset, algorithm, number.partitions, number.clusters)
}
\arguments{
  \item{dataset}{: the dataset for which partitions should
  be created.}

  \item{algorithm}{: a string, that specifies the technique
  for generating partitions : "manyKmeans" : each partition
  is the result of a kmeans with random initialization. The
  number of partitions is set by the user. "EachFeature" :
  each partition is computed on each feature of the
  dataset. The number of partitions output is the number of
  features, so don't specify it when calling the function,
  use ",,"}
}
\value{
a dataframe containing the partitions, which columns are
labels. These columns are factors
}
\description{
Generate partitions of a given data set
}
\examples{
x <- data.frame(cbind(c(1,2,2,2),c(1,1,2,2)))
 df <- generatePartitions(x,"manyKmeans",3,2)
}
\author{
M.GELGON
}
\keyword{Partition}

