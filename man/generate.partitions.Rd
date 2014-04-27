\name{generate.partitions}
\alias{generate.partitions}
\title{Generate partitions of a given data set}
\usage{
generate.partitions(dataset, algorithm, number.partitions, number.clusters)
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
generate.partitions(
dataset=iris[1:20,-5],
algorithm="manyKmeans",
number.partitions=10,
number.cluster=3)
}
\author{
M.GELGON
}
\keyword{Partition}

