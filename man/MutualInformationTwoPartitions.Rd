\name{MutualInformationTwoPartitions}
\alias{MutualInformationTwoPartitions}
\title{Mutual information between two partitions}
\usage{
MutualInformationTwoPartitions(partition1, partition2)
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
Mutual information between two partitions
}
\author{
M.GELGON
}
\keyword{Generate}

