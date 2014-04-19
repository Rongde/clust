 

# load data set
data(iris)

dataset <- iris
true_classif <- as.factor(as.matrix(dataset[ncol(dataset)])) # 
partitions <- generatePartitions(dataset[-ncol(dataset)],"eachFeature",number.partition=10,number.clusters=5) #-ncol : so that it doesnt send the truth classif label which is on last column

simil.matrix <- dataPairWiseSimilarityMatrix(partitions)
ach <- agnes(simil.matrix,diss=TRUE,,,"single")