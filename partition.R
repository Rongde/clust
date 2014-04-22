
setClass("partition",
        representation(labels="vector"))

setMethod(f="entropyOfPartition",
          signature="partition",
          definition=function(){
              populations=table(partition)
              probability.distribution=populations/sum(populations)
              return(entropy(probability.distribution))
            }
          
)