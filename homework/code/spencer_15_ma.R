spencer_15_ma<- function(data){
    out= vector(mode="numeric",length = length(data)-14)
    weights= c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)
    for (i in 1: (length(data)-14)){
        current_values<- data[i:(i+14)]
        out[i]<- sum(current_values*weights)/sum(weights)
    }
    return(out)
}