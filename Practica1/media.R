media <- function(var) {
    sum<-0

    for (data in var) {
        sum<-sum+data
    }

    return(sum/length(var))
}