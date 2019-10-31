errorE <- function(x,y,r) {
    sz<-length(x)
    sum<-0
    for (i in 1:sz){
        sum<-sum+((r$a+r$b*x[i])-y[i])^2
    }

    sR<-sqrt(sum/sz)

    return(sR)
}