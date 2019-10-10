toTxt <- function(save,name,r) {
    write.table(save, paste("./",name,sep=""), sep="\t", row.names=r) 
}