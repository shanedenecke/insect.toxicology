#' Merge Wiggle Files
#' 
#' @param wd The working directory that your collection of .xls wiggle outputs are in
#' @param output.name A string of the desired name of the output file
#' @param write If TRUE the results will be written to a .csv file. Defaults to FALSE
#' @return Merges wiggle index outputs from individual wells into one file




wiggle.merge <- function(wd=".",output.name="merged.wiggle.data.csv",write=T){
      setwd(wd)
      merged.data <- data.frame()
      for (i in list.files()){
            if (grepl(".xls",i)){
                  single.well.data <- read.table(i,sep="\t",header=T) 
                  relevant.columns=cbind(as.character(single.well.data$Image.Name),single.well.data$Wiggle.Index) 
                  merged.data=rbind(merged.data,relevant.columns)
            } 
      }
      colnames(merged.data)=c("Image.Name","Wiggle.Index")
      if(write==T){
            write.csv(x=merged.data,file=output.name,row.names = F)
      }
      return(merged.data)
}