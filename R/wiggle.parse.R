#' Parse Raw Wiggle Data
#' 
#' @param data data frame containing the raw wiggle data in two columns: "Image.Name" and "Wiggle.Index"
#' @param output.name A string of the desired name of the output file
#' @param write If TRUE the results will be written to a .csv file. Defaults to FALSE
#' @return Parses wiggle index outputs into cleaner more readable data frame
#' @export
wiggle.parse <- function(data,ouptut.name="parsed.wiggle.data.csv",write=T){
      split.frame <- matrix(unlist(strsplit(as.character(data$Image.Name),"\\).")),ncol=8,byrow=T)[,c(1:6,8)]
      split.frame <- gsub("\\(","",split.frame)
      split.frame <- gsub(".tif","",split.frame)
      split.frame <- gsub("opRight","TopRight",split.frame)
      split.frame <- gsub("opLeft","TopLeft",split.frame)
      split.frame <- gsub("owRight","LowRight",split.frame)
      split.frame <- gsub("owLeft","LowLeft",split.frame)
      parsed.data <- cbind(split.frame,as.numeric(as.character(data$Wiggle.Index)))
      colnames(parsed.data) <- c("genotype","dose","time","plate","insecticide","date","position","Wiggle.Index")
      parsed.data <- data.frame(parsed.data)
      parsed.data$time <- as.numeric(gsub('min',"",parsed.data$time))
      parsed.data$Wiggle.Index <- as.numeric(as.character(parsed.data$Wiggle.Index))
      if(write==T){
            write.csv(x=parsed.data,file=ouptut.name,row.names = F)
      }
      return(parsed.data) 
}

