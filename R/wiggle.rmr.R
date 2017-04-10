#' Correct Wiggle Data
#' 
#' @param data parsed wiggle data frame containing the columns genotype, time, dose, well, rep, and date. Usually this data frame is the ouput of wiggle.parse
#' @param output.name A string of the desired name of the output file
#' @param write If TRUE the results will be written to a .csv file. Defaults to FALSE
#' @return Corrects wiggle data by adding RMR value. See Denecke et. al 2017
#' @export
wiggle.rmr <- function(data,file.name="rmr.wiggle.data.csv",write=T){
      library(tidyr)
      library(dplyr)
      spread.data <- spread(data,time,Wiggle.Index)
      for(i in colnames(spread.data)[-1:-6]){
            rmr.values <- spread.data[[i]]/spread.data[["0"]]
            new.name <-paste(i,"rmr",sep="_")
            spread.data[[new.name]] <- rmr.values
      }
      raw.gather <- gather(spread.data,key="time",value="raw",
            which(colnames(spread.data) %in% colnames(spread.data)[which(grepl("[0-9]",colnames(spread.data)) & !grepl("rmr",colnames(spread.data)))]))     
      rmr.gather <- gather(spread.data,key="time",value="rmr",
            which(colnames(spread.data) %in% colnames(spread.data)[which(grepl("[0-9]",colnames(spread.data)) & grepl("rmr",colnames(spread.data)))]))     
      rmr.gather$time <- gsub("_rmr","",rmr.gather$time)
      rmr.data <- merge(raw.gather,rmr.gather) %>% select(genotype,dose,plate,insecticide,date,position,time,raw,rmr)
      if(write==T){
            write.csv(x=rmr.data,file=file.name,row.names = F)
      }
      return(rmr.data)
}
