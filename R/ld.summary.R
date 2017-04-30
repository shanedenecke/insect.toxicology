#' Summarizes data into LC50 table
#' @param adjusted.data data frame of toxicology in standard format
#' @param conf.level What do you want your confidence intervals to be % wise? defaults to .95
#' @param ld.level What LD level do you want to predict? defaults to 50 to calculate Ld50
#' @return Calculates LD values for genotype:insecticide combination
#' @export
ld.summary <- function(adjusted.data,conf.level=.95,ld.level=50){
      
      colnames(adjusted.data) <- tolower(colnames(adjusted.data))
      sum.data <- adjusted.data %>% group_by(pesticide,genotype,dose) %>%
            summarize(alive=sum(alive),dead=sum(dead),total=sum(total))
      
      index <-  select(sum.data,pesticide,genotype) %>% unique() %>% data.frame()
      l <- vector('list',nrow(index))
      names(l) <- paste(index$pesticide,index$genotype,sep="_")
      for(i in 1:nrow(index)){
            
            pest <- as.character(index[i,'pesticide'])
            gen <- as.character(index[i,'genotype'])
            
            sub.data <- subset(sum.data,genotype==gen & pesticide==pest)
            
            l[[i]] <- with(sub.data,LD(dead,total,dose,conf.level,ld.level))
      }
      output <- cbind(index,rbindlist(l,use.names=T))
      return(output)
}

