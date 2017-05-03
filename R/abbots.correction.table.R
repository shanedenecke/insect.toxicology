#' Creates Abbot's Correction Plots and Tables for Toxicology
#'
#' @param adjusted.data A data frame or data table. Must include control dose, 0, for each insecticide. This is input for this value is usually an ouput from dmc.correct
#' @param control A string of your control genotype. e.g "Line14"
#' @param write If TRUE graphs will be produced in the specified format and .csv tables will be generated. If FALSE ouput will be localized to R. Defaults to TRUE
#' @param format what format do you want your graph images? Possible values "pdf", "png","tiff","jpeg"
#' @return Creates Abbot's correction Plots and Tables for Toxicology
#' @export
abbots.correction.table <- function(adjusted.data,control,write=T,subfolder="Abbots_Correction",interval=.95){
      options(warn=-1)
      if(!dir.exists(subfolder)){
            dir.create(file.path(getwd(),subfolder))
            setwd(file.path(getwd(),subfolder))
      }
      
      colnames(adjusted.data) <- tolower(colnames(adjusted.data))
      index <- adjusted.data %>% select(genotype,dose,pesticide) %>% unique() %>% data.frame()
      reduced.index <- index %>% select(genotype,pesticide) %>% unique()
      emlist <- list()
      for(i in 1:nrow(index)){
            sub.data.pesticide=filter(adjusted.data,pesticide==as.character(index[i,'pesticide']) & genotype==as.character(index[i,'genotype']) & dose %in% as.numeric(c(0,index[i,'dose'])))
            sum.data <- matrix(ncol=4,nrow=1)
            colnames(sum.data) <- c("Genotype","Dose","Corrected.Mortality","CI")
            summary.genotype <- sub.data.pesticide %>% group_by(dose) %>%
                  summarize(mean_mortality=mean(percent_mortality),n=length(percent_mortality),var_mortality=var(percent_mortality))
            
            attach(summary.genotype,warn.conflicts=F)
            ## Calculate Abbots Correction
            control.mean <- mean_mortality[dose==0]
            control.count <- n[dose==0]
            control.var <- var_mortality[dose==0]
            ex.mean <- mean_mortality[dose==index[i,'dose']]
            ex.count <- n[dose==index[i,'dose']]
            ex.var <- var_mortality[dose==index[i,'dose']]
            dof <- min(ex.count,control.count) - 1 #degrees of freedom
            t.value <- qt(1-interval, dof)
            g <- (control.var * (t.value^2))/(((1-control.mean)^2) * control.count)
            p.corr.mort <- 1 - ((1-ex.mean)/((1-control.mean)/(1-g)))
            c.i <- ((((1-g) * (ex.var/ex.count)) + ((((1-ex.mean)^2) * control.var) / (((1-control.mean)^2) * control.count)))^0.5) * (t.value/((1-control.mean) / (1-g)))
            emlist[[i]] <- data.frame(corrected.mortality=p.corr.mort,conf.int=c.i)
            detach(summary.genotype)
      }
      
      summary.data <- cbind(index,rbindlist(emlist)) %>%
            mutate(corrected.survival=1-corrected.mortality) %>%
            mutate(genotype=factor(genotype,factor(genotype,levels=tolower(c(control,
            unique(as.character(genotype)[which(unique(as.character(genotype))!=control)])))))) %>%
            mutate(dose=as.factor(dose)) %>%
            mutate(ucl=corrected.survival-conf.int) %>%
            mutate(lcl=corrected.survival+conf.int) %>%
            select(genotype,dose,pesticide,corrected.survival,conf.int,ucl,lcl)
      
      if(write==T){
            write.csv(summary.data,file="Abbots_Summary.csv")
      }
      else{
            return(summary.data)
      }
      setwd("..")
      options(warn=0)
}
