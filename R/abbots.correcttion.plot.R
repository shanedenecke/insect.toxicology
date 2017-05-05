#' Creates Abbot's Correction Plots and Tables for Toxicology
#'
#' @param adjusted.data A data frame or data table. Must include control dose, 0, for each insecticide. This is input for this value is usually an ouput from dmc.correct
#' @param control A string of your control genotype. e.g "Line14"
#' @param write If TRUE graphs will be produced in the specified format and .csv tables will be generated. If FALSE ouput will be localized to R. Defaults to TRUE
#' @param format what format do you want your graph images? Possible values "pdf", "png","tiff","jpeg"
#' @return Creates Abbot's correction Plots and Tables for Toxicology
#' @export
abbots.correction.plot <- function(adjusted.data,control,write=T,format="pdf",subfolder="Abbots_Correction",interval=.95){
      if(!dir.exists(subfolder)){
      dir.create(file.path(getwd(),subfolder))
      }
      setwd(file.path(getwd(),subfolder))
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

            attach(summary.genotype)
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

      summary.data <- cbind(index,rbindlist(emlist))
      summary.data$corrected.survival <- 1-summary.data$corrected.mortality
      summary.data$genotype <- factor(summary.data$genotype,levels=tolower(c(control,as.character(unique(summary.data$genotype)[which(unique(summary.data$genotype)!=control)]))))
      summary.data$dose <- as.factor(summary.data$dose)

      for(p in unique(adjusted.data$pesticide)){
            plot.data <- subset(summary.data,pesticide==p)
            ## Plot Abbots Correction
            if(write==T){
                  get(format)(file=paste(p,"Abbots_Correction.",format,sep=""))
            }
            gp <- ggplot(data=plot.data,aes(x=dose,y=corrected.survival,group=interaction(dose,genotype)))
            gp <- gp+geom_bar(aes(fill=genotype),position=position_dodge(width=.5),stat="identity",colour="black",width=.5)
            gp <- gp+geom_errorbar(aes(ymin=corrected.survival+conf.int,ymax=corrected.survival-conf.int),position=position_dodge(width=.5),stat="identity",width=.5)
            gp <- gp+ggtitle(paste("Abbots Correction",p,sep=" "))
            gp <- gp+scale_y_continuous(breaks=seq(0,1,by=.1))
            gp <- gp+ylab("Corrected Percent Survival\n")
            gp <- gp+xlab("\nDose (ppm)")
            gp <- gp+scale_x_discrete(breaks=unique(plot.data$dose))
            gp <- gp+scale_fill_manual(values=c("grey50",rainbow(length(unique(summary.data$genotype))-1)))
            gp <- gp+theme_bw(base_size = 14, base_family = "serif")
            gp <- gp+theme(text=element_text(face="bold",family="serif"),panel.grid=element_blank(),axis.ticks.x=element_line(),
                           panel.border=element_rect(colour="black",fill=NA),strip.text=element_text(size=20),strip.background=element_rect("grey95"),
                           axis.title=element_text(size=17))
            print(gp)
            if(write==T){
                  dev.off()
            }
      }
      setwd("..")
}
