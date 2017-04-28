#' Creates Dotplots That Reflect How Each Vial Survives an Insecticide Treatment
#' 
#' @param adjusted.data A data frame or data table. Must include control dose, 0, for each insecticide. This is input for this value is usually an ouput from dmc.correct
#' @param control A string of your control genotype. e.g "Line14"
#' @param write If TRUE graphs will be produced in the specified format and .csv tables will be generated. If FALSE ouput will be localized to R. Defaults to TRUE
#' @param format what format do you want your graph images? Possible values "pdf", "png","tiff","jpeg"
#' @param subfolder What do you want the subfolder to be called. This will contain all figures and tables generated in this study
#' @return Creates Abbot's correction Plots and Tables for Toxicology
#' @export
response.dotplot <- function(adjusted.data,control,write=T,format="pdf",subfolder="Individual_Vial_Dotplots"){
      dir.create(subfolder)
      setwd(paste("./",subfolder,sep=""))
      ungen <-  unique(adjusted.data$genotype)
      colnames(adjusted.data) <- tolower(colnames(adjusted.data))
      adjusted.data$genotype <- tolower(adjusted.data$genotype)
      adjusted.data$pesticide <- tolower(adjusted.data$pesticide)
      adjusted.data$pesticide <- gsub(" ","",adjusted.data$pesticide)
      adjusted.data$genotype <- factor(adjusted.data$genotype,levels=c(control,ungen[-which(ungen==control)]))
      
      for (p in as.character(unique(adjusted.data$pesticide))){
            
            sub.data=subset(adjusted.data,pesticide==p) 
            
            sub.data$dose <- as.factor(as.character(sub.data$dose))
            
            if(write==T){
                  get(format)(file=paste(p,"individual vial dotplot.",format,sep=""))
            }
            gp=ggplot(data=sub.data,aes(x=dose,y=alive,group=interaction(dose,genotype)))
            gp=gp+geom_dotplot(aes(fill=genotype),binaxis='y',stackdir="center", binwidth=.1,colour="black",position=position_dodge(width=.5),dotsize = 10)
            gp=gp+ggtitle(paste("Individual Vial Response for",p,sep=" ")) 
            gp=gp+stat_summary(geom="errorbar",colour='black',size=.6,fun.data=mean_cl_normal,position=position_dodge(width=.5),width=.5)
            gp=gp+ylab("Number Emerging\n")
            gp=gp+xlab("\nDose (ppm)")
            gp=gp+stat_summary(fun.y="mean",fun.ymax="mean",fun.ymin="mean", geom="crossbar",colour='black',position=position_dodge(width=.5),width=.5)
            gp=gp+scale_y_continuous(limits=c(0,55))
            gp=gp+scale_x_discrete(breaks=as.factor(sub.data$dose))
            gp <- gp+scale_colour_manual(values=c("grey50",rainbow(length(ungen)-1)))
            gp=gp+ggtitle(p)
            gp=gp+theme(text=element_text(size=18 ,face="bold"),
                        axis.text.x=element_text(angle = 60, hjust = 1,size=14),
                        axis.text.y=element_text(size=14,family="",face="bold"),
                        panel.background=element_rect(fill="grey95"),
                        panel.border=element_rect(colour="black",fill=NA),
                        panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
                        plot.title = element_text(hjust = 0.5))
            print(gp)
            if(write==T){
                  dev.off()
            }
      }
      setwd("..")
}