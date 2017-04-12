a <- example.wiggle.data %>% wiggle.parse() %>% wiggle.rmr()
control="HRG4xUAS-NULL"
other.genotypes="all"


wiggle.plot <- function(rmr.data,control,other.genotypes="all",colours=c("grey50","red","blue","gold4","purple","orange"),write=T,format="pdf",font="serif"){
      if(other.genotypes=="all"){
            other.genotypes <- as.character(unique(rmr.data$genotype))[which(unique(rmr.data$genotype)!=control)]
      }

      sub.data <- rmr.data %>% filter(genotype %in% c(control,other.genotypes))
      lengthgen <- length(unique(sub.data$genotype))-1
      sum.data <- sub.data %>% group_by(time,insecticide,genotype) %>%
            summarize(rmr.mean=mean(rmr), sd=sd(rmr), se=sd/sqrt(length(rmr)), ci= qnorm(.975)*se)
      sum.data$time <- as.numeric(as.character(sum.data$time))
      sum.data$genotype <- factor(sum.data$genotype,levels=c(control,other.genotypes))
      errorbar.scale <- max(sum.data$time)/10
      batts.colour.pallete <- c("grey50","red","blue","gold4","purple","orange")
      for(p in unique(sum.data$insecticide)){
            plot.data <- subset(sum.data,insecticide==p)

            if(write==T){
                  get(format)(file=paste(p,"individual vial dotplot.",format,sep=""))
            }


            gp <- ggplot(plot.data,aes(x=time,y=rmr.mean,colour=genotype))
            gp <- gp+geom_line(size=.9,aes(linetype=genotype))
            gp <- gp+geom_errorbar(aes(ymin=rmr.mean-se, ymax=rmr.mean+se),width=errorbar.scale,size=.5)
            gp <- gp+geom_point(size=3,fill=1,aes(shape=genotype))
            gp <- gp+scale_colour_manual(values=c(colours[1:(lengthgen+1)]))
            gp <- gp+scale_shape_manual(values=c(1,rep(2,lengthgen)))
            gp <- gp+scale_linetype_manual(values=c(2,rep(1,lengthgen)))
            gp <- gp+labs(x="\nTime (min)",y="RMR\n")
            gp <- gp+theme_bw(base_size = 14,base_family = font)
            gp <- gp+ theme(text=element_text(face="bold",family="serif"),panel.grid=element_blank(),axis.ticks.x=element_line(),
                                              panel.border=element_rect(colour="black",fill=NA),strip.text=element_text(size=18),strip.background=element_rect("grey95"),
                                              axis.title=element_text(size=18),axis.text.x=element_text(angle=30,hjust=1))
            print(gp)
            if(write==T){
                  dev.off()
            }
      }
}
