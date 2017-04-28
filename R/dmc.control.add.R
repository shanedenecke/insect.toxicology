#' Adds Controls for Each Pesticide You are Using
#' 
#' @param raw.data Raw toxicology in standard Batterham Lab Format
#' @param key Do you want to specify your own insecticide:solvent key? If so put it in here. Default is NULL
#' @param new.key Do you want to specify your own insecticide solvent key? Defaults to FALSE
#' @param solvent.override You can manually set all of the solvents to "water" "dmso" or "ethanol" if you please. Defaults to NA
#' @return returns data frame which has added the 0 dose for all of your insecticides
#' @export
dmc.control.add <- function(raw.data,key=NULL,new.key=F,solvent.overide=NA){
      
      ## Determine if a new key is to be accepted
      if(new.key==T){
            insecticide.key <- key
      }else{
            key <- list(c(insecticide="imidacloprid",solvent="h20"),
                        c(insecticide="spinosad",solvent="h20"),
                        c(insecticide="nicotine",solvent="h20"),
                        c(insecticide="malathion",solvent="h20"),
                        c(insecticide="imi-5-oh",solvent="dmso"),
                        c(insecticide="imi-olefin",solvent="dmso"),
                        c(insecticide="clothianidin",solvent="dmso"),
                        c(insecticide="acetamiprid",solvent="dmso"),
                        c(insecticide="lufenuron",solvent="dmso"),
                        c(insecticide="sulfoxaflor",solvent="dmso"),
                        c(insecticide="permethrin",solvent="dmso"),
                        c(insecticide="pyripole",solvent="ethanol"),
                        c(insecticide="nitenpyram",solvent="h20"),
                        c(insecticide="chlorantraniliprole",solvent="h20"),
                        c(insecticide="ivermectin",solvent="dmso"),
                        c(insecticide="abamectin",solvent="dmso"),
                        c(insecticide="mix",solvent="dmso"),
                        c(insecticide="ethanol",solvent="ethanol"),
                        c(insecticide="dmso",solvent="dmso"),
                        c(insecticide="h20",solvent="h20"))
      }
      names(key) <- sapply(key,function(x) x[1])
      
      ### Modify data frame
      colnames(raw.data) <- tolower(colnames(raw.data))
      raw.data$genotype <- tolower(raw.data$genotype)
      raw.data$pesticide <- tolower(raw.data$pesticide)
      raw.data$pesticide <- gsub(" ","",raw.data$pesticide)
      
      ## Add control doses for each pesticide and genotpe 
      
      index <- unique(select(raw.data,pesticide,genotype))
      clean.data.list <- vector('list',nrow(index))

      for(i in 1:nrow(index)){
            p <- index[i,1]
            #g <- index[i,2]
            sub.data <- subset(raw.data,pesticide==p)
            control.chemical <- key[[which(names(key)==p)]][2]
            if(is.na(solvent.overide)){
                  control.data <- subset(raw.data,pesticide==control.chemical)
            }else if(!is.na(solvent.overide)){
                  control.data <- subset(raw.data,pesticide==solvent.overide)
            }
            control.data$pesticide=sub.data$pesticide[1]
            clean.data.list[[paste(p,sep="_")]] <- rbind(sub.data,control.data)
            if (p==control.chemical) clean.data.list[[paste(p,sep="_")]] <- sub.data
      }
      
      return(rbindlist(clean.data.list,use.names=T))           
}
