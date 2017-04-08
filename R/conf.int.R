conf.int <- function(data,level=.95){
      ci <- mean(data)+c(-1,1)*qnorm(1-((1-level)/2))*sd(data)/sqrt(length(data))
      return(ci)
}
