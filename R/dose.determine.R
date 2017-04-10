#' Determines Doses for an Lc50 screen 
#' 
#' @param min The minimum concentration that will be used in your screen. Ideally only a small number of individuals should die at this dose
#' @param max The minimum concentration that will be used in your screen. Ideally only a small number of individuals should survive at this dose
#' @param n The number of doses that you want to use in your screen. More is always better but is always more
#' @return This function determines the doses to use for an Lc50 screen
#' @export
dose.determine <- function(min,max,n){
      diff=max-min
      step=(1/(n-1))*diff
      doses=seq(min,max,by=step)
      return(doses)
}

