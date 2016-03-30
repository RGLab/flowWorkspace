# copied from plyr to avoid the dependency on plyr
compact <- function (l) 
  Filter(Negate(is.null), l)
