#' wrap the calibration table into transformation function using stats:::splinefun
#'
#' @param coef the coefficients returned by the calibration table from flowJo
.flowJoTrans <- function(coef){
  #define the dummy spline function(simplied version of the one from stats package)
  f <- function (x, deriv = 0)
  {
    deriv <- as.integer(deriv)
    if (deriv < 0 || deriv > 3)
      stop("'deriv' must be between 0 and 3")
    if (deriv > 0) {
      z0 <- double(z$n)
      z[c("y", "b", "c")] <- switch(deriv, list(y = z$b, b = 2 *
                  z$c, c = 3 * z$d), list(y = 2 * z$c, b = 6 * z$d,
              c = z0), list(y = 6 * z$d, b = z0, c = z0))
      z[["d"]] <- z0
    }

    res <- stats::: .splinefun(x,z)
    if (deriv > 0 && z$method == 2 && any(ind <- x <= z$x[1L]))
      res[ind] <- ifelse(deriv == 1, z$y[1L], 0)
    res
  }
  #update the parameters of the function
  z<-coef$z
  z$n<-length(z$x)
  z$method<-coef$method
  assign("z",z,environment(f))

  type <- coef$type
  attr(f,"type") <- type
  if(type == "biexp")
    attr(f,"parameters") <- list(channelRange = coef$channelRange
        , maxValue = coef$maxValue
        , neg = coef$neg
        , pos = coef$pos
        , widthBasis = coef$widthBasis
    )


  return (f)

}

#' construct the flowJo-type biexponentioal transformation function
#'
#' Normally it was parsed from flowJo xml workspace. This function provides the alternate
#' way to construct the flowJo version of logicle transformation function within R.
#'
#' @param channelRange \code{numeric} the maximum value of transformed data
#' @param maxValue \code{numeric} the maximum value of input data
#' @param pos \code{numeric} the full width of the transformed display in asymptotic decades
#' @param neg \code{numeric} Additional negative range to be included in the display in asymptotic decades
#' @param widthBasis \code{numeric} unkown.
#' @param inverse \code{logical} whether to return the inverse transformation function.
#' @export
#' @examples
#' trans <- flowJoTrans()
#' data.raw <- c(-1, 1e3, 1e5)
#' data.trans <- trans(data.raw)
#' round(data.trans)
#' inv <- flowJoTrans(inverse = TRUE)
#' round(inv(data.trans))
flowJoTrans <- function(channelRange=4096, maxValue=262144, pos = 4.5, neg = 0, widthBasis = -10, inverse = FALSE){

  coef <- .getSplineCoefs(channelRange = channelRange, maxValue = maxValue, pos = pos, neg = neg, widthBasis = widthBasis, inverse = inverse)
  .flowJoTrans(coef)

}

#' Generate the breaks that makes sense for flow data visualization
#'
#' It is mainly used as helper function to construct breaks function used by 'trans_new'.
#'
#' @return either 10^n intervals or equal-spaced(after transformed) intervals in raw scale.
#' @param n desired number of breaks (the actual number will be different depending on the data range)
#' @param x the raw data values
#' @param equal.space whether breaks at equal-spaced intervals
#' @param trans.fun the transform function (only needed when equal.space is TRUE)
#' @param inverse.fun the inverse function (only needed when equal.space is TRUE)
#' @export
#' @examples
#' data(GvHD)
#' fr <- GvHD[[1]]
#' data.raw <- exprs(fr)[, "FL1-H"]
#' flow_breaks(data.raw)
#'
#' trans <- logicleTransform()
#' inv <- inverseLogicleTransform(trans = trans)
#' myBrks <- flow_breaks(data.raw, equal.space = TRUE, trans = trans, inv = inv)
#' round(myBrks)
#' #to verify it is equally spaced at transformed scale
#' print(trans(myBrks))
flow_breaks <- function(x, n = 6, equal.space = FALSE, trans.fun, inverse.fun){
  rng.raw <- range(x, na.rm = TRUE)
  if(equal.space){

    rng <- trans.fun(rng.raw)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min)
      return(inverse.fun(min))
    by <- (max - min)/(n-1)

    myBreaks <- inverse.fun(seq(min, max, by = by))

  }else{
    #log10 (e.g. 0, 10, 1000, ...)
    base10raw <- unlist(lapply(2:n,function(e)10^e))
    base10raw <- c(0,base10raw)
    myBreaks <- base10raw[base10raw>rng.raw[1]&base10raw<rng.raw[2]]

  }
  myBreaks
}


#' helper function to generate a trans objects
#' Used by other specific trans constructor
#' @param name transformation name
#' @inheritParams flow_breaks
#' @export
flow_trans <- function(name, trans.fun, inverse.fun, equal.space = FALSE, n = 6){

  brk <- function(x){
    flow_breaks(x, n = n, equal.space = equal.space, trans.fun, inverse.fun)
  }

   if(equal.space){
    fmt <- format_format(digits = 0)
  }else{
    fmt <- function(x){
      pretty10exp(as.numeric(x),drop.1=TRUE)
    }

  }

  trans_new(name, transform = trans.fun, inverse = inverse.fun, breaks = brk, format = fmt)
}


#' flowJo biexponential transformation.
#'
#' Used for constructing biexponential transformation object.
#'
#' @export
#' @importFrom scales trans_new format_format
#' @inheritParams flow_breaks
#' @param ... parameters passed to \code{\link[flowWorkspace]{flowJoTrans}}
#' @examples
#' data(GvHD)
#' fr <- GvHD[[1]]
#' data.raw <- exprs(fr)[, "FL1-H"]
#' trans.obj <- flowJo_biexp_trans(equal.space = TRUE)
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data.raw)
#' brks # biexp space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#'
#' print(trans.func(brks))
#' @return biexponential transformation object
flowJo_biexp_trans <- function(..., n = 6, equal.space = FALSE){

  trans <- flowJoTrans(...)
  inv <- flowJoTrans(..., inverse = TRUE)
  flow_trans(name = "flowJo_biexp", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)

}

#' inverse hyperbolic sine transform function
#'
#'  hyperbolic sine/inverse hyperbolic sine (flowJo-version) transform function constructor
#'
#' @rdname flowJo.fasinh
#' @param m numeric the full width of the transformed display in asymptotic decades
#' @param t numeric the maximum value of input data
#' @param a numeric Additional negative range to be included in the display in asymptotic decades
#' @param length numeric the maximum value of transformed data
#' @return fasinh/fsinh transform function
#' @examples
#' trans <- flowJo.fasinh()
#' data.raw <- c(1,1e2,1e3)
#' data.trans <- trans(data.raw)
#' data.trans
#'
#' inverse.trans <- flowJo.fsinh()
#' inverse.trans(data.trans)
#'
#' @export
flowJo.fasinh <- function (m = 4.0, t = 12000, a =  0.7, length = 256)
{
  function(x){ #copied fom c++ code
    length * ((asinh(x * sinh(m * log(10)) / t) + a * log(10)) / ((m + a) * log(10)))
  }
}

#' @rdname flowJo.fasinh
#' @export
flowJo.fsinh <- function(m = 4.0, t = 12000, a =  0.7, length = 256){
  function(x){
    sinh(((m + a) * log(10)) * x/length - a * log(10)) * t / sinh(m * log(10))
  }
}



#' flowJo inverse hyperbolic sine transformation.
#'
#' Used to construct the inverse hyperbolic sine transform object.
#'
#' @inheritParams flow_breaks
#' @param ... parameters passed to flowJo.fasinh
#' @return fasinh transformation object
#' @examples
#' trans.obj <- flowJo_fasinh_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#' round(trans.func(brks))
#' @export
flowJo_fasinh_trans <- function(..., n = 6, equal.space = FALSE){
  trans <- flowJo.fasinh(...)
  inv <- flowJo.fsinh(...)
  flow_trans(name = "flowJo_fasinh", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}

#' inverse hyperbolic sine transform function generator (GatingML 2.0 version)
#'
#' hyperbolic sine/inverse hyperbolic sine transform function constructor.
#' It is simply a special form of \code{flowJo.fasinh} with \code{length} set to 1
#' and different default values for parameters \code{t,m,a}.
#'
#' @rdname asinh_Gml2
#' @param M numeric the full width of the transformed display in asymptotic decades
#' @param T numeric the maximum value of input data
#' @param A numeric Additional negative range to be included in the display in asymptotic decades
#' @param inverse whether to return the inverse function
#' @return fasinh/fsinh transform function
#' @examples
#' trans <- asinh_Gml2()
#' data.raw <- c(1,1e2,1e3)
#' data.trans <- trans(data.raw)
#' data.trans
#'
#' inverse.trans <- asinh_Gml2(inverse = TRUE)
#' inverse.trans(data.trans)
#'
#' @export
asinh_Gml2 <- function(T = 262144,M = 4.5,A = 0, inverse = FALSE)
{

    if(inverse){

      flowJo.fsinh(m = M, t = T, a = A, length = 1)
    }else{

      flowJo.fasinh(m = M, t = T, a = A, length = 1)
    }


}


#' Inverse hyperbolic sine transformation.
#'
#' Used to construct inverse hyperbolic sine transform object.
#'
#' @inheritParams flow_breaks
#' @param ... parameters passed to asinh_Gml2
#' @return asinhtGml2 transformation object
#' @examples
#' trans.obj <- asinhtGml2_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#' brks.trans <- trans.func(brks)
#' brks.trans
#' @export
asinhtGml2_trans <- function(..., n = 6, equal.space = FALSE){
  trans <- asinh_Gml2(...)
  inv <- asinh_Gml2(..., inverse = TRUE)
  flow_trans(name = "asinhtGml2", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}


#' logicle transformation.
#'
#' Used for construct logicle transform object.
#'
#' @inheritParams flow_breaks
#' @param ... arguments passed to logicleTransform.
#' @return a logicle transformation object
#' @examples
#' trans.obj <- logicle_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # logicle space displayed at raw data scale
#' #transform it to verify the equal-spaced breaks at transformed scale
#' print(trans.obj[["transform"]](brks))
#' @export
logicle_trans <- function(..., n = 6, equal.space = FALSE){
  trans.obj <- logicleTransform(...)
  trans <- trans.obj@.Data
  inv <- inverseLogicleTransform(trans = trans.obj)@.Data
  flow_trans(name = "logicle", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}


#' GatingML2 version of logicle transformation.
#'
#' The only difference from \link{logicle_trans} is it is scaled to c(0,1) range.
#'
#' @inheritParams flow_breaks
#' @param T,M,W,A see \link{logicletGml2}
#' @return a logicleGml2 transformation object
#' @examples
#' trans.obj <- logicleGml2_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # logicle space displayed at raw data scale
#' #transform it to verify the equal-spaced breaks at transformed scale
#' print(trans.obj[["transform"]](brks))
#' @export
logicleGml2_trans <- function (T = 262144, M = 4.5, W = 0.5, A = 0, n = 6, equal.space = FALSE)
{

  trans <- function (x)
  {

    flowCore:::logicle_transform(as.double(x), as.double(T), as.double(W), as.double(M), as.double(A), FALSE)/M


  }


  inv <- function (x)
  {

    flowCore:::logicle_transform(as.double(x) * M, as.double(T), as.double(W), as.double(M), as.double(A), TRUE)


  }
  flow_trans(name = "logicleGml2", trans.fun = trans, inverse.fun = inv,n = n, equal.space = equal.space)
}
