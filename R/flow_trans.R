#' Gating-ML 2.0 Log transformation.
#'
#' Used to construct GML 2.0 flog transformer object.
#'
#' GML 2.0 standard log transform function constructor. The definition is as
#' in the GML 2.0 standard section 6.2 "parametrized logarithmic transformation -- flog"
#' This deviates from standard only in the following way. Before applying the logarithmic
#' transformation, non-positive values are assigned the smallest positive value from
#' the input rather than having undefined values (NA) under the transformation.
#'
#' @inheritParams flow_breaks
#' @param t top scale value 
#' @param m number of decades
#' @return logtGml2 transformation object
#' @examples
#' trans.obj <- logtGml2_trans(t = 1e3, m = 1, equal.space = TRUE)
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
logtGml2_trans <- function (t = 262144, m = 4.5, n = 6, equal.space = FALSE)
{
  t = eval(t)
  m = eval(m)
  trans <- function(x){
    x[x<0] <- min(x[x>0])
    log10(x/t)/m + 1
  }
  inv <- function(x){
    t*(10 ^ (m*(x-1)))
  }
  flow_trans(name = "logtGml2", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

#' @templateVar old flowJo.flog
#' @templateVar new flowjo_log_trans
#' @template template-depr_pkg
NULL

#' flog transform function
#'
#' flog transform function constructor. It is different from flowCore version of \link{logtGml2}
#' in the way that it reset negative input so that no NAN will be returned.
#'
#' @name flowjo_log_trans
#' @aliases flowJo.flog flowjo_flog
#' @param decade total number of decades (i.e. log(max)-log(min)
#' @param offset offset to the orignal input(i.e. min value)
#' @param scale the linear scale factor
#' @inheritParams flow_breaks
#' @return flog(or its inverse) transform function
#' @examples
#' trans <- flowjo_log_trans()
#' data.raw <- c(1,1e2,1e3)
#' data.trans <- trans[["transform"]](data.raw)
#' data.trans
#'
#' inverse.trans <- trans[["inverse"]]
#' inverse.trans(data.trans)
#'
#'#negative input
#' data.raw <- c(-10,1e2,1e3)
#' data.trans <- trans[["transform"]](data.raw)
#' data.trans
#' inverse.trans(data.trans)#we lose the original value at lower end since flog can't restore negative value
#' 
#' #different
#' trans <- flowjo_log_trans(decade = 3, offset = 30)
#' data.trans <- trans[["transform"]](data.raw)
#' data.trans
#' inverse.trans <- trans[["inverse"]]
#' inverse.trans(data.trans)
#' 
#' @export  
flowjo_log_trans <- function(decade = 4.5, offset = 1, scale = 1, n = 6, equal.space = FALSE)
{
  decade = eval(decade)
  offset = eval(offset)
  scale = eval(scale)
   trans <- function(x){
    x[x<offset] <- offset
    x <- log10(x)-log10(offset)
    x * scale/decade
  }
  
   inv <- function(x)
   {
     10 ^ (x* decade/scale + log10(offset))
   }
   
  
  flow_trans(name = "flowJo_log", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

#' @export
flowJo.flog <- function(decade = 4.5, offset = 1, max_val = 262144, min_val = 0, scale = 1, inverse = FALSE){
  .Defunct("flowjo_log_trans")
  }
#' wrap the calibration table into transformation function using stats:::splinefun
#'
#' @param coef the coefficients returned by the calibration table from flowJo
#' @noRd 
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
#' @templateVar old flowJoTrans
#' @templateVar new flowjo_biexp
#' @template template-depr_pkg
NULL
#' construct the flowJo-type biexponentioal transformation function
#'
#' Normally it was parsed from flowJo xml workspace. This function provides the alternate
#' way to construct the flowJo version of logicle transformation function within R.
#' 
#' @name flowjo_biexp
#' @aliases flowJoTrans
#' @param channelRange \code{numeric} the maximum value of transformed data
#' @param maxValue \code{numeric} the maximum value of input data
#' @param pos \code{numeric} the full width of the transformed display in asymptotic decades
#' @param neg \code{numeric} Additional negative range to be included in the display in asymptotic decades
#' @param widthBasis \code{numeric} unkown.
#' @param inverse \code{logical} whether to return the inverse transformation function.
#' @export
#' @examples
#' trans <- flowjo_biexp()
#' data.raw <- c(-1, 1e3, 1e5)
#' data.trans <- trans(data.raw)
#' round(data.trans)
#' inv <- flowjo_biexp(inverse = TRUE)
#' round(inv(data.trans))
flowjo_biexp <- function(channelRange=4096, maxValue=262144, pos = 4.5, neg = 0, widthBasis = -10, inverse = FALSE){
  channelRange = eval(channelRange)
  maxValue = eval(maxValue)
  pos = eval(pos)
  neg = eval(neg)
  widthBasis = eval(widthBasis)
  coef <- getSplineCoefs(channelRange = channelRange, maxValue = maxValue, pos = pos, neg = neg, widthBasis = widthBasis, inverse = inverse)
  .flowJoTrans(coef)

}

#' @export
flowJoTrans <- function(channelRange=4096, maxValue=262144, pos = 4.5, neg = 0, widthBasis = -10, inverse = FALSE){
  .Deprecated("flowjo_biexp")
  flowjo_biexp(channelRange, maxValue, pos, neg, widthBasis, inverse)
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
#' library(flowCore)
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

#' @templateVar old flowJo_biexp_trans
#' @templateVar new flowjo_biexp_trans
#' @template template-depr_pkg
NULL
#' flowJo biexponential transformation.
#'
#' Used for constructing biexponential transformation object.
#'
#' @export
#' @importFrom scales trans_new format_format
#' @inheritParams flow_breaks
#' @param ... parameters passed to \code{\link[flowWorkspace]{flowJoTrans}}
#' @examples
#' library(flowCore)
#' data(GvHD)
#' fr <- GvHD[[1]]
#' data.raw <- exprs(fr)[, "FL1-H"]
#' trans.obj <- flowjo_biexp_trans(equal.space = TRUE)
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data.raw)
#' brks # biexp space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#'
#' print(trans.func(brks))
#' @return biexponential transformation object
flowjo_biexp_trans <- function(..., n = 6, equal.space = FALSE){

  trans <- flowjo_biexp(...)
  inv <- flowjo_biexp(..., inverse = TRUE)
  flow_trans(name = "flowJo_biexp", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)

}

#' @rdname flowjo_biexp_trans
#' @export
flowJo_biexp_trans <- function(...){
  .Deprecated("flowjo_biexp_trans")
  flowjo_biexp_trans(...)
}

#' @templateVar old flowJo.fasinh
#' @templateVar new flowjo_fasinh
#' @template template-depr_pkg
NULL
#' inverse hyperbolic sine transform function
#'
#'  hyperbolic sine/inverse hyperbolic sine (flowJo-version) transform function constructor
#'
#' @name flowjo_fasinh
#' @aliases flowJo.fasinh
#' @param m numeric the full width of the transformed display in asymptotic decades
#' @param t numeric the maximum value of input data
#' @param a numeric Additional negative range to be included in the display in asymptotic decades
#' @param length numeric the maximum value of transformed data
#' @return fasinh/fsinh transform function
#' @examples
#' trans <- flowjo_fasinh()
#' data.raw <- c(1,1e2,1e3)
#' data.trans <- trans(data.raw)
#' data.trans
#'
#' inverse.trans <- flowjo_fsinh()
#' inverse.trans(data.trans)
#'
#' @export
flowjo_fasinh <- function (m = 4.0, t = 12000, a =  0.7, length = 256)
{
  m = eval(m)
  t = eval(t)
  a = eval(a)
  
  function(x){ #copied fom c++ code
    length * ((asinh(x * sinh(m * log(10)) / t) + a * log(10)) / ((m + a) * log(10)))
  }
}

#' @export
flowJo.fasinh <- function(m = 4.0, t = 12000, a =  0.7, length = 256){
  .Defunct("flowjo_fasinh")
}

#' @templateVar old flowJo.fsinh
#' @templateVar new flowjo_fsinh
#' @template template-depr_pkg
NULL
#' @name flowjo_fasinh
#' @aliases flowJo.fsinh
#' @export
flowjo_fsinh <- function(m = 4.0, t = 12000, a =  0.7, length = 256){
  m = eval(m)
  t = eval(t)
  a = eval(a)
  function(x){
    sinh(((m + a) * log(10)) * x/length - a * log(10)) * t / sinh(m * log(10))
  }
}

#' @export
flowJo.fsinh <- function(m = 4.0, t = 12000, a =  0.7, length = 256){
  .Defunct("flowjo_fsinh")
  }


#' @templateVar old flowJo_fasinh_trans
#' @templateVar new flowjo_fasinh_trans
#' @template template-depr_pkg
NULL
#' flowJo inverse hyperbolic sine transformation.
#'
#' Used to construct the inverse hyperbolic sine transform object.
#'
#' @inheritParams flow_breaks
#' @param ... parameters passed to flowjo_fasinh
#' @return fasinh transformation object
#' @examples
#' trans.obj <- flowjo_fasinh_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#' round(trans.func(brks))
#' @rdname flowjo_fasinh_trans
#' @export
flowjo_fasinh_trans <- function(..., n = 6, equal.space = FALSE){
  trans <- flowjo_fasinh(...)
  inv <- flowjo_fsinh(...)
  flow_trans(name = "flowJo_fasinh", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}

#' @rdname flowjo_fasinh_trans
#' @export
flowJo_fasinh_trans <- function(...){
  .Deprecated("flowjo_fasinh_trans")
  flowjo_fasinh_trans(...)
}


#' inverse hyperbolic sine transform function generator (GatingML 2.0 version)
#'
#' hyperbolic sine/inverse hyperbolic sine transform function constructor.
#' It is simply a special form of \code{flowjo_fasinh} with \code{length} set to 1
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
  #avoid lazy evaluation side effects
  #https://github.com/RGLab/CytoML/issues/130
    T = eval(T)
    M = eval(M)
    A = eval(A)
    if(inverse){

      flowjo_fsinh(m = M, t = T, a = A, length = 1)
    }else{

      flowjo_fasinh(m = M, t = T, a = A, length = 1)
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
  trans <- trans.obj@.Data#by retrieving the function object, it effectively force the argument to be evaluated. so no need to explicitly call eval()
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
  T = eval(T)
  M = eval(M)
  W = eval(W)
  A = eval(A)
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
