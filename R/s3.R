#' Temperature response curve for simple generic infection model
#'
#' Function \code{tresp} estimates the temperature response function of a disease
#' given minimum, \code{tmin}, maximum, \code{tmax}, and optimum, \code{topt}, 
#' temperatures for infection.
#'
#' @param x a scalar, vector or matrix of temperatures at which to estimate 
#' response function
#' @param tmin,tmax,topt scalar or vector minimum, maximum and optimum 
#' temperatures for infection, respectively; see Details
#' 
#' @details
#' 
#' If \code{tmin}, \code{tmax} and \code{topt} are scalar and \code{nrow(t)} > 1,
#' then their values are repeated \code{ncol{t}}. \code{tmin}, \code{tmax} and 
#' \code{topt} must be the same length. If \code{length(tmin)} > 1 then 
#' \code{length(tmin)} must equal \code{ncol{t}}, or, if \code{ncol{t}} = 1,
#' then columns are \code{t} are repeated \code{length(tmin)} times.
#' 
#' @references 
#' 
#' Magarey, R. D., T. B. Sutton, and C. L. Thayer. (2005). A simple generic 
#' infection model for foliar fungal plant pathogens. Phytopathology 95 (1): 
#' 92–100. \doi{10.1094/PHYTO-95-0092}
#'
#' @examples
#'
#' # phytophthora cactorum (apple fruit)
#' tempresp(0:40, 1, 35, 25)
#'
#' @return A scalar, vector or matrix
#' 
#' @export
#' 
tempresp <- function(x, tmin, tmax, topt) {
  x <- as.matrix(x)
  nv <- nrow(x)
  ns <- ncol(x)
  if (ns == 1) {
    lt <- c(length(tmin), length(tmax), length(topt))
    if (length(unique(lt)) != 1)
      stop('lengths of tmin, tmax and topt must all be the same.')
    ns <- max(lt)
    if (ns > 1) {
      x <- matrix(x, nv, ns)
    }
  } else {
    if (length(tmin) == 1) {
      tmin <- rep(tmin, ns)
      tmax <- rep(tmax, ns)
      topt <- rep(topt, ns)
    } else {
      if (ns / length(tmin) != 1)
        stop('lengths of tmin, tmax and topt must be one or ncol(x) as ncol(x) > 1.')
    }
  }
  .tresp_Rcpp(x, tmin, tmax, topt)
}  

#' Wet duration requirement for critical disease threshold at a given temperature
#'
#' Function \code{wetdur} estimates the wetness duration requirement (hours)
#' for the critical disease threshold for a given temperature and given
#' minimum, \code{wmin}, and maximum, \code{wmax}, wetness duration requirements.
#'
#' @param x a scalar, vector or matrix of temperatures at which to estimate 
#' response function
#' @param wmin,wmax scalar or vector minimum and maximum wetness duration 
#' requirements
#' @param tmin,tmax,topt scalar or vector minimum, maximum and optimum 
#' temperatures for infection, respectively; see Details
#' 
#' @details
#' 
#' Calls \code{tempresp}. Note that \code{wmin} and \code{wmax} must be compatible
#' with and \code{topt} are scalar and \code{nrow(t)} > 1,
#' then their values are repeated \code{ncol{t}}. \code{tmin}, \code{tmax} and 
#' \code{topt} must be the same length. If \code{length(tmin)} > 1 then 
#' \code{length(tmin)} must equal \code{ncol{t}}, or, if \code{ncol{t}} = 1,
#' then columns are \code{t} are repeated \code{length(tmin)} times.
#' 
#' @references 
#' 
#' Magarey, R. D., T. B. Sutton, and C. L. Thayer. (2005). A simple generic 
#' infection model for foliar fungal plant pathogens. Phytopathology 95 (1): 
#' 92–100. \doi{10.1094/PHYTO-95-0092}
#'
#' @examples
#'
#' # phytophthora cactorum (apple fruit)
#' reqwetdur(0:40, 2, 5, 1, 35, 25)
#'
#' @return A scalar, vector or matrix
#' 
#' @seealso
#' 
#' \link{tempresp}
#' 
#' @export
#' 
reqwetdur <- function(x, wmin, wmax, tmin, tmax, topt) {
  ft <- tempresp(x, tmin, tmax, topt)
  nv <- nrow(ft)
  ns <- ncol(ft)
  lt <- c(length(wmin), length(wmax), length(tmin), length(tmin), length(topt))
  if (length(unique(lt)) != 1)
    stop('lengths of wmin and wmax must be the same.')
  ft[ft == 0] <- NA
  out <- matrix(wmin, nv, ns, byrow = TRUE) / ft
  out[out > matrix(wmax, nv, ns, byrow = TRUE)] <- NA
  out
}  
