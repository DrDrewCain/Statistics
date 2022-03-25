
#' PRESS for a given smoother
#'
#' @param x Predictor vector
#' @param y Observed vector
#' @param smoother Function of smoother
#' @param parm Vector of smoother parameter
#'
#' @return A value of PRESS
#' @export
#'
#' @examples
press <- function(x, y, smoother, parm) {
  if(length(parm)>1) {
    lambda <- parm[1]
    nnn <- parm[2]
    mod <- smoother(x, y, lambda, nnn)
  }else {
    lambda <- parm
    mod <- smoother(x, y, parm) 
  }
  stud_e <- as.vector(mod$resid)/(1-diag(mod$smat))
  #stud_e[!is.finite(stud_e)] <- 0
  PRESS <- sum(stud_e^2)
  return(PRESS)
}

#' Greedy random search to optimize smoother parameters
#'
#' @param x Predictor vector
#' @param y Observed vector
#' @param smoother Function of smoother
#' @param parm Vector of smoother parameter
#' @param max_iter Maximum number of iteration (default set to 1000)
#'
#' @return A list of adjustable parameters and corresponding PRESS 
#' @export
#'
#' @examples
adj_parm <- function(x, y, smoother, parm, max_iter = 1000) {
  press_old <- press(x, y, smoother, parm)
  iter <- 1
  while (iter < max_iter) {
    inc1 <- rnorm(1,0,0.01)
    inc2 <- round(rnorm(1))
    int_pram <- round(abs(parm+inc2))
    dec_pram <- round(abs(parm+inc1)+0.001,3)
    parm_new <- ifelse(parm>1, int_pram, dec_pram)
    press_new <- press(x, y, smoother, parm_new)
    if (press_new<press_old) {
      press_old <- press_new
      parm <- parm_new
    }else{
      press_old <- press_old
      parm <- parm 
    }
    iter <- iter+1
  }
  output <- list(parm,press_old)
  names(output) <- c("Adjustable_parameters", "PRESS")
  return(output)
  }

NOAA1 <- read.csv("NOAANew_csv")
adj_bin_mean <- adj_parm(NOAA1[,3], NOAA1[,2], bin.mean, 6)
adj_gauss_mean <- adj_parm(NOAA1[,3], NOAA1[,2], gauss.mean, 0.063)
adj_gauss_reg <- adj_parm(NOAA1[,3], NOAA1[,2], gauss.reg, 0.078)
adj_gauss_mean_trunc <- adj_parm(NOAA1[,3], NOAA1[,2], gauss.mean.trunc, c(0.063,20))
adj_gauss_reg_trunc <- adj_parm(NOAA1[,3], NOAA1[,2], gauss.reg.trunc, c(0.08,17))


plot(NOAA1[,3], NOAA1[,2],
     xlab = "temperature rise", 
     ylab="rate of billion dollar weather disasters")

bin.mean(NOAA1[,3],NOAA1[,2],
         adj_bin_mean$Adjustable_parameters,do.plot=T)
gauss.mean(NOAA1[,3],NOAA1[,2],
           adj_gauss_mean$Adjustable_parameters,do.plot=T)
gauss.reg(NOAA1[,3],NOAA1[,2],
          adj_gauss_reg$Adjustable_parameters[1],do.plot=T)
gauss.mean.trunc(NOAA1[,3],NOAA1[,2],
                 adj_gauss_mean_trunc$Adjustable_parameters[1],
                 adj_gauss_mean_trunc$Adjustable_parameters[2],do.plot=F)
gauss.reg.trunc(NOAA1[,3],NOAA1[,2],
                adj_gauss_reg_trunc$Adjustable_parameters[1],
                adj_gauss_reg_trunc$Adjustable_parameters[2],do.plot=T)
lines(lowess(NOAA1[,3],NOAA1[,2]),col=7)
lines(smooth.spline(NOAA1[,3],NOAA1[,2]),col=8)
