library(ggplot2) 
set.seed(16)
common_theme = theme_bw() + theme(axis.line=element_line(colour="black")
                                  ,panel.grid.major=element_blank()
                                  ,legend.position="bottom"
                                  ,legend.title=element_blank()
                                  ,axis.text.x=element_text(angle=30, hjust=1)
                                  ,strip.background=element_rect(fill="grey96"))

df = data.frame(
  is_failure = c(rbinom(50,1,0.10),rbinom(50,1,0.08),rbinom(50,1,0.05),
                 rbinom(50,1,0.10),rbinom(50,1,0.13),rbinom(50,1,0.14),
                 rbinom(50,1,0.14),rbinom(50,1,0.09),rbinom(50,1,0.25)
  ),
  p0 = c(rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
         rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
         rnorm(50, 0.10, 0.03),rnorm(50, 0.15, 0.03),rnorm(50, 0.20, 0.03)
  ),
  by=rep(factor(c("Surgeon A", "Surgeon B", "Surgeon C")), times=c(150,150,150))
)

#' Creats a cumulative observed minus expected failure plot
#'
#' Implementation of an unadjusted or risk-adjusted cumulative observed minus expected failure graph as described in Rogers et al. (2004)
#' 
#' "The graph starts at 0, but is incremented by 1 - p0 for a failure and decremented by p0 for a success"
#' 
#' @param failure_indicator a numeric indicator variable consiting of only \code{c(0,1)}, where 0 is no failure and 1 is failure for each procedure
#' @param p0 either a constant which represents the acceptable event rate, or a numeric vector representing the acceptable risk score for each single individual. The later is used when plotting risk audjusted scores. This is then equal to an VLAD (variable life adjusted displays) or CRAM (cumulative risk adjusted mortality) chart 
#' @param by a factor vector consisting of the stratification variable.
#' @param scale_ylim Limits the Y axis scale
#' 
#' @return an object of the class \code{ggplot}
#' 
#' @author Alexander Meyer
#' @family cusum
#' @export
#' 
#' @references Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation Chris. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811–819. doi:10.1016/j.jtcvs.2004.03.011 
#' 
#' @examples
#' set.seed(16)
#' df = data.frame(
#'  is_failure = c(rbinom(50,1,0.10),rbinom(50,1,0.08),rbinom(50,1,0.05),
#'                  rbinom(50,1,0.10),rbinom(50,1,0.13),rbinom(50,1,0.14),
#'                  rbinom(50,1,0.14),rbinom(50,1,0.09),rbinom(50,1,0.25)
#'  ),
#'  p0 = c(rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.15, 0.03),rnorm(50, 0.20, 0.03)
#'  ),
#'  by=rep(factor(c("Surgeon A", "Surgeon B", "Surgeon C")), times=c(150,150,150))
#' )
#' cp= cusum.obs_minus_exp(rbinom(200,1,0.10), c(rnorm(100, 0.10, 0.03),rnorm(100, 0.10, 0.03)))
#' print(cp)
#' cp= cusum.obs_minus_exp(rbinom(200,1,0.10), 0.10)
#' print(cp)
#' cp= cusum.obs_minus_exp(df$is_failure, 0.10, by=df$by)
#' print(cp)
#' cp= cusum.obs_minus_exp(df$is_failure, df$p0, by=df$by)
#' print(cp)
cusum.obs_minus_exp = function(failure_indicator, p0, by=NULL, scale_ylim = 20) {
  require(ggplot2)
  require(plyr)
  
  stopifnot(is.numeric(failure_indicator))
  stopifnot(failure_indicator %in% c(0,1))
  stopifnot(is.numeric(p0), p0 >= 0 & p0 <= 1 & (length(p0) == length(failure_indicator) | length(p0) == 1))
  stopifnot(is.numeric(scale_ylim), length(scale_ylim) == 1)
  if (!is.null(by)) {
    stopifnot(is.factor(by))
    stopifnot(length(failure_indicator) == length(by))
  }
  
  d = NULL
  if (is.null(by)) {
    failure_line = c()
    for (i in 1:length(failure_indicator)) {
      before = ifelse(i == 1, 0, failure_line[i-1])
      
      p0_ = ifelse(length(p0) == 1, p0, p0[i])
      
      if (failure_indicator[i] == 0) {
        failure_line[i] = before - p0_
      } else if (failure_indicator[i] == 1) {
        failure_line[i] = before + (1-p0_)
      }
    }
    
    d = data.frame(n=1:length(failure_line),
                    failure_line=failure_line)
  } else {
    if (length(p0) == 1) {
      p0_ = rep(p0,times=length(failure_indicator))
    } else {
      p0_ = p0
    }
    
    d = data.frame(
      failure_indicator=failure_indicator,
      by=by,
      p0=p0_
    )
    
    d = ddply(d, .(by), function(sub_d) {  
            failure_line = c()
            for (i in 1:length(sub_d$failure_indicator)) {
              before = ifelse(i == 1, 0, failure_line[i-1])
              
              if (sub_d$failure_indicator[i] == 0) {
                failure_line[i] = before - sub_d$p0[i]
              } else if (sub_d$failure_indicator[i] == 1) {
                failure_line[i] = before + (1-sub_d$p0[i])
              }
            }
            
            data.frame(n=1:length(failure_line),
                       failure_line=failure_line,
                       by=sub_d$by)
          }
        )    
  }
  
  if (is.null(by)) {
    p = ggplot(d, aes_string(x="n",y="failure_line"))
  } else {
    p = ggplot(d, aes_string(x="n",y="failure_line", color="by"))
  }
  p = p + geom_hline(yintercept=0, linetype=6, size=1)
  p = p + geom_step()
  
  p = p + ylim(-scale_ylim, scale_ylim)
  p = p + ylab("Cumulative observed minus expected failures")
  p = p + xlab("")
  p <- p + common_theme
  return(p)
}
#cp= cusum.obs_minus_exp(rbinom(200,1,0.10), c(rnorm(100, 0.10, 0.03),rnorm(100, 0.10, 0.03)))
#cp= cusum.obs_minus_exp(rbinom(200,1,0.10), 0.10)
#cp= cusum.obs_minus_exp(df$is_failure, 0.10, by=df$by)
#cp= cusum.obs_minus_exp(df$is_failure, df$p0, by=df$by)
#print(cp + ggthemes::theme_few())


#' Creats a cusum chart and cusum log-likelihood chart
#'
#' Implementation of an unadjusted cusum chart and cusum log-likelihood chart as described in Rogers et al. (2004)
#' 
#' @param failure_indicator a numeric indicator variable consiting of only \code{c(0,1)}, where 0 is no failure and 1 is failure for each procedure
#' @param p0 a constant representing the fixed acceptable event rate when the process is in control
#' @param p1 a constant representing the fixed unacceptable event rate we want to detect
#' @param alpha Type I error (the probability of concluding that the failure rate has increased, when in fact it has not)
#' @param beta Type II error (the probability of concluding that the failure rate has not increased, when in fact it has)
#' @param by a factor vector consisting of the stratification variable.
#' @param loglike_chart a flag controling which kind of chart will be shown
#'
#' @return an object of the class \code{ggplot}
#' 
#' @author Alexander Meyer
#' @family cusum
#' @export
#' 
#' @references Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation Chris. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811–819. doi:10.1016/j.jtcvs.2004.03.011 
#' 
#' @examples
#' set.seed(16)
#' df = data.frame(
#'  is_failure = c(rbinom(50,1,0.10),rbinom(50,1,0.08),rbinom(50,1,0.05),
#'                  rbinom(50,1,0.10),rbinom(50,1,0.13),rbinom(50,1,0.14),
#'                  rbinom(50,1,0.14),rbinom(50,1,0.09),rbinom(50,1,0.25)
#'  ),
#'  p0 = c(rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.15, 0.03),rnorm(50, 0.20, 0.03)
#'  ),
#'  by=rep(factor(c("Surgeon A", "Surgeon B", "Surgeon C")), times=c(150,150,150))
#' )
#' cusum_plot = cusum(df$is_failure, .10, .20, alpha=0.01,beta=0.01, loglike_chart=TRUE, by=df$by)
#' print(cusum_plot)
#' cusum_plot = cusum(df$is_failure, .10, .20, alpha=0.01,beta=0.01, loglike_chart=FALSE, by=df$by)
#' print(cusum_plot)
cusum = function(failure_indicator, p0, p1, alpha=.01, beta=.01, by=NULL, loglike_chart=FALSE) {
  require(plyr)
  require(ggplot2)
  
  stopifnot(is.numeric(failure_indicator))
  stopifnot(failure_indicator %in% c(0,1))
  stopifnot(is.numeric(p0), p0 >= 0, p0 <= 1)
  stopifnot(is.numeric(p1), p1 >= 0, p1 <= 1)
  stopifnot(is.numeric(alpha), alpha >= 0, alpha <= 1)
  stopifnot(is.numeric(beta), beta >= 0, beta <= 1)
  if (!is.null(by)) {
    stopifnot(is.factor(by))
    stopifnot(length(failure_indicator) == length(by))
  }
    
  OR = (p1*(1-p0))/(p0*(1-p1))
  s  = log((1-p0)/(1-p1))/log(OR)
  h0 = log((1-alpha)/beta)/log(OR)
  h1 = log((1-beta)/alpha)/log(OR)
  n = length(failure_indicator)
  
  if (is.null(by)) {
    d = data.frame(
      n=1:n,
      failure=failure_indicator,
      cusum=cumsum(failure_indicator),
      l0=((1:n)*s)-h0,
      l1=((1:n)*s)+h1,
      T_horizontal = cumsum(failure_indicator - s)
    )  
  } else {
    d = data.frame(
      failure=failure_indicator,
      by=by
    )
    
    d = ddply(d, .(by), function(sub_d) {      
      data.frame(
        n=1:nrow(sub_d),
        failure=sub_d$failure,
        cusum=cumsum(sub_d$failure),
        l0=((1:nrow(sub_d))*s)-h0,
        l1=((1:nrow(sub_d))*s)+h1,
        T_horizontal = cumsum(sub_d$failure - s),
        by=sub_d$by
      )
    })
  }
  
  p = NULL
  if (loglike_chart) {
    d_ = cbind(d, h0=rep(h0,times=nrow(d)), h1=rep(h1, times=nrow(d)))

    if (is.null(by)) {
      p = ggplot(data=d_, aes_string(y="T_horizontal",x="n"))
    } else {
      p = ggplot(data=d_, aes_string(y="T_horizontal",x="n", color="by"))
    }
    
    p = p + geom_step() 
    p = p + geom_hline(yintercept=-h0, linetype=2, color="black")
    p = p + geom_hline(yintercept=h1, linetype=2, color="black")
    p = p + geom_hline(yintercept=0, color="lightgrey")
    
    p = p + ylab("Cumulative log-likelihood ratio")
    
    p <- p + annotate("text", label=paste("Accept H1"), x=-Inf, y=h1*2, hjust=-0.5)#, x=n/5, y=h1*2)
    p <- p + annotate("text", label=paste("Accept H0"), x=-Inf, y=-h0*2, hjust=-0.5)#, x=n/5, y=-h0*2)
  } else {
    
    if (is.null(by)) {
      p = ggplot(data=d ,aes(y=cusum,x=n))
    } else {
      p = ggplot(data=d ,aes(y=cusum,x=n, color=by))
    }
    
    p = p + geom_step() 
    p = p + geom_line(mapping=aes_string(x="n",y="l0"),linetype=2, color="black")
    p = p + geom_line(mapping=aes_string(x="n",y="l1"),linetype=2, color="black")
    
    p <- p + annotate("text", label=paste("Accept H1"), x=-Inf, y=Inf, vjust=1.3, hjust=-0.3)#x=n/3, y=(n/2)*p1*1.0)
    p <- p + annotate("text", label=paste("Accept H0"), x=Inf, y=-Inf, vjust=-1.2, hjust=1.3)#x=n/1.5, y=(n/2)*(p0*1.0))
  }
  
  p <- p + common_theme
  p = p + xlab("")
  
#   if (!is.null(by)) {
#     p <- p + facet_wrap(~by, as.table=TRUE)
#   }
  
  return(p)
}
#cusum_plot = cusum(df$is_failure, .10, .20, alpha=0.01,beta=0.01, loglike_chart=TRUE, by=df$by)
#print(cusum_plot)


#' Creats a risk-adjusted sequential probability ratio test (SPRT) chart
#'
#' Implementation of a risk-adjusted sequential probability ratio test (SPRT) chart with control limits as described in Rogers et al. (2004)
#' 
#' For the unadjusted chart, increase in risk is defined in terms of the unacceptable failure rate. However, when risk for each patient varies, it does not make sense to have a common unacceptable rate applied across all operations.
#' This variable unacceptable rate is achieved by defining the increase in terms of a relative risk (ie. odds ratio), rather than a specific rate.
#' 
#' @param failure_indicator a numeric indicator variable consiting of only \code{c(0,1)}, where 0 is no failure and 1 is failure for each procedure
#' @param p0 a numeric vector representing the acceptable risk score/acceptable failure rate for each single individual. I.e. STS Score values, or emperically modeled risks
#' @param OR the increase in relative risk to the modeled acceptable risk, where An odds ratio of 2, for example, would equate approximately to a doubling of patientspecific risk of failure, an odds ratio of 1.5 to a 50 percent increase in failure risk, and so on.
#' @param alpha Type I error (the probability of concluding that the failure rate has increased, when in fact it has not)
#' @param beta Type II error (the probability of concluding that the failure rate has not increased, when in fact it has)
#' @param by a factor vector consisting of the stratification variable.
#'
#' @return an object of the class \code{ggplot}
#' 
#' @author Alexander Meyer
#' @family cusum
#' @export
#' 
#' @references Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation Chris. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811–819. doi:10.1016/j.jtcvs.2004.03.011 
#' 
#' @examples
#' set.seed(16)
#' df = data.frame(
#'  is_failure = c(rbinom(50,1,0.10),rbinom(50,1,0.08),rbinom(50,1,0.05),
#'                  rbinom(50,1,0.10),rbinom(50,1,0.13),rbinom(50,1,0.14),
#'                  rbinom(50,1,0.14),rbinom(50,1,0.09),rbinom(50,1,0.25)
#'  ),
#'  p0 = c(rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),
#'          rnorm(50, 0.10, 0.03),rnorm(50, 0.15, 0.03),rnorm(50, 0.20, 0.03)
#'  ),
#'  by=rep(factor(c("Surgeon A", "Surgeon B", "Surgeon C")), times=c(150,150,150))
#' )
#' 
#' sprt1= cusum.sprt(rbinom(200,1,0.10), rnorm(200, 0.10, 0.03), 1.5)
#' print(sprt1)
#' sprt2= cusum.sprt(df$is_failure, df$p0, 1.5, by=df$by)
#' print(sprt2)
cusum.sprt = function(failure_indicator, p0, OR, alpha=.01, beta=.01, by=NULL) {
  require(plyr)
  require(ggplot2)
  
  stopifnot(is.numeric(failure_indicator))
  stopifnot(failure_indicator %in% c(0,1))
  stopifnot(is.numeric(p0), p0 >= 0 & p0 <= 1 & length(p0) == length(failure_indicator))
  stopifnot(is.numeric(OR), OR >= 1)
  stopifnot(is.numeric(alpha), alpha >= 0, alpha <= 1)
  stopifnot(is.numeric(beta), beta >= 0, beta <= 1)
  if (!is.null(by)) {
    stopifnot(is.factor(by))
    stopifnot(length(failure_indicator) == length(by))
  }
  
  h0 = log((1-alpha)/beta)/log(OR)
  h1 = log((1-beta)/alpha)/log(OR)
  n = length(failure_indicator)
  
  if (is.null(by)) {
    d = data.frame(
      n=1:n,
      failure=failure_indicator,
      T_horizontal = cumsum(failure_indicator - (log((1-p0)+(OR*p0))/log(OR)))
    )  
  } else {
    d = data.frame(
      failure=failure_indicator,
      p0=p0,
      by=by
    )
    
    d = ddply(d, .(by), function(sub_d) {      
      data.frame(
        n=1:nrow(sub_d),
        failure=sub_d$failure,
        T_horizontal = cumsum(sub_d$failure - (log((1-sub_d$p0)+(OR*sub_d$p0))/log(OR))),
        by=sub_d$by
      )
    })
  }
  
  d_ = cbind(d, h0=rep(h0,times=nrow(d)), h1=rep(h1, times=nrow(d)))
  
  if (is.null(by)) {
    p = ggplot(data=d_, aes_string(y="T_horizontal",x="n"))
  } else {
    p = ggplot(data=d_, aes_string(y="T_horizontal",x="n", color="by"))
  }
  
  p = p + geom_step() 
  p = p + geom_hline(yintercept=-h0, linetype=2, color="black")
  p = p + geom_hline(yintercept=h1, linetype=2, color="black")
  p = p + geom_hline(yintercept=0, color="lightgrey")
  
  p = p + ylab("Cumulative log-likelihood ratio")
  
  p <- p + annotate("text", label=paste("Accept H1"), x=-Inf, y=h1*2, hjust=-0.5)#, x=n/5, y=h1*2)
  p <- p + annotate("text", label=paste("Accept H0"), x=-Inf, y=-h0*2, hjust=-0.5)#, x=n/5, y=-h0*2)
  
  p <- p + common_theme
  p = p + xlab("")
  
  #   if (!is.null(by)) {
  #     p <- p + facet_wrap(~by, as.table=TRUE)
  #   }
  
  return(p)
}
#cp= cusum.obs_minus_exp(rbinom(200,1,0.10), c(rnorm(100, 0.10, 0.03),rnorm(100, 0.10, 0.03)))
#sprt1= cusum.sprt(rbinom(200,1,0.10), rnorm(200, 0.10, 0.03), 1.5)
#sprt2= cusum.sprt(df$is_failure, df$p0, 1.5, by=df$by)
#print(sprt2)