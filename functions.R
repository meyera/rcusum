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
           rnorm(50, 0.10, 0.03),rnorm(50, 0.10, 0.03),rnorm(50, 0.35, 0.03)
          ),
    by=rep(factor(c("Surgeon A", "Surgeon B", "Surgeon C")), times=c(150,150,150))
  )

## Implementation of a cumulative observed minus expected failure graph as described in
## Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation Chris. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811–819. doi:10.1016/j.jtcvs.2004.03.011 
## @failure_indicator indicator variable consiting of c(0,1), where 0 is no failure and 1 is failure for each procedure
## @p0 either a constant which represents then the acceptable event rate, or a numeric vector representing the acceptable risk score for each single individual. The later is used when plotting risk audjusted scores. This is then equal to an VLAD (variable life adjusted displays) or CRAM (cumulative risk adjusted mortality) chart 
##
## "The graph starts at 0, but is incremented by 1 - p0 for a failure and decremented by p0 for a success"
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
    p = ggplot(d, aes(x=n,y=failure_line))
  } else {
    p = ggplot(d, aes(x=n,y=failure_line, linetype=by))
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
cp= cusum.obs_minus_exp(df$is_failure, df$p0, by=df$by)
print(cp)

## Implementation of a cusum chart and cusum log-likelihood chart with control limits as described in
## Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S., Bonser, R. S., & Angelini, G. D. (2004). Control chart methods for monitoring cardiac surgical performance and their interpretation Chris. The Journal of Thoracic and Cardiovascular Surgery, 128(6), 811–819. doi:10.1016/j.jtcvs.2004.03.011 
## in Appendix A
## @failure_indicator indicator variable consiting of c(0,1), where 0 is no failure and 1 is failure for each procedure
## @p0 the acceptable event rate
## @p1 the unacceptable event rate we want to detect
cusum = function(failure_indicator, p0, p1, alpha=.05, beta=.05, by=NULL, loglike_chart=FALSE) {
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
      p = ggplot(data=d_, aes(y=T_horizontal,x=n))
    } else {
      p = ggplot(data=d_, aes(y=T_horizontal,x=n, linetype=by))
    }
    
    p = p + geom_step() 
    p = p + geom_hline(yintercept=-h0, linetype=2)
    p = p + geom_hline(yintercept=h1, linetype=2)
    p = p + geom_hline(yintercept=0, color="lightgrey")
    
    p = p + ylab("Cumulative log-likelihood ratio")
    
    p <- p + annotate("text", label=paste("Accept H1"), x=-Inf, y=h1*2, hjust=-0.5)#, x=n/5, y=h1*2)
    p <- p + annotate("text", label=paste("Accept H0"), x=-Inf, y=-h0*2, hjust=-0.5)#, x=n/5, y=-h0*2)
  } else {
    
    if (is.null(by)) {
      p = ggplot(data=d ,aes(y=cusum,x=n))
    } else {
      p = ggplot(data=d ,aes(y=cusum,x=n, linetype=by))
    }
    
    p = p + geom_step() 
    p = p + geom_line(mapping=aes(x=n,y=l0),linetype=2)
    p = p + geom_line(mapping=aes(x=n,y=l1),linetype=2)
    
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