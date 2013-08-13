\name{cusum}
\alias{cusum}
\title{Creats a cusum chart and cusum log-likelihood chart}
\usage{
  cusum(failure_indicator, p0, p1, alpha = 0.01,
    beta = 0.01, by = NULL, loglike_chart = FALSE)
}
\arguments{
  \item{failure_indicator}{a numeric indicator variable
  consiting of only \code{c(0,1)}, where 0 is no failure
  and 1 is failure for each procedure}

  \item{p0}{a constant representing the fixed acceptable
  event rate when the process is in control}

  \item{p1}{a constant representing the fixed unacceptable
  event rate we want to detect}

  \item{alpha}{Type I error (the probability of concluding
  that the failure rate has increased, when in fact it has
  not)}

  \item{beta}{Type II error (the probability of concluding
  that the failure rate has not increased, when in fact it
  has)}

  \item{by}{a factor vector consisting of the
  stratification variable.}

  \item{loglike_chart}{a flag controling which kind of
  chart will be shown}
}
\value{
  an object of the class \code{ggplot}
}
\description{
  Implementation of an unadjusted cusum chart and cusum
  log-likelihood chart as described in Rogers et al. (2004)
}
\examples{
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
cusum_plot = cusum(df$is_failure, .10, .20, alpha=0.01,beta=0.01, loglike_chart=TRUE, by=df$by)
print(cusum_plot)
cusum_plot = cusum(df$is_failure, .10, .20, alpha=0.01,beta=0.01, loglike_chart=FALSE, by=df$by)
print(cusum_plot)
}
\author{
  Alexander Meyer
}
\references{
  Rogers, C. A., Reeves, B. C., Caputo, M., Ganesh, J. S.,
  Bonser, R. S., & Angelini, G. D. (2004). Control chart
  methods for monitoring cardiac surgical performance and
  their interpretation Chris. The Journal of Thoracic and
  Cardiovascular Surgery, 128(6), 811–819.
  doi:10.1016/j.jtcvs.2004.03.011
}
\seealso{
  Other cusum: \code{\link{cusum.obs_minus_exp}},
  \code{\link{cusum.sprt}}
}

