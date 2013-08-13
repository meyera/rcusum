\name{cusum.sprt}
\alias{cusum.sprt}
\title{Creats a risk-adjusted sequential probability ratio test (SPRT) chart}
\usage{
  cusum.sprt(failure_indicator, p0, OR, alpha = 0.01,
    beta = 0.01, by = NULL)
}
\arguments{
  \item{failure_indicator}{a numeric indicator variable
  consiting of only \code{c(0,1)}, where 0 is no failure
  and 1 is failure for each procedure}

  \item{p0}{a numeric vector representing the acceptable
  risk score/acceptable failure rate for each single
  individual. I.e. STS Score values, or emperically modeled
  risks}

  \item{OR}{the increase in relative risk to the modeled
  acceptable risk, where An odds ratio of 2, for example,
  would equate approximately to a doubling of
  patientspecific risk of failure, an odds ratio of 1.5 to
  a 50 percent increase in failure risk, and so on.}

  \item{alpha}{Type I error (the probability of concluding
  that the failure rate has increased, when in fact it has
  not)}

  \item{beta}{Type II error (the probability of concluding
  that the failure rate has not increased, when in fact it
  has)}

  \item{by}{a factor vector consisting of the
  stratification variable.}
}
\value{
  an object of the class \code{ggplot}
}
\description{
  Implementation of a risk-adjusted sequential probability
  ratio test (SPRT) chart with control limits as described
  in Rogers et al. (2004)
}
\details{
  For the unadjusted chart, increase in risk is defined in
  terms of the unacceptable failure rate. However, when
  risk for each patient varies, it does not make sense to
  have a common unacceptable rate applied across all
  operations. This variable unacceptable rate is achieved
  by defining the increase in terms of a relative risk (ie.
  odds ratio), rather than a specific rate.
}
\examples{
set.seed(16)
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

sprt1= cusum.sprt(rbinom(200,1,0.10), rnorm(200, 0.10, 0.03), 1.5)
print(sprt1)
sprt2= cusum.sprt(df$is_failure, df$p0, 1.5, by=df$by)
print(sprt2)
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
  Other cusum: \code{\link{cusum}},
  \code{\link{cusum.obs_minus_exp}}
}

