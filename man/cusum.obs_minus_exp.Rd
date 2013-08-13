\name{cusum.obs_minus_exp}
\alias{cusum.obs_minus_exp}
\title{Creats a cumulative observed minus expected failure plot}
\usage{
  cusum.obs_minus_exp(failure_indicator, p0, by = NULL,
    scale_ylim = 20)
}
\arguments{
  \item{failure_indicator}{a numeric indicator variable
  consiting of only \code{c(0,1)}, where 0 is no failure
  and 1 is failure for each procedure}

  \item{p0}{either a constant which represents the
  acceptable event rate, or a numeric vector representing
  the acceptable risk score for each single individual. The
  later is used when plotting risk audjusted scores. This
  is then equal to an VLAD (variable life adjusted
  displays) or CRAM (cumulative risk adjusted mortality)
  chart}

  \item{by}{a factor vector consisting of the
  stratification variable.}

  \item{scale_ylim}{Limits the Y axis scale}
}
\value{
  an object of the class \code{ggplot}
}
\description{
  Implementation of an unadjusted or risk-adjusted
  cumulative observed minus expected failure graph as
  described in Rogers et al. (2004)
}
\details{
  "The graph starts at 0, but is incremented by 1 - p0 for
  a failure and decremented by p0 for a success"
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
cp= cusum.obs_minus_exp(rbinom(200,1,0.10), c(rnorm(100, 0.10, 0.03),rnorm(100, 0.10, 0.03)))
print(cp)
cp= cusum.obs_minus_exp(rbinom(200,1,0.10), 0.10)
print(cp)
cp= cusum.obs_minus_exp(df$is_failure, 0.10, by=df$by)
print(cp)
cp= cusum.obs_minus_exp(df$is_failure, df$p0, by=df$by)
print(cp)
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
  \code{\link{cusum.sprt}}
}

