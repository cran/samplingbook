\name{influenza}
\Rdversion{1.1}
\alias{influenza}
\docType{data}
\title{
Population and Cases of Influenza for Administrative Districts of Germany
}
\description{
The data frame \code{influenza} provides cases of influenza and inhabitants for administrative districts of Germany in 2007.
}
\usage{data(influenza)}
\format{
  A data frame with 424 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{district}}{a factor with levels \code{LK Aachen}, \code{LK Ahrweiler}, ..., \code{SK Zweibruecken}. Names of administrative districts in Germany.} 
    \item{\code{population}}{a numeric vector. Number of inhabitants in specific administrative district.}
    \item{\code{cases}}{a numeric vector. Number of influenza cases in specific administrative district.}
  }
}
\details{
Data of 2007. If you want to use this data in the future, be aware of local governmental reorganizations e.g. district unions.
}
\source{
Database SurvStat of Robert Koch-Institute. Many thanks to Hermann Claus.
}
\references{
Database of Robert Koch-Institute http://www3.rki.de/SurvStat/

Kauermann, Goeran/Kuechenhoff, Helmut (2010): Stichproben. Methoden und praktische Umsetzung mit R. Springer.
}
\examples{
data(influenza)
summary(influenza)

# 1) Usage of pps.sampling
set.seed(108506)
pps.sampling(z=influenza$population,n=20,method='tille')
set.seed(108506)
pps.sampling(z=influenza$population,n=20,method='madow')
set.seed(108506)
pps.sampling(z=influenza$population,n=20,method='midzuno')

# 2) Usage of htestimate
set.seed(108506)
pps <- pps.sampling(z=influenza$population,n=20,method='midzuno')
sample <- influenza[pps$sample,]
# htestimate()
N <- nrow(influenza)
# exact variance estimate
PI <- pps$PI
htestimate(sample$cases, N=N, PI=PI, method='ht')
htestimate(sample$cases, N=N, PI=PI, method='yg')
# approximate variance estimate
pk <- pps$pik[pps$sample]
htestimate(sample$cases, N=N, pk=pk, method='hh')
pik <- pps$pik
htestimate(sample$cases, N=N, pk=pk, pik=pik, method='ha')
htestimate(sample$cases, N=N, pk=pk, method='ha') # without pik just approximative calculation of Hajek method
# calculation of confidence interval based on normal distribution for number of cases
est.ht <- htestimate(sample$cases, N=N, PI=PI, method='ht')
est.ht$mean*N  # true number of cases is 18900
lower <- est.ht$mean*N - qnorm(0.975)*N*est.ht$se
upper <- est.ht$mean*N + qnorm(0.975)*N*est.ht$se
c(lower,upper) 
}
\keyword{datasets}
