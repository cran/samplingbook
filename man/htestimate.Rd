\name{htestimate}
\alias{htestimate}
\title{Horvitz-Thompson Estimator}
\description{Calculates Horvitz-Thompson estimate with different methods for variance estimation such as Yates and Grundy, Hansen-Hurwitz and Hajek.}
\usage{
htestimate(y, N, PI, pk, pik, method = 'yg')
}
\arguments{
  \item{y}{vector of observations}
  \item{N}{integer for population size}     
  \item{PI}{square matrix of second order inclusion probabilities with \code{n} rows and cols. It is necessary for variance estimation by methods 'ht' and 'yg'.}
  \item{pk}{vector of first order inclusion probabilities of length \code{n} for the sample elements. It is necessary for variance estimation by methods 'hh' and 'ha'.}
  \item{pik}{vector of first order inclusion probabilities of length \code{N} for the population elements . It can be used for variance estimation by method 'ha'.}
  \item{method}{method for variance estimation. Options are 'yg' (Yates and Grundy) and 'ht' (Horvitz-Thompson), approximative options are 'hh' (Hansen-Hurwitz) and 'ha' (Hajek).}
}
\details{
For using methods \code{'yg'} or \code{'ht'} matrix \code{PI} has to be provided, for \code{'hh'} and \code{'ha'} vector \code{pik} of inclusion probabilities has to be specified. 
For Hajek method \code{'ha'} of variance estimation \code{pk} can be specified additionally. Unless an approximative Hajek method is used.
}
\value{
The function \code{htestimate} returns a value, which is a list consisting of the components
  \item{call}{is a list of call components: \code{y} vector of observations, \code{N} population size, \code{PI} inclusion probabilities, \code{pk} inclusion probabilities of sample, \code{pik} full inclusion probabilities and \code{method} method for variance estimation}
  \item{mean}{mean estimate}
  \item{se}{standard error of the mean estimate}
}
\references{
  Kauermann, Goeran/Kuechenhoff, Helmut (2010): Stichproben. Methoden und praktische Umsetzung mit R. Springer.
}
\author{Juliane Manitz}
\seealso{\code{\link{pps.sampling}}}
\examples{
data(influenza)
summary(influenza)

# pps.sampling()
set.seed(108506)
pps <- pps.sampling(z=influenza$population,n=20,method='midzuno')
sample <- influenza[pps$sample,]
# htestimate()
N <- nrow(influenza)
# exact variance estimate
PI <- pps$PI
htestimate(sample$cases, N=N, PI=PI, method='yg')
htestimate(sample$cases, N=N, PI=PI, method='ht')
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
