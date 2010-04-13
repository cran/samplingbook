\name{Smean}
\alias{Smean}
\title{Sampling Mean Estimation}
\description{
 The function \code{Smean} estimates the population mean out of simple samples optional with consideration of finite population correction.
}
\usage{
Smean(y, N = Inf, level = 0.95)
}
\arguments{
  \item{y}{vector with sample data}
  \item{N}{integer for population size. Default is \code{N=Inf}, which means that calculations are carried out without finite population correction}
  \item{level}{coverage probability for confidence intervals. Default is \code{level=0.95}}
}
\value{
The function \code{Smean} returns a value, which is a list consisting of the components
  \item{call}{is a list of call components: \code{y} vector with sample data, \code{n} size of sample, \code{N} size of population, \code{level} coverage probability for confidence intervals}
  \item{mean}{mean estimate}
  \item{se}{standard error of the mean estimate}
  \item{ci}{vector of confidence interval boundaries}
}

\references{
  Kauermann, Goeran/Kuechenhoff, Helmut (2010): Stichproben. Methoden und praktische Umsetzung mit R. Springer.
}
\author{Juliane Manitz}
\seealso{\code{\link{Sprop}}, \code{\link{sample.size.mean}}}
\examples{
data(pop)
Y <- pop$Y
Y
# Draw a random sample with size=3
set.seed(93456)
y <- sample(x = Y, size = 3)
sort(y)
# Estimation with infiniteness correction
est <- Smean(y = y, N = length(pop$Y))
est
}
