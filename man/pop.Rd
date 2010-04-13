\name{pop}
\alias{pop}
\docType{data}
\title{Small Sampling Example}
\description{
\code{pop} is a suppositious data frame for a small population with 5 elements. It is used to illustrate survey sampling
estimators.
}
\usage{data(pop)}
\format{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{a numeric vector, individual id}
    \item{\code{X}}{a numeric vector, first characteristic}
    \item{\code{Y}}{a numeric vector, second characteristic}
  }
}
\references{
  Kauermann, Goeran/Kuechenhoff, Helmut (2010): Stichproben. Methoden und praktische Umsetzung mit R. Springer.
}
\examples{
data(pop)
print(pop)

## 1) Usage of Smean()
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

## 2) Usage of mbes()
data(pop)
# Draw a random sample of size=3
set.seed(802016)
data <- pop[sample(1:5, size=3),]
names(data) <- c('id','x','y')
# difference estimator
mbes(formula=y~x, data=data, aux=15, N=5, method='diff', level=0.95)
# ratio estimator
mbes(formula=y~x, data=data, aux=15, N=5, method='ratio', level=0.95)
# regression estimator
mbes(formula=y~x, data=data, aux=15, N=5, method='regr', level=0.95)
}
\keyword{datasets}
