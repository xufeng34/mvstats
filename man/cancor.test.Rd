\name{cancor.test}
\alias{cancor.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cancor.test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cancor.test(x, y, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, plot = FALSE) 
{
    x = scale(x)
    y = scale(y)
    n = nrow(x)
    p = ncol(x)
    q = ncol(y)
    ca = cancor(x, y)
    cat("\n")
    print(ca)
    r = ca$cor
    m <- length(r)
    Q <- rep(0, m)
    P = rep(0, m)
    lambda <- 1
    for (k in m:1) {
        lambda <- lambda * (1 - r[k]^2)
        Q[k] <- -log(lambda)
    }
    s <- 0
    i <- m
    for (k in 1:m) {
        Q[k] <- (n - k + 1 - 1/2 * (p + q + 3) + s) * Q[k]
        P[k] <- 1 - pchisq(Q[k], (p - k + 1) * (q - k + 1))
    }
    cat("cancor test: \n")
    print(cbind(r, Q, P))
    if (plot) {
        u = as.matrix(x) \%*\% ca$xcoef
        v = as.matrix(y) \%*\% ca$ycoef
        plot(u[, 1], v[, 1], xlab = "u1", ylab = "v1")
        abline(lm(u[, 1] ~ v[, 1]))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
