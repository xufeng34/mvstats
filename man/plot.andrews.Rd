\name{plot.andrews}
\alias{plot.andrews}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.andrews
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.andrews(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    if (is.data.frame(x) == TRUE) 
        x <- as.matrix(x)
    t <- seq(-pi, pi, pi/30)
    m <- nrow(x)
    n <- ncol(x)
    f <- array(0, c(m, length(t)))
    for (i in 1:m) {
        f[i, ] <- x[i, 1]/sqrt(2)
        for (j in 2:n) {
            if (j\%\%2 == 0) 
                f[i, ] <- f[i, ] + x[i, j] * sin(j/2 * t)
            else f[i, ] <- f[i, ] + x[i, j] * cos(j\%/\%2 * t)
        }
    }
    plot(c(-pi, pi), c(min(f), max(f)), type = "n", xlab = "", 
        ylab = "")
    for (i in 1:m) lines(t, f[i, ], col = i)
    legend(2, max(f), rownames(x), col = 1:nrow(x), lty = 1:nrow(x), 
        bty = "n", cex = 0.8)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
