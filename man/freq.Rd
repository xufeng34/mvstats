\name{freq}
\alias{freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
freq
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freq(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
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
function (X) 
{
    vars = names(X)
    X = as.matrix(X)
    p = ncol(X)
    tab <- function(Y) {
        T1 = table(Y)
        Percent = T1/sum(T1) * 100
        T2 = cbind(Freqency = T1, Percent, Cum.percent = cumsum(Percent))
        Total = c(sum(T1), 100, 100)
        print(rbind(T2, Total))
    }
    if (p == 1) {
        cat("\n")
        tab(X)
    }
    else {
        for (i in 1:p) {
            cat("\n", vars[i], "\n")
            tab(X[, i])
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
