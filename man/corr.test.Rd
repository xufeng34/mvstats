\name{corr.test}
\alias{corr.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
corr.test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corr.test(X, diag = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{diag}{
%%     ~~Describe \code{diag} here~~
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
function (X, diag = TRUE) 
{
    options(digits = 4)
    p = ncol(X)
    if (diag) {
        tp = matrix(1, p, p)
        for (i in 1:p) {
            for (j in 1:i) tp[i, j] = cor.test(X[, i], X[, j])$stat
            for (j in i:p) tp[i, j] = cor.test(X[, i], X[, j])$p.value
        }
        cat("corr test: \n")
        tp = round(matrix(tp, p, dimnames = list(names(X), names(X))), 
            4)
        print(tp)
        cat("lower is t value£¬upper is p value \n")
    }
    else {
        cat("\n corr test: t value, p value \n")
        if (is.matrix(X)) 
            var = 1:p
        else var = names(X)
        for (i in 1:(p - 1)) {
            for (j in (i + 1):p) cat(" ", var[i], "-", var[j], 
                cor.test(X[, i], X[, j])$stat, cor.test(X[, i], 
                  X[, j])$p.value, "\n")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
