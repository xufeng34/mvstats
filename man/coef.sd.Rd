\name{coef.sd}
\alias{coef.sd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
coef.sd
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coef.sd(fm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fm}{
%%     ~~Describe \code{fm} here~~
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
function (fm) 
{
    b = fm$coeff
    p = length(b)
    si = apply(fm$model[, 2:p], 2, sd)
    sy = sd(fm$model[, 1])
    b1 = b[2:p] * (si/sy)
    list(coef.sd = b1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
