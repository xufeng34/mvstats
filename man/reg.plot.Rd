\name{reg.plot}
\alias{reg.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reg.plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reg.plot(fm)
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
    p = ncol(fm$model)
    if (p == 2) {
        plot(fm$model[, 2], fm$model[, 1], xlab = names(fm$model[2]), 
            ylab = names(fm$model[1]))
        abline(fm, col = "red")
    }
    else {
        plot(rownames(fm$model), fm$model[, 1], type = "p", xlab = "i", 
            ylab = ".y,-y^")
        lines(rownames(fm$model), fm$fit)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
