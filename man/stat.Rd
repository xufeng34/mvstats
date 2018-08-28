\name{stat}
\alias{stat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
stat
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stat(x)
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
    if (all(is.na(x))) {
        stop("All elements of ", substitute(x), " have a missing value")
    }
    if (is.vector(x)) {
        S = cbind(n = length(x), mean = mean(x), sd = sd(x), 
            min = min(x), max = max(x))
    }
    else {
        S = cbind(n = nrow(x), mean = apply(x, 2, mean), sd = apply(x, 
            2, sd), min = apply(x, 2, min), max = apply(x, 2, 
            max))
    }
    print(round(S, 4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
