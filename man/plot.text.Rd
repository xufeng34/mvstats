\name{plot.text}
\alias{plot.text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.text
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.text(X, h = 0, v = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
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
function (X, h = 0, v = 0) 
{
    plot(X)
    abline(h = h, v = v, lty = 3)
    text(X, label = rownames(X), pos = 1.1, adj = 0.5, cex = 0.85)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
