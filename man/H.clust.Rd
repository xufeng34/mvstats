\name{H.clust}
\alias{H.clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
H.clust
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
H.clust(X, d = "euc", m = "comp", proc = FALSE, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{proc}{
%%     ~~Describe \code{proc} here~~
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
function (X, d = "euc", m = "comp", proc = FALSE, plot = TRUE) 
{
    D = dist(X, d)
    hc <- hclust(D, m)
    PROC = cbind(merge = hc$merge, height = hc$height)
    if (proc) 
        print(PROC)
    if (plot) 
        plot(hc, ylab = d, main = m)
    return(hc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
