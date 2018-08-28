\name{factanal.rank}
\alias{factanal.rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
factanal.rank
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
factanal.rank(Fac, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Fac}{
%%     ~~Describe \code{Fac} here~~
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
function (Fac, plot = FALSE) 
{
    Fs = Fac$scores
    W = apply(Fac$loadings^2, 2, sum)
    Wi = W/sum(W)
    F = Fs \%*\% Wi
    Ri = data.frame(F = F, rank = rank(-F))
    if (plot) {
        plot(Fs)
        abline(h = 0, v = 0, lty = 3)
        text(Fs, label = rownames(Fs), pos = 1.1, adj = 0.5, 
            cex = 0.85)
    }
    list(Fs = Fs, Ri = Ri)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
