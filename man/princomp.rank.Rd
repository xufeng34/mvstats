\name{princomp.rank}
\alias{princomp.rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
princomp.rank
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
princomp.rank(PCA, m, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PCA}{
%%     ~~Describe \code{PCA} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (PCA, m, plot = FALSE) 
{
    W = as.matrix(PCA[[1]]^2/sum(PCA[[1]]^2))
    PCs = as.matrix(PCA$scores[, 1:m])
    PC = PCs \%*\% W[1:m]/sum(W[1:m])
    ans = cbind(PCs, PC = PC[, 1], rank = rank(PC[, 1]))
    if (plot) {
        plot(PCs)
        abline(h = 0, v = 0, lty = 3)
        text(PCs, label = rownames(PCs), pos = 1.1, adj = 0.5, 
            cex = 0.85)
    }
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
