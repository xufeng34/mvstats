\name{z_data}
\alias{z_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
z_data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z_data(data, converse = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{converse}{
%%     ~~Describe \code{converse} here~~
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
function (data, converse = FALSE) 
{
    n = ncol(data)
    m = nrow(data)
    score_array = array(1:(m * n), c(m, n))
    for (i in 1:n) {
        score_array[, i] = z_score(data[, i], converse)
    }
    SCORE = as.matrix(score_array)
    dimnames(SCORE)[1] = dimnames(data)[1]
    dimnames(SCORE)[2] = dimnames(data)[2]
    round(SCORE, 4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
