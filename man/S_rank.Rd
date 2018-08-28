\name{S_rank}
\alias{S_rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S_rank
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
S_rank(data, Wi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{Wi}{
%%     ~~Describe \code{Wi} here~~
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
function (data, Wi) 
{
    wight_matrix = matrix(Wi, ncol = 1, byrow = FALSE)
    score_matrix = as.matrix(data)
    Si = score_matrix \%*\% wight_matrix
    print(data.frame(Si = Si, ri = rank(-Si)))
    list(Si = Si)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
