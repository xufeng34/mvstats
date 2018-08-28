\name{weight}
\alias{weight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
weight
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weight(B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function (B) 
{
    A = matrix(B, nrow = sqrt(length(B)), ncol = sqrt(length(B)), 
        byrow = TRUE)
    n = ncol(A)
    mul_collect = c(1:n)
    for (i in 1:n) mul_collect[i] = prod(A[i, ])
    weight = mul_collect^(1/n)
    weight_one = weight/sum(weight)
    round(weight_one, 4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
