\name{z_score}
\alias{z_score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
z_score
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z_score(B, converse = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function (B, converse = FALSE) 
{
    B = as.vector(B)
    if (converse == FALSE || converse == F || converse == "") {
        min_value = min(B)
        max_value = max(B)
        z_score = (B - min_value)/(max_value - min_value) * 60 + 
            40
        z_score
    }
    else if (converse == TRUE || converse == T) {
        min_value = min(B)
        max_value = max(B)
        z_score = (max_value - B)/(max_value - min_value) * 60 + 
            40
        z_score
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
