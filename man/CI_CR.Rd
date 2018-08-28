\name{CI_CR}
\alias{CI_CR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CI_CR
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CI_CR(B)
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
    RI = c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 
        1.51)
    Wi = weight(B)
    n = length(Wi)
    if (n > 2) {
        W = matrix(Wi, ncol = 1)
        A = matrix(B, nrow = sqrt(length(B)), ncol = sqrt(length(B)), 
            byrow = TRUE)
        AW = A \%*\% W
        aw = as.vector(AW)
        la_max = sum(aw/Wi)/n
        CI = (la_max - n)/(n - 1)
        CR = CI/RI[n]
        cat("\n CI=", round(CI, 4), "\n")
        cat("\n CR=", round(CR, 4), "\n")
        cat("\n la_max=", round(la_max, 4), "\n\n")
        if (CR <= 0.1) {
            cat(" Consistency test is OK£¡\n")
            cat("\n Wi: ", round(Wi, 4), "\n")
        }
        else {
            cat(" Please adjust the judgment matrix! \n")
            Wi = null
            break
        }
    }
    else if (n <= 2) {
        return(Wi)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
