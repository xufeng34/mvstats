\name{factpc}
\alias{factpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
factpc
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
factpc(X, m = 2, rotation = "none", scores = "regression")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{rotation}{
%%     ~~Describe \code{rotation} here~~
}
  \item{scores}{
%%     ~~Describe \code{scores} here~~
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
function (X, m = 2, rotation = "none", scores = "regression") 
{
    options(digits = 4)
    S = cor(X)
    p <- nrow(S)
    diag_S <- diag(S)
    sum_rank <- sum(diag_S)
    rowname = names(X)
    colname <- paste("Factor", 1:p, sep = "")
    A <- matrix(0, nrow = p, ncol = p, dimnames = list(rowname, 
        colname))
    eig <- eigen(S)
    for (i in 1:p) A[, i] <- sqrt(eig$values[i]) * eig$vectors[, 
        i]
    for (i in 1:p) {
        if (sum(A[, i]) < 0) 
            A[, i] = -A[, i]
    }
    h <- diag(A \%*\% t(A))
    rowname <- c("SS loadings", "Proportion Var", "Cumulative Var")
    B <- matrix(0, nrow = 3, ncol = p, dimnames = list(rowname, 
        colname))
    for (i in 1:p) {
        B[1, i] <- sum(A[, i]^2)
        B[2, i] <- B[1, i]/sum_rank
        B[3, i] <- sum(B[1, 1:i])/sum_rank
    }
    W = B[2, 1:m] * 100
    Vars = cbind(Vars = B[1, ], Vars.Prop = B[2, ], Vars.Cum = B[3, 
        ] * 100)
    A = A[, 1:m]
    if (rotation == "varimax") {
        cat("\n Factor Analysis for Princomp in Varimax: \n\n")
        VA = varimax(A)
        A = VA$loadings
        s2 = apply(A^2, 2, sum)
        k = rank(-s2)
        s2 = s2[k]
        W = s2/sum(B[1, ]) * 100
        Vars = cbind(Vars = s2, Vars.Prop = W, Vars.Cum = cumsum(W))
        rownames(Vars) <- paste("Factor", 1:m, sep = "")
        A = A[, k]
        for (i in 1:m) {
            if (sum(A[, i]) < 0) 
                A[, i] = -A[, i]
        }
        A = A[, 1:m]
        colnames(A) <- paste("Factor", 1:m, sep = "")
    }
    fit <- NULL
    fit$Vars <- Vars[1:m, ]
    fit$loadings <- A
    X = as.matrix(scale(X))
    PCs = X \%*\% solve(S) \%*\% A
    fit$scores <- PCs
    {
        W = W/sum(W)
        PC = PCs \%*\% W
        Ri = data.frame(F = PC, Ri = rank(-PC))
        fit$Rank <- Ri
    }
    common = apply(A^2, 1, sum)
    fit$common <- common
    fit
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
