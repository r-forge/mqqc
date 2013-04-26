\name{col.grad}
\alias{col.grad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
col.grad(test = best, best = 1, bad = 0, col = c("blue", "orange", "yellow", "green"), res = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{best}{
%%     ~~Describe \code{best} here~~
}
  \item{bad}{
%%     ~~Describe \code{bad} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
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
function (test = best, best = 1, bad = 0, col = c("blue", "orange", 
    "yellow", "green"), res = 100) 
{
    col.ramp <- rev(colorRampPalette(col)(res))
    grad <- seq(best, bad, length.out = res)
    if (best > bad) {
        if (test > best) {
            x.col <- col[length(col)]
        }
        else {
            if (test < bad) {
                x.col <- col[1]
            }
            else {
                x.col <- col.ramp[test <= grad]
                x.col <- x.col[length(x.col)]
            }
        }
    }
    if (best < bad) {
        if (test < best) {
            x.col <- col[length(col)]
        }
        else {
            if (test > bad) {
                x.col <- col[1]
            }
            else {
                x.col <- col.ramp[test >= grad]
                x.col <- x.col[length(x.col)]
            }
        }
    }
    return(list(col = x.col, ramp = col.ramp))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
