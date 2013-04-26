\name{plot.stat}
\alias{plot.stat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.stat(x, thresh, name.val, rev = F, bg = "lightblue", main = "2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{thresh}{
%%     ~~Describe \code{thresh} here~~
}
  \item{name.val}{
%%     ~~Describe \code{name.val} here~~
}
  \item{rev}{
%%     ~~Describe \code{rev} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (x, thresh, name.val, rev = F, bg = "lightblue", main = "2") 
{
    col.temp <- (colorRampPalette(c("blue", "yellow", "green"))(thresh * 
        100))
    if (rev) {
        col.temp <- (colorRampPalette(c("blue", "yellow"))(thresh * 
            100))
        col.temp <- rev(c(col.temp, rep("green", thresh * 100)))
    }
    if (x > thresh) {
        col.sel <- thresh
        x.range <- c(0, x + x * 0.2)
    }
    else {
        col.sel <- x
        x.range <- c(0, thresh + thresh * 0.2)
    }
    max(c(x, thresh))
    barplot2(x, las = 2, col = col.temp[col.sel * 100], ylab = name.val, 
        ylim = x.range, bg = bg)
    abline(h = thresh, lty = "dashed", lwd = 2)
    mtext(main, 3, cex = 0.7, line = 0.3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
