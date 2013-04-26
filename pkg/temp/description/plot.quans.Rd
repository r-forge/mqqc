\name{plot.quans}
\alias{plot.quans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.quans(temp.plot, log2, xlab = "x", ylab = "y", ref.data, thresh.auto = T, bg = "lightblue", fg.col = 1, main = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp.plot}{
%%     ~~Describe \code{temp.plot} here~~
}
  \item{log2}{
%%     ~~Describe \code{log2} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{ref.data}{
%%     ~~Describe \code{ref.data} here~~
}
  \item{thresh.auto}{
%%     ~~Describe \code{thresh.auto} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{fg.col}{
%%     ~~Describe \code{fg.col} here~~
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
function (temp.plot, log2, xlab = "x", ylab = "y", ref.data, 
    thresh.auto = T, bg = "lightblue", fg.col = 1, main = "") 
{
    if (log2) {
        temp.plot <- log2(temp.plot)
    }
    plot(temp.plot, type = "n", xlim = c(1:2), axes = F, frame = T, 
        xlab = xlab, ylab = ylab, lwd = 5, las = 2, fg = fg.col, 
        bty = "n")
    mtext(xlab, 1, line = 1)
    mtext(main, 3, line = 0, cex = 0.6)
    temp.lwd <- c(4, 4, 5, 4, 4)
    temp.col <- c(4, 2, 1, 2, 4)
    for (i in 1:5) {
        lines(c(1.2, 1.8), rep(temp.plot[i], 2), lwd = temp.lwd[i], 
            col = temp.col[i])
    }
    if (log2) {
        abline(h = sort(c(-ref.data, ref.data, 0)), col = temp.col, 
            lwd = 2, lty = "dotted")
    }
    if (!log2) {
        abline(h = sort(c(ref.data)), col = temp.col, lty = "dotted", 
            lwd = 2)
    }
    axis(2, las = 2, fg = 1, lwd = 2)
    box(lwd = 4, fg = fg.col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
