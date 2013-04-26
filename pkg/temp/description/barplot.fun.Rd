\name{barplot.fun}
\alias{barplot.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Barplot function for mqqc output.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is part of the mqqc quality control analysis and should not be used alone.
}
\usage{
barplot.fun(x, name.lab, max.val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name.lab}{
%%     ~~Describe \code{name.lab} here~~
}
  \item{max.val}{
%%     ~~Describe \code{max.val} here~~
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
function (x, name.lab, max.val) 
{
    x[is.infinite(x)] <- NA
    temp.b <- barplot(x[order(temp.time)], names.arg = temp.time[order(temp.time)], 
        las = 2, main = paste(unique(name.collect)[g], "\n", 
            name.lab), ylim = c(0, max.val))
    grid()
    abline(v = temp.b[as.logical(temp.extra[order(temp.time)])], 
        col = 2)
    abline(h = median(x[order(temp.time)], na.rm = T), col = "darkgreen")
    legend("topright", legend = median(x[order(temp.time)], na.rm = T))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
