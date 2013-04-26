\name{graph.layout.change}
\alias{graph.layout.change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
graph.layout.change(.layout, .names, templates, change.matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.layout}{
%%     ~~Describe \code{.layout} here~~
}
  \item{.names}{
%%     ~~Describe \code{.names} here~~
}
  \item{templates}{
%%     ~~Describe \code{templates} here~~
}
  \item{change.matrix}{
%%     ~~Describe \code{change.matrix} here~~
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
function (.layout, .names, templates, change.matrix) 
{
    if (length(templates) > 0) {
        for (i in 1:length(templates)) {
            temp <- grep(templates[i], .names, fixed = T)
            if (length(temp) > 0) {
                if (length(temp) > 1) {
                  temp.2 <- gsub(" ", "", .names[temp], fixed = T)
                  temp.3 <- gsub(" ", "", templates, fixed = T)
                  temp.4 <- temp.2 == temp.3[i]
                  temp.vec <- temp[temp.4]
                  if (length(temp.vec == 1) & !is.na(temp.vec)) {
                    temp <- temp.vec
                  }
                  else {
                    temp <- temp[1]
                  }
                }
                if (temp > dim(.layout)[1]) {
                  try(.layout[temp, ] <- .layout[temp, ] * change.matrix[i, 
                    ])
                }
                else {
                  alarm()
                }
            }
        }
    }
    return(.layout)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
