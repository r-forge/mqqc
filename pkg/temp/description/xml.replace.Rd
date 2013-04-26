\name{xml.replace}
\alias{xml.replace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xml.replace(parent, insert, xml, start.string = "<string>", end.string = "</string>")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parent}{
%%     ~~Describe \code{parent} here~~
}
  \item{insert}{
%%     ~~Describe \code{insert} here~~
}
  \item{xml}{
%%     ~~Describe \code{xml} here~~
}
  \item{start.string}{
%%     ~~Describe \code{start.string} here~~
}
  \item{end.string}{
%%     ~~Describe \code{end.string} here~~
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
function (parent, insert, xml, start.string = "<string>", end.string = "</string>") 
{
    for (a in parent) {
        temp.xml <- xml.mod(a, xml)
        temp.range <- temp.xml$template
        temp.substr <- (temp.range[1] + 1):(temp.range[2] - 1)
        if (length(temp.substr) > 0) {
            measure.init <- xml[temp.substr[1]]
            measure.init <- unlist.fun(measure.init, "")
            xml <- xml[-temp.substr]
            vec.collect <- c()
            for (i in insert) {
                temp.i <- paste(paste(rep(" ", measure.init), 
                  collapse = "", sep = ""), start.string, i, 
                  end.string, collapse = "", sep = "")
                vec.collect <- c(vec.collect, temp.i)
            }
        }
        if (length(insert) > 0) {
            xml <- c(xml[1:temp.xml$template[1]], vec.collect, 
                xml[(temp.xml$template[1] + 1):length(xml)])
        }
    }
    return(xml)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
