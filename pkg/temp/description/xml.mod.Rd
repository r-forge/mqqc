\name{xml.mod}
\alias{xml.mod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xml.mod(template, xml, gsub.string = "</string>")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{template}{
%%     ~~Describe \code{template} here~~
}
  \item{xml}{
%%     ~~Describe \code{xml} here~~
}
  \item{gsub.string}{
%%     ~~Describe \code{gsub.string} here~~
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
function (template, xml, gsub.string = "</string>") 
{
    start <- grep(paste("<", template, ">", sep = ""), xml, fixed = T)
    stop <- grep(paste("</", template, ">", sep = ""), xml, fixed = T)
    insert.fun <- xml[(start + 1):(stop - 1)]
    insert.clean <- c()
    for (i in 1:length(insert.fun)) {
        temp.i <- insert.fun[i]
        temp.i <- unlist(strsplit(temp.i, ""))
        temp.i.ex <- grep(">", temp.i, fixed = T)
        if (length(temp.i.ex) > 0) {
            temp.i <- temp.i[-c(1:temp.i.ex[1])]
        }
        temp.i <- paste(temp.i, collapse = "")
        temp.i <- gsub(gsub.string, "", temp.i, fixed = T)
        insert.clean[i] <- temp.i
    }
    return(list(template = c(start, stop), insert = insert.fun, 
        insert.clean = insert.clean))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
