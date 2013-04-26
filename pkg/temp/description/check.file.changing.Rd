\name{check.file.changing}
\alias{check.file.changing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check.file.changing(files, mode = "size")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
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
function (files, mode = "size") 
{
    temp <- file.info(files)
    temp1 <- temp[, names(temp) == mode]
    temp1 <- cbind(rownames(temp), temp1)
    Sys.sleep(2)
    temp2 <- temp[, names(temp) == mode]
    temp2 <- cbind(rownames(temp), temp2)
    temp.all <- merge(temp1, temp2, by = 1)
    temp.all.F <- apply(temp.all[, 2:3], 1, function(x) {
        x <- all(x == 0)
        return(x)
    })
    if (!all(!temp.all.F)) {
        temp.all <- temp.all[!temp.all.F, ]
    }
    temp.all <- temp.all[temp.all[, 2] == temp.all[, 3], 1]
    return(temp.all)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
