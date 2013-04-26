\name{mqqc.folder.control}
\alias{mqqc.folder.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mqqc.folder.control(hotFolder)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hotFolder}{
%%     ~~Describe \code{hotFolder} here~~
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
function (hotFolder) 
{
    cat("\r", hotFolder, rep(" ", 100))
    temp.files <- list.folders(hotFolder)
    if (length(list.files(hotFolder, pattern = "_RmqqcFile_Info.txt")) > 
        0) {
        tryError <- class(try(mqqcinfo <- unique(read.table(paste(hotFolder, 
            "_RmqqcFile_Info.txt", sep = "/"), header = F, sep = "\t"))))
        if (tryError != "try-error") {
            for (i in temp.files) {
                tempI <- list.files(i, include.dirs = T, pattern = "evidence.txt", 
                  recursive = T, full.name = T)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
