\name{checkMqqcInfo}
\alias{checkMqqcInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkMqqcInfo(hotFolder)
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
    tempFolders <- listFolders(hotFolder)
    tempFiles <- listFiles(hotFolder)
    return.vec <- c()
    if (length(list.files(hotFolder, pattern = "_RmqqcFile_Info")) > 
        0) {
        try(mqqcinfo <- unique(read.table(paste(hotFolder, "_RmqqcFile_Info.txt", 
            sep = "/"), header = F, sep = "\t", skip = 0)))
        for (i in 1:length(mqqcinfo[, 1])) {
            mqqcinfo.paths <- dirname(as.character(mqqcinfo[i, 
                1]))
            mqqcinfo.paths <- sapply(mqqcinfo.paths, basename)
            diff.data <- setdiff(tempFolders, mqqcinfo.paths)
            if (length(diff.data) > 0) {
                mqqcinfo <- mqqcinfo[mqqcinfo.paths != diff.data, 
                  ]
                mqqcinfo[i, 2] <- 1
            }
            tempI <- list.files(dirname(as.character(mqqcinfo[i, 
                1])), pattern = "combined", full.name = T)
            tempI <- tempI[file.info(tempI)$isdir]
            if (length(tempI) > 0) {
                mqqcinfo[i, 3] <- 1
            }
            tempI <- list.files(dirname(as.character(mqqcinfo[i, 
                1])), pattern = "evidence.txt", full.name = T, 
                recursive = T)
            tempI <- tempI[!file.info(tempI)$isdir]
            if (length(tempI) > 0) {
                mqqcinfo[i, 4] <- 1
                mqqcInfo <- list.files(dirname(tempI), pattern = "mqqc")
                mqqcInfo <- mqqcInfo[file.info(mqqcInfo)$isdir]
                if (length(mqqcInfo) > 0) {
                  mqqcinfo[i, 5] <- 1
                }
                else {
                  return.vec <- c(return.vec, tempI)
                }
            }
        }
        write.table(as.matrix(mqqcinfo), file = paste(hotFolder, 
            "_RmqqcFile_Info.txt", sep = "/"), quote = F, sep = "\t", 
            row.names = F, col.names = F)
    }
    return(return.vec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
