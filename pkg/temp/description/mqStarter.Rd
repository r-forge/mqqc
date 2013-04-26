\name{mqStarter}
\alias{mqStarter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mqStarter(temp.batch.n, mq.fun, InfoString = "_RmqqcFile_", folder)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp.batch.n}{
%%     ~~Describe \code{temp.batch.n} here~~
}
  \item{mq.fun}{
%%     ~~Describe \code{mq.fun} here~~
}
  \item{InfoString}{
%%     ~~Describe \code{InfoString} here~~
}
  \item{folder}{
%%     ~~Describe \code{folder} here~~
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
function (temp.batch.n, mq.fun, InfoString = "_RmqqcFile_", folder) 
{
    PointGrp <- grep(".", unlist(strsplit(temp.batch.n, "")), 
        fixed = T)
    if (length(PointGrp) < 0) {
        folder.name <- substr(temp.batch.n, 1, (max(PointGrp) - 
            1))
    }
    else {
        folder.name <- paste(temp.batch.n, "folder", sep = "_")
    }
    folder.name <- gsub(".", "", folder.name, fixed = T)
    dir.create(folder.name)
    try(file.rename(temp.batch.n, path.file <- paste(getwd(), 
        folder.name, temp.batch.n, sep = "/")))
    Sys.sleep(0.1)
    vec <- c(path.file, 0, 0, 0, 0)
    if (length(list.files(pattern = paste(InfoString, "Info.txt", 
        sep = ""))) == 0) {
        write.table(t(as.matrix(vec)), file = paste(InfoString, 
            "Info.txt", sep = ""), quote = F, sep = "\t", row.names = F, 
            col.names = F)
    }
    else {
        write.table(t(as.matrix(vec)), file = paste(InfoString, 
            "Info.txt", sep = ""), append = T, quote = F, sep = "\t", 
            row.names = F, col.names = F)
    }
    mq.fun(path.file, folder)
    return(path.file)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
