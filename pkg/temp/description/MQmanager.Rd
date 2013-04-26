\name{MQmanager}
\alias{MQmanager}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MQmanager(x, folder, File = "_RmqqcFile_Manager.txt", cores = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{folder}{
%%     ~~Describe \code{folder} here~~
}
  \item{File}{
%%     ~~Describe \code{File} here~~
}
  \item{cores}{
%%     ~~Describe \code{cores} here~~
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
function (x, folder, File = "_RmqqcFile_Manager.txt", cores = 1) 
{
    if (length(cores) == 0) {
        try(cores <- system("wmic cpu get NumberOfCores", intern = T))
        cores <- as.numeric(unlist(strsplit(cores, "")))
        cores <- cores[!is.na(cores)] - 1
        if (length(cores) == 0) {
            cores <- 1
        }
    }
    MQman <- list.files(folder, pattern = File)
    setwd(folder)
    TL <- system("tasklist", intern = T)
    TL <- grep("MAXQUA", TL)
    try(tkControl(paste(Sys.time(), "Status: Observing", folder), 
        paste("\nCores used", length(TL), "/", cores)))
    TL <- cores - length(TL)
    if (length(x) > 0) {
        if (length(MQman) == 0) {
            write(x, File)
        }
        else {
            write(x, File, append = T)
        }
    }
    if (length(MQman) > 0) {
        TL <- TL[TL > 0]
        if (length(TL) > 0) {
            print(TL)
            for (i in 1:TL) {
                print(i)
                try(tempi <- readLines(File))
                if (length(tempi) > 0) {
                  alarm()
                  system("start")
                  system(tempi[1], wait = F)
                  tempi <- tempi[-1]
                  if (length(tempi) == 0) {
                    unlink(File)
                  }
                  else {
                    write(tempi, File)
                  }
                }
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
