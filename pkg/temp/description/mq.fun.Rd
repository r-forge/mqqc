\name{mq.fun}
\alias{mq.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mq.fun(filePath, folder)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filePath}{
%%     ~~Describe \code{filePath} here~~
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
function (filePath, folder) 
{
    checkMQ <- list.files(paste(path.package("mqqc"), "data", 
        sep = "/"), pattern = "MQpath", full.name = T)
    if (length(checkMQ) == 0) {
        cat("\rChoose MQ Directory!", rep(" ", 100))
        MQloop <- T
        require(tcltk)
        while (MQloop) {
            checkMQ <- tk_choose.dir(caption = "Please select folder containing MQ.")
            checkMQ.bin <- list.files(paste(checkMQ, "bin", sep = "/"), 
                pattern = "MaxQuantCmd.exe", full.name = T)
            if (length(checkMQ.bin) != 0) {
                write(checkMQ, file = paste(path.package("mqqc"), 
                  "data", "MQpath", sep = "/"))
                MQloop = F
            }
        }
    }
    else {
        checkMQ.bin <- readLines(checkMQ)
    }
    assign("filePath", filePath, envir = .GlobalEnv)
    mqpar.name <- list.files(paste(path.package("mqqc"), "data", 
        sep = "/"), "^mqpar", full.name = T)
    if (length(mqpar.name) != 0) {
        mqpar <- readLines(mqpar.name)
        xmlNEW <- xml.replace(c("filePaths"), path.convert(filePath), 
            mqpar)
        xmlNEW <- xml.replace(c("fileNames"), basename(filePath), 
            xmlNEW)
        xmlNEW <- xml.replace(c("paramGroups"), rep(1, length(filePath)), 
            xmlNEW, start.string = "<int>", end.string = "</int>")
        write(xmlNEW, xml.path <- paste(dirname(filePath), "mqpar.xml", 
            sep = "/"))
        threads <- 1
        MQ <- "MaxQuantCmd.exe"
        MQcmd <- paste(checkMQ.bin, "/bin/", MQ, " ", xml.path, 
            " ", threads, sep = "")
        MQcmd <- path.convert(MQcmd)
        MQmanager(MQcmd, folder, cores = NULL)
    }
    else {
        print("Error in MQ start. No XML provided.")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
