\name{initFastaMQ}
\alias{initFastaMQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
initFastaMQ(newFasta = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newFasta}{
%%     ~~Describe \code{newFasta} here~~
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
function (newFasta = T) 
{
    newFasta <- T
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
    mqpar.name <- list.files(paste(path.package("mqqc"), "data", 
        sep = "/"), "^mqpar", full.name = T)
    if (length(mqpar.name) == 0 | newFasta) {
        mqpar.name <- list.files(paste(path.package("mqqc"), 
            "data", sep = "/"), "init_mqpar", full.name = T)
        xmlTemplate <- readLines(mqpar.name)
        Filters <- matrix(c("fasta", ".fasta", "All files", "*"), 
            2, 2, byrow = TRUE)
        loopDB <- T
        while (loopDB) {
            loopDB <- F
            db <- tk_choose.files(filters = Filters, caption = "Select a fasta file for MQ search!")
            warning.col <- c()
            warnings1 <- "no"
            warnings2 <- "no"
            for (i in db) {
                tempI <- list.files(dirname(i), pattern = basename(i))
                if (length(tempI) == 0) {
                  alarm()
                  warnings1 <- "yes"
                  if ((warnings1) == "yes") {
                    db <- db[!db == i]
                  }
                }
                dbLib <- list.files(checkMQ.bin, recursive = T, 
                  pattern = "databases.xml", full.name = T)[1]
                dbLib <- readLines(dbLib)
                test.grep <- grep(paste("filename=\"", basename(i), 
                  "\"", sep = ""), dbLib, fixed = T)
                if (length(test.grep) == 0) {
                  warnings2 <- "yes"
                  if ((warnings2) == "yes") {
                    db <- db[!db == i]
                  }
                }
                warning.col <- c((warnings1), (warnings2))
            }
            if (any(warning.col == "yes")) {
                loopC <- tclvalue(tkmessageBox(message = paste(i, 
                  "is not existent.\nRemaining databases:", paste(db, 
                    collapse = "\n"), "Do you like to choose a new fasta file?"), 
                  type = "yesno"))
                if (loopC == "yes") {
                  loopDB <- T
                }
            }
            if (length(db) == 0 & !loopDB) {
                tkmessageBox(message = "No db loaded, please rerun mqqc and set a proper database for MQ search.")
                stop("no db loaded")
            }
            xmlNew <- xml.replace("fastaFiles", db, xmlTemplate)
            write(xmlNew, paste(path.package("mqqc"), "data/mqpar.xml", 
                sep = "/"))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
