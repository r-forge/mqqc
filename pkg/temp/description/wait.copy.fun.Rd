\name{wait.copy.fun}
\alias{wait.copy.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wait.copy.fun(x, type = "size")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (x, type = "size") 
{
    if (type == "md5sum") {
        require(tools)
        loop <- T
        while (loop) {
            temp <- md5sum(x)
            Sys.sleep(1)
            temp2 <- md5sum(x)
            if (all(!is.na(c(temp, temp2)))) {
                if (temp == temp2) {
                  loop <- F
                }
                else {
                }
            }
            else {
            }
        }
    }
    if (type == "tryRead") {
        require(tools)
        loop <- T
        while (loop) {
            temp <- class(try(readLines(x), n = 1))
            Sys.sleep(1)
            if (temp != "try-error") {
                loop <- F
            }
            else {
            }
        }
    }
    if (type == "size") {
        loop.t <- T
        while (loop.t) {
            temp <- class(try(file.1 <- file.info(x)$size))
            Sys.sleep(1)
            temp2 <- class(try(file.2 <- file.info(x)$size))
            if (all(!is.na(c(file.1, file.2)))) {
                Sys.sleep(2)
                temp2 <- class(try(file.2 <- file.info(x)$size))
                if (all(!is.na(c(file.1, file.2)))) {
                  cat("\r", file.1, " # ", file.2, "\n")
                  if (file.1 == file.2) {
                    loop.t <- F
                  }
                }
                else {
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
