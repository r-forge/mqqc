\name{tkControl}
\alias{tkControl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tkControl(info = "MQQC INFO BOX", info2 = "Status...")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{info2}{
%%     ~~Describe \code{info2} here~~
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
function (info = "MQQC INFO BOX", info2 = "Status...") 
{
    info <- paste(info, info2, sep = "\n")
    require(tcltk)
    if (!exists("tt")) {
        tt <<- tktoplevel()
        tkwm.title(tt, "mqqc")
        tkString <<- tklabel(tt, text = info)
        tkgrid(.GlobalEnv$tkString)
    }
    if (tclvalue(tkwinfo("exists", tt)) == "0") {
        tt <<- tktoplevel()
        tkwm.title(tt, "mqqc")
        tkString <<- tklabel(tt, text = info)
        tkgrid(tkString)
    }
    else {
        tkconfigure(.GlobalEnv$tkString, text = info)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
