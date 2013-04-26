\name{folder.observe}
\alias{folder.observe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Loop function to observe a "hot" folder.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Sets a folder to constantly scan for new files. New files will be moved to a new subfolder of the same name and MaxQuant analysis is started. For proper usage this function requires a working MaxQuant installation on the system. In the initial run folder.observe will ask for the system path to MaxQuant and a useable fasta database. The database has to be configured in the used MaxQuant distribution, otherwise folder.observe will return an error message and stops the analysis. 
}
\usage{
folder.observe(folder = NULL, fun = mqStarter, temp.name = "test")
}

\arguments{
  \item{folder}{
%%     ~~Describe \code{folder} here~~
Folder to be observed. If NULL it will be asked for the path.
}
  \item{fun}{
 Function to execute on detected files. Default is mqStarter function.
%%     ~~Describe \code{fun} here~~
}
  \item{temp.name}{
%%     ~~Describe \code{temp.name} here~~
Name of mqqc specific file to track observed files. Currently unused.
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

\author{
%%  ~~who you are~~
Henrik Zauber
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
 \code{\link{start.qc}}

}
\examples{
# folder.observe()
}
 
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
