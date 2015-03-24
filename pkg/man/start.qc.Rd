\name{start.qc}
\alias{start.qc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality analysis of evidence.txt
%%  ~~function to do ... ~~
}
\description{
Function for quality control of evidence.txt data from MaxQuant analysis. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{ 
start.qc(DataEvidence = NULL, RawBased = T, n = NA, show.path = F, open.doc = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataEvidence}{
%%     ~~Describe \code{DataEvidence} here~~
}
  \item{RawBased}{
  TRUE - all entries in evidence.txt are analyzed as one file. False - raw file based analysis, based on rawFile column in evidence table.
%%     ~~Describe \code{RawBased} here~~
}
  \item{n}{
  Number of raw files to be analyzed. If n = NA all raw files in evidence.txt will be analyzed.
%%     ~~Describe \code{n} here~~
}
  \item{show.path}{
  Logical, returns path in tk window.
%%     ~~Describe \code{show.path} here~~
}
  \item{open.doc}{
  Tries to open created pdf.
%%     ~~Describe \code{open.doc} here~~
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
\author{Henrik Zauber
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
 \code{\link{folder.observe}}
}
\examples{
  # before run, resize Output window or set pdfOut to T (will create a folder and a pdf in your working directory)
	#data(DataEvidence)
	#start.qc(DataEvidence,pdfOut = F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
