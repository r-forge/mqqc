\name{mqqc-package}
\alias{mqqc-package}
\alias{mqqc}
\docType{package}
\title{
Quality control of MaxQuant derived proteomic data.
}
\description{
mqqc provides functions to quality control the performance of LCMS systems by analyzing proteomic data from MaxQuant calculations. A hot folder can be set, that is continuously scanned for new MaxQuant data or raw files (only Windows) in the background.
As input for the quality control serves the evidence.txt from MQ calculations. This function works on all systems.
}
\details{
\tabular{ll}{
Package: \tab mqqc\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2013-08-20\cr
License: \tab This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.\cr
}
Windows: Use observe.folder() to set a folder that is continously scanned for new raw files to start MQ runs.
Other Systems: Use start.qc() to select evidence.txt to start quality control.
}
\author{
Henrik Zauber

Maintainer: <henrik.zauber@mdc-berlin.de>

}
\references{
Cox, J. and M. Mann (2008). "MaxQuant enables high peptide identification rates, individualized p.p.b.-range mass accuracies and proteome-wide protein quantification." Nat Biotech 26(12): 1367-1372.
}
%\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
 \code{\link{folder.observe}}
  \code{\link{start.qc}}
}
\examples{
# Windows
# Requires a working MQ version on your system
folder.observe()

# for evidence analysis:
start.qc()
}
