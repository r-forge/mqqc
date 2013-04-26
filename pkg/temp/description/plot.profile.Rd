\name{plot.profile}
\alias{plot.profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.profile(data.i, layout = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.i}{
%%     ~~Describe \code{data.i} here~~
}
  \item{layout}{
%%     ~~Describe \code{layout} here~~
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
function (data.i, layout = T) 
{
    if (layout) {
        layout(matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2), height = c(5, 
            1.5), width = c(5, 1))
    }
    name.file <- unique(data.i$raw.file)
    Ramp.col <- colorRampPalette(c("blue", "green", "yellow", 
        "red"))(101)
    intensity <- data.i$intensity
    intensity <- intensity/max(intensity, na.rm = T) * 100
    Ramp.col <- Ramp.col[(round(intensity) + 1)]
    intensity <- intensity/100 * 5
    intensity[is.na(intensity)] <- 0
    intensity <- sqrt(intensity)
    intensity <- round(intensity, 1) * 1.5
    par(mai = c(0, 1, 0.1, 0))
    col.intensity <- grep("intensity", tolower(colnames(data.i)))
    plot(data.i$retention.time, data.i$m.z, pch = 20, cex = intensity, 
        col = Ramp.col, type = "n", ylab = "m/z", xlim = range(data.i$retention.time, 
            na.rm = T), axes = F, ylim = range(data.i$m.z, na.rm = T), 
        frame = T)
    axis(2)
    axis(1, labels = F)
    axis(4, xpd = NA, labels = F, padj = 0.5)
    legend("topright", legend = name.file)
    grid(col = "darkgrey", lwd = 1.5)
    for (i in 1:length(unique(Ramp.col))) {
        temp.i.sel <- Ramp.col == unique(Ramp.col)[i]
        points(data.i$retention.time[temp.i.sel], data.i$m.z[temp.i.sel], 
            pch = 20, cex = intensity[temp.i.sel], col = Ramp.col[temp.i.sel])
    }
    par(mai = c(0.6, 1, 0, 0))
    dens.crt <- class(try(temp <- density(data.i$retention.time)))
    if (dens.crt == "try-error") {
        temp <- list(x = 0, y = 0)
    }
    plot(temp$x, temp$y, main = "", axes = F, frame = T, xlim = range(data.i$retention.time, 
        na.rm = T), type = "l", xlab = "", ylab = "Density")
    mtext("time in min", 1, line = 1.9, cex = 0.6)
    axis(2, xpd = NA, las = 2)
    axis(1)
    dens.crt <- class(try(temp <- density(data.i$m.z)))
    if (dens.crt == "try-error") {
        temp <- list(x = 0, y = 0)
    }
    grid(col = "darkgrey", lwd = 1.5)
    abline(v = median(data.i$retention.time), col = "red")
    quantiles <- quantile(data.i$retention.time)
    abline(v = quantiles[c(2, 4)], col = "blue")
    par(mai = c(0, 0, 0.1, 0.1))
    plot(temp$y, temp$x, type = "l", axes = F, frame = T, ylim = range(data.i$m.z, 
        na.rm = T), xlab = "Density", ylab = "")
    grid(col = "darkgrey", lwd = 1.5)
    axis(1, xpd = NA, las = 2)
    mtext("Density", 1, line = 4, cex = 0.6)
    plot(1, type = "n", frame = F, axes = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
