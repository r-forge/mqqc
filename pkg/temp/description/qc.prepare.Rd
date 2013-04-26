\name{qc.prepare}
\alias{qc.prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qc.prepare(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (data) 
{
    get.env2 <- environment()
    ls.null <- function(get.env = get.env2) {
        ls.temp <- ls(envir = get.env)
        for (i in 1:length(ls.temp)) {
            temp.i <- get(ls.temp[i])
            if (length(temp.i) == 0) {
                assign(ls.temp[i], 0, envir = get.env)
            }
        }
    }
    data <- as.data.frame(data)
    grep.col <- function(string, data) {
        x <- grep(string, colnames(data), fixed = T)
        if (length(x) == 0) {
            x <- 0
        }
        if (length(x) > 1) {
            alarm()
            cat("\rwarning, more than one match found", rep(" ", 
                100))
        }
        return(x)
    }
    summary.data <- list()
    colnames(data) <- tolower(colnames(data))
    raw.files <- grep.col("raw.file", data)
    data.i <- data[unique(data[, raw.files])[1] == data[, raw.files], 
        ]
    type.ident <- data.i[, grep.col("type", data.i)]
    msms.count <- length(grep("MSMS", type.ident))
    summary.data$msms.count <- msms.count
    reten.ident <- data.i[, grep.col("retention.time", data.i)]
    reten.start <- reten.ident[, grep.col("start", reten.ident)]
    reten.stop <- reten.ident[, grep.col("finish", reten.ident)]
    reten.mid <- reten.ident[, colnames(reten.ident) == "calibrated.retention.time"]
    ls.null()
    ret.peak.ratio <- (reten.stop - reten.mid)/(reten.mid - reten.start)
    ret.total <- reten.stop - reten.start
    summary.data$ret.peak.shape <- quantile(ret.peak.ratio, na.rm = T)
    summary.data$ret.width <- quantile(ret.total, na.rm = T)
    summary.data$ret.max <- max(reten.ident, na.rm = T)
    quant.range <- quantile(reten.mid, na.rm = T)[2:4]
    ident.peps <- reten.mid > min(quant.range, na.rm = T) & reten.mid < 
        max(quant.range, na.rm = T)
    data.i.quant <- data.i[ident.peps, ]
    summary.data$total.msms.min <- summary.data$msms.count/summary.data$ret.max
    summary.data$quan.msms.min <- length(grep("MSMS", data.i.quant$type))/(max(quant.range, 
        na.rm = T) - min(quant.range, na.rm = T))
    mass.error <- data.i[, grep.col("mass.error", data.i)]
    mass.error.uncal <- mass.error[, grep.col("uncalibrated", 
        mass.error)]
    mass.error <- mass.error[, -grep.col("uncalibrated", mass.error)]
    summary.data$mass.error.cal <- quantile(mass.error, na.rm = T)
    summary.data$mass.error.uncal <- quantile(mass.error.uncal, 
        na.rm = T)
    pep.identifier <- paste(round(data.i.quant$m.z), round(data.i.quant$charge), 
        data.i.quant$modified.sequence)
    double <- length(pep.identifier) - length(unique(pep.identifier))
    summary.data$quan.duplicates <- double
    summary.data$quan.duplicates.msms <- double/length(grep("MSMS", 
        data.i.quant$type))
    summary.data$score <- quantile(data.i$score, na.rm = T)
    thresholds <- list()
    thresholds$mass.error <- c(0.5, 5)
    thresholds$peak.shape <- c(0.5, 3)
    thresholds$peak.durance <- c(0.3, 1)
    thresholds$msms.count <- 4000
    thresholds$ret.peak.shape <- c(0.5, 3)
    thresholds$ret.width <- c(0.3, 1)
    thresholds$total.msms.min <- 150
    thresholds$quan.msms.min <- 250
    thresholds$mass.error.cal <- c(0.5, 5)
    thresholds$quan.duplicates.msms <- 0.05
    thresholds$score <- 100
    score <- list()
    score$msms <- summary.data$quan.msms.min/thresholds$quan.msms.min
    score$mass.error <- max(thresholds$mass.error.cal[1]/abs(summary.data$mass.error.cal[c(2, 
        4)])) * 0.7 + max(abs(summary.data$mass.error.cal[c(1, 
        5)])/thresholds$mass.error.cal[2]) * 0.3
    score$score <- summary.data$score[3]/thresholds$score
    score$peak.shape <- thresholds$peak.shape[1]/max(abs(log2((summary.data$ret.peak.shape[c(2, 
        4)]))))
    score$ret.width <- thresholds$ret.width[1]/(summary.data$ret.width[c(3)])
    score$quan.duplicates.msms <- thresholds$quan.duplicates.msms[1]/((((summary.data$quan.duplicates.msms))))
    return(list(th = thresholds, sc = score, sd = summary.data, 
        diq = data.i.quant))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
