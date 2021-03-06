% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{summary.IMEC}
\alias{summary.IMEC}
\title{Summary of an IMEC object.}
\usage{
\method{summary}{IMEC}(object, ...)
}
\arguments{
\item{object}{IMEC object.}

\item{...}{other parameters passed on from S3 method.}
}
\description{
Summary of an IMEC object.
}
\examples{
# simple example comparing two hypotheses one of them with more explanatory breadth##
T1 <- c("H1", "H2")
Phenomena <- c("E1", "E2")
Thresholds <- c(2,2)
explanations <- initializeNetwork(Phenomena, T1)
explanations <- explain("H1", "E1", explanations)
explanations <- explain("H1", "E2", explanations)
explanations <- explain("H2", "E2", explanations)
explanations <- contradict("H1", "H2", explanations)
coherence <- computeIMEC(explanations, Thresholds, Phenomena, T1)
summary(coherence)
plot(coherence)
}
