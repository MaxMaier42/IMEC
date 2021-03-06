% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{initializeNetwork}
\alias{initializeNetwork}
\title{Initialize the explanatory network}
\usage{
initializeNetwork(phenomena, theory1, theory2 = character())
}
\arguments{
\item{phenomena}{Vector of phenomena that are explained}

\item{theory1}{Vector of propositions included in theory 1}

\item{theory2}{Vector of propositions included in theory 2 (only set manually if theory comparison is intended)}
}
\value{
An empty edge matrix (all edges 0)
}
\description{
This function initializes the network in which explanatory relations can be stored later.
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
