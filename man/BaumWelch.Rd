% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BaumWelch.R
\name{BaumWelch}
\alias{BaumWelch}
\title{BaumWelch}
\usage{
BaumWelch(example, switching, visible, initial_distribution, n.iter = 100)
}
\arguments{
\item{example}{An example/reference HMM distribution}

\item{switching}{Initial hidden state probability (A->A, A->B, B->A, B->B)}

\item{visible}{Intitial visible state probability (A->1,..,B->3)}

\item{initial_distribution}{Initial probability of the model being A or B.}

\item{n.iter}{Number of iterations until convergence}
}
\value{
The overall probability of hidden and visible states at any given time step.
}
\description{
This function iterates over the values over the probabilities
switching and visible a fixed number of times, it maximizes the
probabilities with respect to the example distribution provided.
}
