By Arnav Bhutani

<!-- badges: start -->
<!-- badges: end -->

The goal of bhutania-project-viterbi is to implement the viterbi algorithm in R using an example path through a Hidden Markov Model. This approach was done using a combination of the Baum-Welch Algorithm and the Viterbi Algorithm.

[Project Report](./doc/Final_Report.pdf)

[Project Walkthrough](./doc/project_walkthrough.pdf)

[main file](./src/main.pdf)

Required Packages:
  * testthat
  * devtools
  * HMM (for testing/timing)
  * BenchMark
  * Tidyverse
  * Rcpp
  * Here
  * Readr

Important Code Files:
  * main.R

Primary Functions:
  * [Forward](./R/forward.R): This function will be called by the EM algorithm, BaumWelch to find the probability of the next hidden state from a given visible state.
    * Usage: forward({A path of visible states}, {switching probabilties}, {visible (emission) probabilities}, {probability of observing first hidden state})
  * [Backward](./R/Backward.R): This function will be called by the EM algorithm, BaumWelch to find the probability of the previous hidden state from a previous visible state.
    * Usage: (same as forward) backward({a path of visible states}, {switching probabilties}, {visible probabilities}, {probability of observing first hidden state})
  * [BaumWelch](./R/BaumWelch.R): This function uses expectation-maximization to find the largest probability for the switching and visible(emission) probabities given a visible path and some initial values. 
    * Usage: BaumWelch({a set of visible states}, {initial switching probability}, {initial visible probability}, {initial distribution of hidden states}, {number of iterations})
  * [viterbi.R](./R/viterbi.R): This function uses the probabilities derived from the BaumWelch algorithm to find the most likely hidden state for each visible state in a path trace.
    * Usage: viterbi({set of visible states}, {switching probability}, {visible probability}, {initial distribtion of hidden states})



