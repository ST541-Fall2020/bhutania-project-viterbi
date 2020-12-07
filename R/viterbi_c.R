#' Viterbi_c
#'
#' Function to path trace the most likely path through a Hidden
#' Markov Model given an end state. C++ variant of
#' BhutaniaViterbi::Viterbi
#'
#' @param example An example HMM, on which various statistics are derived
#' @param switching the probability of switching hidden states (A->A, ..., B->A)
#' @param visible the probability of a hidden state resulting in a visible state.
#' @param initial_distribution probability of the initial hidden state.
#'
#' @return A list with the most likely path of hidden states through the HMM
#' @export
#'
#' @examples
Viterbi_c <- function(example, switching, visible,initial_distribution) {
  #initialization
  time_step = length(example)
  hidden_s = nrow(switching)
  prev = matrix(0, time_step-1, hidden_s)
  omega = matrix(0, hidden_s, time_step)

  #Log likelyhood to ease computation
  omega[, 1] = log(initial_distribution * visible[, example[1]])

  cppFunction('
    using namespace Rcpp;

    // [[Rcpp::export]]
    void calculate_P(int hidden_s, int time_s, NumericMatrix &prev_m, NumericMatrix &prob_m, NumericMatrix switching, NumericMatrix visible, NumericMatrix example)
    {
      for(int i = 1; i < time_s; i++)
      {
        for(int j = 0; j < hidden_s; j++)
        {
          NumericVector probs = {};
          for(int k = 0; k < hidden_s; k++)
          {
            probs.insert(k, (prob_m(k, (i-1)) + log(switching(k, j)) + log(visible(j, example[i]-1))));
          }
          prev_m(i-1, j) = which_max(probs);
          prev_m(i-1, j)++;
          prob_m(j, i) = max(probs);
        }
      }
    }')

  calculate_P(hidden_s, time_step, prev, omega, as.matrix(switching), as.matrix(visible), as.matrix(example))

  S = rep(0, time_step)
  #Most probable last hidden state
  last_state=which.max(omega[,ncol(omega)])
  S[1]=last_state

  #Find most probable route back to front
  j=2
  for(i in (time_step-1):1){
    S[j]=prev[i,last_state]
    last_state=prev[i,last_state]
    j=j+1
  }

  #Set hidden states
  S[which(S==1)]='A'
  S[which(S==2)]='B'

  #reverse the list
  S=rev(S)

  #return the list
  return(S)
}
