
#' Viterbi
#'
#' Function to path trace the most likely path through a Hidden
#' Markov Model given an example model and an end state.
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
Viterbi=function(example, switching, visible,initial_distribution) {
  #initialization
  time_step = length(example)
  hidden_s = nrow(switching)
  prev = matrix(0, time_step-1, hidden_s)
  omega = matrix(0, hidden_s, time_step)

  #Log likelyhood to ease computation
  omega[, 1] = log(initial_distribution * visible[, example[1]])

  #Find the most likely hidden step for time step t.
  for(t in 2:time_step){
    for(s in 1:hidden_s) {
      #Find the probability of the next switch
      probs = omega[, t - 1] + log(switching[, s]) + log(visible[s, example[t]])
      #Find the largest probability of the switch
      prev[t - 1, s] = which.max(probs)
      #Find the probability of the most probable switch
      omega[s, t] = max(probs)
    }
  }

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
