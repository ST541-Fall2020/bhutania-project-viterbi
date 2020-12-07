#' Forward
#'
#' This function finds the probability of given model producing the
#' same result as the example set of states from a givin initial
#' point
#'
#' @param example example HMM path to be compared to
#' @param switching probability of each hidden state at the next time step
#' @param visible probability of hidden state producing a visible state
#' @param init initial probability for each hidden state (at time step 1)
#'
#' @return A matrix named example_p that contains the probability of each hidden state at each time step.
#' @export
#'
#' @examples
forward = function(example, switching, visible, init) {
  #Initialization
  example_length = length(example)
  hidden_states = nrow(switching)
  example_p = matrix(0, example_length, hidden_states)

  #The first state is determined using a
  #initial chance * prob(visible_state| hidden state)
  example_p[1, ] = init*visible[ , example[1]]

  #calculate the probability of the next result being one of the
  #hidden states based on the probabilty of switching hidden states,
  #and the probability of those hidden states producing each visible
  #state.
  for(t in 2:example_length)
  {
    tmp = example_p[(t-1), ] %*% switching
    example_p[t, ] = tmp * visible[ ,example[t]]
  }
  return(example_p)
}
