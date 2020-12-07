#' Backward
#'
#' This function finds the probability of given model producing the
#' same result as the example set of states from a given endpoint.
#' Basically the reverse of forward.
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
backward <- function(example, switching, visible, init)
{
  example_length = length(example)
  hidden_states = nrow(switching)
  # The last state is given, so it has a p() of 1.
  example_p = matrix(1, example_length, hidden_states)

  #calculate the probabiity of the previous hidden state based on the
  #example visible state.
  for(t in (example_length-1):1)
  {
    tmp = as.matrix(example_p[t+1, ] * visible[, example[t+1]])
    example_p[t, ] = t(switching %*% tmp)
  }
  return (example_p)
}
