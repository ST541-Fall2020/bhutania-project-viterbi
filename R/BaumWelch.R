#' BaumWelch
#'
#' This function iterates over the values over the probabilities
#' switching and visible a fixed number of times, it maximizes the
#' probabilities with respect to the example distribution provided.
#'
#' @param example An example/reference HMM distribution
#' @param switching Initial hidden state probability (A->A, A->B, B->A, B->B)
#' @param visible Intitial visible state probability (A->1,..,B->3)
#' @param initial_distribution Initial probability of the model being A or B.
#' @param n.iter Number of iterations until convergence
#'
#' @return The overall probability of hidden and visible states at any given time step.
#' @export
#'
#' @examples
BaumWelch = function(example, switching, visible, initial_distribution, n.iter = 100){
  for(i in 1:n.iter){
    #initialization
    time_step = length(example)
    hidden_s = nrow(switching)
    visible_s = ncol(visible)

    #get the probabilities of each timestep forward and backwards
    #based on the current switching and visible probabilities
    alpha = BhutaniaViterbi::forward(example, switching, visible, initial_distribution)
    beta = BhutaniaViterbi::backward(example, switching, visible)

    #array that contains the probability of switching from one state
    #to another for each time step.
    xi = array(0, dim=c(hidden_s, hidden_s, time_step-1))

    #For each timestep, calculate:
    #denominator: the probability of a visible state at time step
    #             t from all hidden states at time step t-1.
    #numerator: the probability of the example visible state at
    #           time step t, being a result of a switch from
    #           each state at time step t-1.
    #xi[s,,t]:  The probability of switching from hidden state s
    #           to another hidden state at time step t
    for(t in 1:time_step-1){
      denominator = ((alpha[t,] %*% switching) * visible[,example[t+1]]) %*% matrix(beta[t+1,])
      for(s in 1:hidden_s){
        numerator = alpha[t,s] * switching[s,] * visible[,example[t+1]] * beta[t+1,]
        xi[s,,t]=numerator/as.vector(denominator)
      }
    }

    #The probability of switching from hidden state s to another
    #hidden state for all time steps
    xi.all.t = rowSums(xi, dims = 2)
    #The probability of switching from hidden state s
    switching = xi.all.t/rowSums(xi.all.t)

    #Find the probability of being in a hidden state at time
    #state t
    gamma = apply(xi, c(1, 3), sum)

    #use gamma to find the probability of a hidden state
    #producing a visible state l
    gamma = cbind(gamma, colSums(xi[, , time_step-1]))
    for(l in 1:visible_s){
      visible[, l] = rowSums(gamma[, which(example==l)])
    }
    visible = visible/rowSums(visible)

  }
  return(list(switching = switching, visible = visible, initial_distribution = initial_distribution))
}
