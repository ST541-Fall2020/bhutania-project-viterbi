test_that("viterbi_c works", {
  library(here)

  data <- read.csv(here("/data/ex_data.csv"))
  hidden  = matrix(0.5, 2, 2)
  visible = matrix(1:6, 2, 3)
  visible = visible/rowSums(visible)
  init = c(1/2, 1/2)

  test_probs = BaumWelch(data$Visible, hidden, visible, init, n.iter = 100)

  library(HMM)
  hmm = initHMM(c("A", "B"), c(1, 2, 3),
                startProbs = init,
                transProbs = hidden,
                emissionProbs = visible)
  ratio = baumWelch(hmm, data$Visible)

  exp_out = viterbi(ratio$hmm, data$Visible)
  path = Viterbi(data$Visible, hidden, visible, init)

  expect_equivalent(path, exp_out)
})
