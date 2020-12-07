test_that("BaumWelch works on provided dataset", {
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
  exp_out = baumWelch(hmm, data$Visible)

  exp_out$hmm$emissionProbs[1,1]
  test_probs$switching[1,1]

  expect_equivalent(test_probs$switching, exp_out$hmm$transProbs)

  expect_equivalent(test_probs$visible, exp_out$hmm$emissionProbs)

})
