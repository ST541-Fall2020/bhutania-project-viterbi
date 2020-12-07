devtools::load_all()
library(here)
library(readr)
library(tidyverse)
library(HMM)
library(Rcpp)
data = read_csv(here("/data/ex_data.csv"))

hidden  = matrix(1/2, 2, 2)
visible = matrix(1:6, 2, 3)
visible = visible/rowSums(visible)
initial_distribution = c(1/2, 1/2)

output = BaumWelch(data$Visible, hidden, visible, initial_distribution, n.iter = 100)

hmm = initHMM(c("A", "B"), c(1, 2, 3),
              startProbs = initial_distribution,
              transProbs = hidden,
              emissionProbs = visible)

hmm.out <- baumWelch(hmm, data$Visible, 100)

test <- (map(seq(100, 10000, 100), ~ simHMM(hmm.out$hmm, .x)))

my_accuracy <- map(1:length(test), ~
                     sum(Viterbi(test[[.]]$observation,output$switching,output$visible,initial_distribution) != test[[.]]$states)/length(test[[.]]$states))

hmm_accuracy <- map(1:length(test), ~
                      sum(viterbi(hmm, test[[.]]$observation) != test[[.]]$states)/length(test[[.]]$states))

png(file=here('/results/accuracy.png'))
plot(seq(100, 10000, 100), my_accuracy, xlab = "HMM length", ylab = "accuracy", col = "red")
points(seq(100, 10000, 100), hmm_accuracy, col = "green")
dev.off()

timings <- bench::mark(
  {
    Viterbi(data$Visible,output$switching,output$visible,initial_distribution)
  },
  {
    Viterbi_c(data$Visible,output$switching,output$visible,initial_distribution)
  },
  {
    viterbi(hmm.out$hmm, data$Visible)
  }
)

timings$expression[1] <- "Viterbi (Bhutania)"
timings$expression[2] <- "Viterbi_c (Bhutania)"
timings$expression[3] <- "viterbi (HMM)"

png(file=here('/results/timing.png'))
plot(timings, type = "beeswarm")
dev.off()
