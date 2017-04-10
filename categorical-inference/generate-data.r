#! /usr/bin/env Rscript

set.seed(551991)

## Hardy-Weiberg equilibrium frequencies
hwfq     <- function(p) c(p^2, 2 * p * (1-p), (1-p)^2)

## Generate CF table
n        <- 50

amostra  <- 1:n
sexo     <- sample(c("M", "F"), n, replace = T)
idade    <- floor(rnorm(n, 15, 3))

hipoptr  <- sample(c("Sim", "Não"), n, replace = T, prob = c(0.76, 0.24))
anemia   <- sample(c("Sim", "Não"), n, replace = T, prob = c(0.4, 0.6))
response <- ifelse(hipoptr == "Sim",
                  sample(c("I", "II", "III"), n, replace = T, prob = hwfq(0.55)),
                  sample(c("I", "II", "III"), n, replace = T, prob = hwfq(0.3)))
mut_a    <- ifelse(hipoptr == "Sim",
                  sample(c("AA", "Aa", "aa"), n, replace = T, prob = hwfq(0.4)),
                  sample(c("AA", "Aa", "aa"), n, replace = T, prob = hwfq(0.1)))
mut_b    <- ifelse(hipoptr == "Sim",
                  sample(c("BB", "Bb", "bb"), n, replace = T, prob = hwfq(0.1)),
                  sample(c("BB", "Bb", "bb"), n, replace = T, prob = hwfq(0.25)))

write.table(
    data.frame(amostra, sexo, idade, hipoptr, anemia, response, mut_a, mut_b),
   'cftr-ex.tsv', quote=F, row.names=F)

## Generate CF Table
n <- 50

enosa <- sample(c("AA", "Aa", "aa"), n, replace = T, prob = hwfq(0.21))
enosb <- sample(c("BB", "Bb", "bb"), n, replace = T, prob = hwfq(0.38))
enosc <- sample(c("CC", "Cc", "cc"), n, replace = T, prob = hwfq(0.75))
enosd <- sample(c("DD", "Dd", "dd"), n, replace = T, prob = c(.8, .05, .15))

add_enosb <- c("BB" = 0, "Bb" = 1, "bb" = 2)[enosb]
lod_hbp   <- rnorm(n, 2 * (enosa != "aa") + add_enosb)
hbp       <- ifelse(plogis(lod_hbp) < 0.6, "sim", "não")

write.table(
    data.frame(enosa, enosb, enosc, enosd, hbp),
    'cf.tsv', quote = F, row.names = F)
