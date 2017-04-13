#! /usr/bin/env Rscript --vanilla

set.seed(551991)

n <- 120

## Generate dataset for in class exercise
hw <- function(p) c(p^2, 2 * p * (1-p), (1-p)^2)

sex   <- sample(c("M", "F"), n, replace = T)
cigar <- sample(c("N", "S"), n, replace = T, prob = c(0.9, 0.15))
cdh1  <- sample(c("11", "12", "22"), n, replace = T, prob = hw(0.35))
tp53  <- sample(c("11", "12", "22"), n, replace = T, prob = hw(0.15))

simulate_anc <- function(n = 100, prob = c(0.25, 0.6, 0.15))
    prop.table(table(sample(c("afr", "eur", "amr"), n, replace = T,
                            prob = prob)))
anc   <- replicate(n, simulate_anc())
afr   <- anc["afr",]
eur   <- anc["eur",]
amr   <- anc["amr",]


lodds <- rnorm(n, 0.5 * (sex == "F") + 2 * (cigar == "S") +
                  3 * (cdh1 == "11") - 2.5 * (tp53 != "22") +
                  10 * amr - 1)
cancer <- ifelse(plogis(lodds) < 0.5, "Caso", "Controle")

write.table(data.frame(sex, cigar, amr, eur, afr, cdh1, tp53, cancer),
            'cancer-leucemia.tsv',
            sep = "\t", quote = FALSE, row.names = F)
