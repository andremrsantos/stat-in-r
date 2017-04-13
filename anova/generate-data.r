#! /usr/bin/env Rscript --vanilla

set.seed(551991)

n <- 64

## Generate in class exercises dataset
sex  <- sample(c("M", "F"), n, replace = T)
drug <- rep(c("P", "A", "B", "C"), each=n/4)
bp   <- rnorm(n, 120 - 10 * (sex=="F"), 3)
bp[drug=="A"] <- bp[drug=="A"] - 12
bp[drug=="C"] <- bp[drug=="C"] - 15 - 7 * (sex[drug=="C"]=="F")

write.table(data.frame(sex, drug, bp),
            'hbp-treatment.tsv',
            sep='\t', quote=F, row.names=F)

## Generate homework exercise dataset
dose <- factor(rep(1:4, each=n/4), labels=c("0mg", "5mg", "10mg", "50mg"))
sup  <- rep(c("none", "vitC"), n/2)

growth <- rnorm(n, 100 - 7.2 * as.numeric(dose), 7)
growth[sup=="vitC"] <- growth[sup=="vitC"] + 4.3 * as.numeric(dose[sup=="vitC"])

write.table(data.frame(dose, sup, growth),
            "tumor-treatment.tsv",
            sep="\t", quote=F, row.names=F)
