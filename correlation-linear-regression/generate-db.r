#! /usr/bin/env Rscript

set.seed(551991)

## Hardy-Weinberg proportions
hwfq <- function(p) c(p^2, 2*p*(1-p), (1-p)^2)

## Generate Diabetes Example data.frame
n <- 50

id       <- 1:n
db_type  <- sample(c("I", "II"), n, replace = T)
sex      <- sample(c("M", "F"), n, replace=T)

sen_ins  <- floor(rnorm(n, 250 + 50 * (db_type == "II"), 15)) # N(250 + 50/II, 15)
fat_acid <- rnorm(n, 15 + sen_ins/100)                        # N(15  + ins/1000, 1)
chl      <- rnorm(n, 120 - exp(scale(sen_ins)), .5)           # N(120 - e^z(ins), .5)

bmi      <- rnorm(n, 25, 3)                # N(25, 3)
bp_mean  <- (110 - 10 * (sex=="F")
             + 10 * scale(bmi)
             - 3 * (sex=="F") * scale(bmi)
             + 6 * scale(chl))
bp       <- floor(rnorm(n, bp_mean, 2))    # N(100 + 7*z(bmi)  + 6*z(chl), 2)/sex=F
                                           # N(110 + 10*z(bmi) + 6*z(chl), 2)/sex=M

enos     <- sample(c("AA", "Aa", "aa"), n, replace = T, prob = hwfq(0.25))
enos_rec <- enos == "AA"

lod_hatk <- rnorm(n, (db_type == "II") - 3 * enos_rec - (sex == "F") + scale(bmi))
hrt_atk  <- ifelse(plogis(lod_hatk) > .75, "Sim", "Não")

db <- data.frame(id, db_type, sex, sen_ins, fat_acid, chl, bmi, bp, enos, hrt_atk)
write.table(db, 'db.tsv', quote=F, row.names=F)
