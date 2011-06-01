#!/usr/bin/env Rscript

source("read.salary.R")

dd <- read.salary("data/2011_may_final.csv")

dd$User.Agent[grep("Android|Macintosh|Linux|like Mac OS X", dd$User.Agent, invert=T)]


################################################################

library(lattice)

png(filename="reports/may2011/salary.%03d.png",
    width=1024, height=1024, res=90)



invisible(dev.off())

