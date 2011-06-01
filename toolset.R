#!/usr/bin/env Rscript

source("read.salary.R")

dd <- read.salary("data/2011_may_final.csv")

dd$User.Agent <- as.character(dd$User.Agent)

dd$platform <- factor(NA,
  levels=c("Windows", "Linux", "Mac", "Android", "iPhone/iPad"))

regexPlatform <- c("Windows NT", "(?!Android)Linux",
                   "Macintosh", "Android", "like Mac OS X")

for (i in 1:length(regexPlatform)) {
  dd$platform[grepl(
    regexPlatform[i], dd$User.Agent, perl=T)] <- levels(dd$platform)[i]
}

################################################################

library(lattice)

# png(filename="reports/may2011/toolset.%03d.png",
#     width=1024, height=1024, res=90)



# invisible(dev.off())

