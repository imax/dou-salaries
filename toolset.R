#!/usr/bin/env Rscript

source("read.salary.R")

assignCategory <- function(dd, dst, agent, lvl, regex) {
  dd[,dst] <- factor(NA, levels=lvl)
  for (i in 1:length(regex)) {
    dd[grepl(regex[i], agent, perl=T), dst] <- lvl[i]
  }
  return (dd)
}

################################################################

dd <- read.salary("data/2011_may_final.csv")

dd$User.Agent <- as.character(dd$User.Agent)

dd <- assignCategory(dd, "platform", dd$User.Agent,
  c("Windows", "Linux", "Mac", "Android", "iPhone/iPad"),
  c("Windows NT|WOW64", "(?!Android)Linux", "Macintosh",
    "Android", "like Mac OS X"))

dd <- assignCategory(dd, "browser", dd$User.Agent,
  c("MSIE", "Firefox", "Chrome", "Opera", "Safari", "Mobile"),
  c(" MSIE ", " Firefox/", " Chrome/", "Opera/",
    " Version/[0-9.]* Safari", " Mobile"))

################################################################

library(lattice)

# png(filename="reports/may2011/toolset.%03d.png",
#     width=1024, height=1024, res=90)



# invisible(dev.off())

