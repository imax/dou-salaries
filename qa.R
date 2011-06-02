#!/usr/bin/env Rscript

source("read.salary.R")

qa <- read.salary("data/2011_may_final.csv")

qa <- qa[qa$cls == "Тестер",]
qa <- qa[qa$salary <= mean(qa$salary) + 2*sd(qa$salary),]

################################################################

library(lattice)

png(filename="reports/may2011/qa.%03d.png",
    width=1024, height=320, res=100)

panel.scatter.loess <- function(x,y) {
  panel.xyplot(x, y);
  panel.loess(x, y, col="brown");
}

xyplot(salaryJitter ~ expJitter | loc, data=qa,
       layout=c(5,1), panel=panel.scatter.loess,
       xlab="Опыт работы, лет", ylab="Зарплата, $/мес")

xyplot(salaryJitter ~ ageJitter | loc, data=qa,
       layout=c(5,1), panel=panel.scatter.loess,
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

invisible(dev.off())

