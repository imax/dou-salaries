#!/usr/bin/env Rscript

source("read.salary.R")

dd <- read.salary("data/2011_may_final.csv")

library(lattice)

png(filename="reports/may2011/salary.%03d.png",
    width=1024, height=1024, res=90)

panel.scatter.loess <- function(x,y) {
  panel.xyplot(x, y, alpha=0.4, pch=20)
  panel.loess(x, y, col="brown", lwd=2, alpha=0.8)
}

xyplot(salaryJitter ~ expJitter | cls + loc, data=dd,
       panel=panel.scatter.loess,
       xlim=c(0, 10), ylim=c(0,5000),
       xlab="Опыт работы, лет", ylab="Зарплата, $/мес")

xyplot(salaryJitter ~ ageJitter | cls + loc, data=dd,
       panel=panel.scatter.loess,
       xlim=c(19, 45), ylim=c(0,5000),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

png(filename="reports/may2011/salary-curve.%03d.png",
    width=1024, height=320, res=90)

xyplot(salary ~ Возраст | loc, groups=cls, data=dd,
       panel=panel.superpose, panel.groups=panel.loess,
       layout=c(5,1), lwd=2, alpha=0.8,
       auto.key=list(columns=3, lines=T, points=F),
       xlim=c(19, 45), ylim=c(0,3500),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

xyplot(salary ~ Возраст | cls, groups=loc, data=dd,
       panel=panel.superpose, panel.groups=panel.loess,
       layout=c(3,1), lwd=2, alpha=0.8,
       auto.key=list(columns=1, space="right", lines=T, points=F),
       xlim=c(19, 41), ylim=c(400,3500),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

dev.off()
