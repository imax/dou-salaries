#!/usr/bin/env Rscript

dd <- read.csv("data/2011_may_final.csv")

dd <- dd[dd$cls != "",]

dd$cls <- factor(dd$cls,
  levels=c("QA", "DEV", "PM"),
  labels=c("Тестер", "Разработчик", "Менеджер"))

dd$loc <- factor(dd$loc,
  levels=c("Киев", "Львов", "Харьков", "Днепр.", "other"),
  labels=c("Киев", "Львов", "Харьков", "Днепропетровск", "остальные города"))

dd$salaryJitter <- dd$salary  + runif(length(dd$salary ), -50, 50)
dd$expJitter    <- dd$exp     + runif(length(dd$exp    ), -.3, .3)
dd$ageJitter    <- dd$Возраст + runif(length(dd$Возраст), -.5, .5)

################################################################

library(lattice)

png(filename="reports/may2011/salary.%03d.png",
    width=1024, height=1024, res=90)

panel.scatter.loess <- function(x,y) {
  panel.xyplot(x, y, alpha=0.4, pch=20)
  panel.loess(x, y, col="brown", lwd=2, alpha=0.7)
}

xyplot(salaryJitter ~ expJitter | cls + loc, data=dd,
       xlim=c(0, 10), ylim=c(0,5000),
       panel=panel.scatter.loess,
       xlab="Опыт работы, лет", ylab="Зарплата, $/мес")

xyplot(salaryJitter ~ ageJitter | cls + loc, data=dd,
       xlim=c(19, 45), ylim=c(0,5000),
       panel=panel.scatter.loess,
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

png(filename="reports/may2011/salary-curve.001.png",
    width=1024, height=320, res=90)

xyplot(salary ~ Возраст | loc, groups=cls, data=dd,
       panel=panel.superpose, panel.groups=panel.loess,
       layout=c(5,1), auto.key=list(columns=3), lwd=2, alpha=0.8,
       xlim=c(19, 45), ylim=c(0,3500),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

png(filename="reports/may2011/salary-curve.002.png",
    width=720, height=320, res=90)

xyplot(salary ~ Возраст | cls, groups=loc, data=dd,
       panel=panel.superpose, panel.groups=panel.loess,
       layout=c(3,1), auto.key=list(columns=5), lwd=2, alpha=0.8,
       xlim=c(19, 41), ylim=c(400,3500),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

invisible(dev.off())

