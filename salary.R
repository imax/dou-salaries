#!/usr/bin/env Rscript

source("read.salary.R")

dd <- read.salary("data/2011_may_final.csv")

library(lattice)
# library(RColorBrewer)

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

################################################################

devel <- dd[dd$cls == "Разработчик" & dd$Предметная.область != "",]

devel$Предметная.область <- factor(devel$Предметная.область,
  levels=levels(devel$Предметная.область)[2:10])

png(filename="reports/may2011/salary-curve.dev.%03d.png",
    width=1024, height=600, res=90)

palette <- trellis.par.get("superpose.line")$col # brewer.pal(9, "Set1")
ltype <- c(rep(1, length(palette)),
           rep(2, length(levels(devel$Предметная.область)) - length(palette)))

xyplot(salary ~ Возраст, groups=Предметная.область, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette, lty=ltype,
       key=list(columns=1, space="right",
                lines=list(lwd=2, alpha=0.9, col=palette, lty=ltype),
                text=list(levels(devel$Предметная.область))),
       xlim=c(19,46), ylim=c(300,2700),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

xyplot(salary ~ exp, groups=Предметная.область, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette, lty=ltype,
       key=list(columns=1, space="right",
                lines=list(lwd=2, alpha=0.9, col=palette, lty=ltype),
                text=list(levels(devel$Предметная.область))),
       xlim=c(0,10), ylim=c(300,2700),
       xlab="Опыт работы, лет", ylab="Зарплата, $/мес")

invisible(dev.off())

