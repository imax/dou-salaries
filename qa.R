#!/usr/bin/env Rscript

dd <- read.csv("data/2011_may_final.csv")

qa <- dd[dd$cls == "QA", c("salary", "loc", "exp",
           "Доп..специализация", "Возраст", "Индустрия",
           "Уровень.английского")]

qa <- qa[!qa$salary > mean(qa$salary)+2*sd(qa$salary),]

qa$salaryJitter <- qa$salary  + runif(length(qa$salary ),    50,   50)
qa$expJitter    <- qa$exp     + runif(length(qa$exp    ), -0.25, 0.25)
qa$ageJitter    <- qa$Возраст + runif(length(qa$Возраст), -0.25, 0.25)

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

dev.off()
