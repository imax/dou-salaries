
dd <- read.csv("data/2011_may_final.csv")

qa <- dd[dd$cls == "QA", c("salary", "loc", "exp",
           "Доп..специализация", "Возраст", "Индустрия",
           "Уровень.английского")]

qa <- qa[!qa$salary > mean(qa$salary)+2*sd(qa$salary),]

################################################################

library(lattice)

png("reports/qa.%03d.png", width=1024, height=480, res=100)

xyplot(I(salary  + runif(length(qa$salary ), -20,   20  ))
     ~ I(Возраст + runif(length(qa$Возраст), -0.25, 0.25))
     | loc, data=qa,
       panel=function(x,y) {
         panel.xyplot(x,y);
         panel.loess(x,y,col="brown");
       }, xlab="Возраст, лет", ylab="Зарплата, $/мес")

xyplot(I(salary + runif(length(qa$salary), -20,   20  ))
     ~ I(exp    + runif(length(qa$exp   ), -0.25, 0.25))
     | loc, data=qa,
       panel=function(x,y) {
         panel.xyplot(x,y);
         panel.loess(x,y,col="brown");
       }, xlab="Опыт работы, лет", ylab="Зарплата, $/мес")

dev.off()
