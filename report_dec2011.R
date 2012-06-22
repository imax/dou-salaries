# source("read.salary.R")
# считаем что dd у нас уже есть

# загружаем данные для сравнения — прошлый опрос
dx <- read.salary("data/2011_may_final.csv")

library(lattice)

# skip #1 because it's N/A
top10lang <- factor(dd$Язык, levels=names(sort(summary(dd$Язык), decreasing=T))[2:11])

# доля студентов в рабочей силе
length(dd$salary[dd$Образование=="Незаконченное высшее"&dd$Возраст<23])/length(dd$salary)*100
# доля джуниоров среди разработчиков
table(dd$title[dd$cls=="DEV"])/length(dd$title[dd$cls=="DEV"])*100


png(width=640, filename="dec11_%d.png")

hist(dd$age)
hist(dd$age, breaks=20, main="Возраст программистов")

barplot(table(top10lang), names.arg=levels(top10lang), las=2)
barplot(table(dd$cls))
bwplot(dd$salary~dd$loc, subset=dd$cls=="DEV", ylim=c(0,4500), main="Зарплаты программистов по регионам", ylab="USD/month")
bwplot(dd$salary~dd$title, subset=dd$loc=="Киев"&dd$cls=="DEV")

dev.off()

comp.salary <- function(cur, past) {
    n_cur <- length(cur$salary)
    n_past <- length(past$salary)
    m_cur <- median(cur$salary, na.rm=TRUE)
    m_past <- median(past$salary, na.rm=TRUE)
    delta <- (m_cur-m_past)/m_past*12/7*100
    return (c(delta, m_cur, n_cur, m_past, n_past))
}

comp.salary(dd[dd$Город=="Львов"&dd$cls=="TL/PM"&dd$exp>=2,], dx[dx$Город=="Львов"&dx$cls=="TL/PM"&dx$exp>=2,])
