dd <- read.csv("~/Documents/salary.data.csv")

# чтобы убрать factors сначала преобразуем в строки
dd$exp <- as.numeric(as.character(dd$Общий.опыт.работы))
dd[dd$Общий.опыт.работы == "10 и более лет",c("exp")] <- 10
dd[dd$Общий.опыт.работы == "меньше 3 месяцев",c("exp")] <- 0

# сокращаем "Днепропетровск" до "Днепр.", для графиков
dd$Город <- as.character(dd$Город)
dd[dd$Город == "Днепропетровск",c("Город")] <- "Днепр."
dd$Город <- factor(dd$Город)

dd$title <- substr(dd$Должность, 1, 20) # Укорачиваем для графиков
top_cities <- c("Киев", "Харьков", "Львов", "Днепр.", "other")
dd$loc <- sapply(dd$Город, function(city) { factor(if (city %in% top_cities) substr(city, 1, 9) else "other", levels=top_cities) })

# переводим все зарплаты в доллары
dd$salary <- dd$Средняя.зарплата.в.месяц

# пытаемся убрать ошибки пользователей с неправильной валютой
dd[dd$salary > 5000 & dd$Возраст<26,c("Валюта")] <- "h"
dd[dd$Средняя.зарплата.в.месяц <2500 & dd$Валюта == "h",c("Валюта")] <- "d"

# dd[dd$salary > 4000,c("Валюта", "salary", "exp", "loc", "title")]
dd$salary[dd$Валюта == "h"] <- dd$Средняя.зарплата.в.месяц[dd$Валюта == "h"] / 8.0
# убираем подозрительные анкеты с зарплатами меньше $150 
dd <- dd[!dd$salary<150,]

pm_titles = c("Team lead", "Project manager", 
	"Senior Project Manager / Program Manager", 	
	"Director of Engineering / Program Director")
dev_titles = c("Junior Software Engineer", "Software Engineer",
	"Senior Software Engineer", "Technical Lead", "System Architect")
qa_titles = c("Junior QA engineer", "QA engineer",
	"Senior QA engineer", "QA Tech Lead")
other_titles = c("DBA / Администратор баз данных",
"Верстальщик", "Гейм-дизайнер", "Дизайнер",
"Системный администратор", "Технический писатель")


# классификация дерева должностей по группам
dd$cls <- ""
dd[dd$Должность %in% pm_titles, c("cls")] <- "PM"
dd[dd$Должность %in% dev_titles, c("cls")] <- "DEV"
dd[dd$Должность %in% qa_titles, c("cls")] <- "QA"
dd$cls <- factor(dd$cls)

dd$Возраст[dd$Возраст<15] <- NA
dd$Возраст[dd$Возраст>65] <- NA

# write.table(dd,file="~/Projects/dou-salaries/data/2011_may_clean.csv", sep=",")

# Примеры графиков
# самые простейшие
hist(dd$salary[dd$cls=="DEV"])
hist(dd$salary[dd$cls=="QA"])
# программисты во Львове, все технологии
boxplot(dd$salary[dd$cls=="DEV" &  dd$loc == "Львов"] ~ dd$exp[dd$cls=="DEV" & dd$loc == "Львов"], main="Программист .NET Львов", ylab="зарплата, USD", xlab="опыт работы, лет", varwidth=TRUE)
# программисты в Киеве, только .NET
boxplot(dd$salary[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET" &  dd$loc == "Киев"] ~ dd$exp[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET" & dd$loc == "Киев"], main="Программист .NET Киев", ylab="зарплата, USD", xlab="опыт работы, лет", varwidth=TRUE)
hist(dd$salary[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET"], xlab="зарплата в USD", main="Программист .NET Киев")
# QA
boxplot(dd$salary[dd$cls=="QA"] ~ dd$exp[dd$cls=="QA"], main="QA Киев", ylab="зарплата, USD", xlab="опыт работы, лет", varwidth=TRUE)
 boxplot(dd$salary[dd$cls=="QA"] ~ dd$loc[dd$cls=="QA"], main="QA Ukraine", ylab="зарплата, USD", xlab="", varwidth=TRUE)
 

library(lattice)
xyplot(dd$salary[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET" &  dd$loc == "Киев"] ~ dd$exp[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET" & dd$loc == "Киев"], main="Software Developer .NET Kiev", ylab="salary, USD", xlab="years of experience", varwidth=TRUE)

## QA
qa <- dd[dd$cls == "QA", c("salary", "loc", "exp", 
	"Доп..специализация", "Возраст", "Индустрия", "Уровень.английского")]
qa <- qa[!qa$salary > mean(qa$salary)+2*sd(qa$salary),]
summary(qa$salary)
quantile(qa$salary, probs=c(.1, .25, .5, .75, .9))
# hist(qa$salary, main="sample frequency distribution for QA", xlab="salary (net), USD")
bwplot(qa$salary ~ qa$loc, ylab="salary (net), USD", main="QA salaries by city", varwidth=TRUE)
bwplot(qa$salary ~ qa$Уровень.английского, ylab="salary (net), USD", main="QA salaries by English skills", varwidth=TRUE)
xyplot(qa$salary ~ qa$Возраст, ylab="salary (net), USD", main="QA salaries by age", varwidth=TRUE)
xyplot(qa$salary ~ qa$exp, ylab="salary (net), USD", xlab="experience, years", main="QA salaries by years of experience")
bwplot(qa$salary ~ qa$Доп..специализация, ylab="salary (net), USD", main="QA salaries by specialization", varwidth=TRUE)

# XXX
summary(qa$salary[qa$loc!="Львов" & qa$loc!="Харьков" & qa$loc!="Киев"])

# Experience, age, english

# идея: уровень англ - возраст. корреляции нет
bwplot(dd$Уровень.английского ~ dd$Возраст, varwidth=TRUE)

> summary(pm$salary)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    400    1600    2300    2294    3000    4300 
> sd(pm$salary)/sqrt(length(pm$salary))
[1] 56.0636

plot(exp + runif(length(exp), min=-0.25, max=0.25), salary + runif(length(salary), min=-50, max=50), main="PM salaries by years of experience", ylab="salary (net), USD", xlab="work experience, years")
abline(lm(salary ~ exp, data=pm))

Львов 2350, Киев 2200, Харьков и Днепр 2500, остальные 1600. (медиана)

devel <- dd[dd$cls == "Разработчик"
          & dd$Предметная.область != ""
          & dd$Язык.программирования != "",]

median(devel$salary[devel$loc=="Днепропетровск"])

tt <- read.csv("~/Projects/dou-salaries/data/2010_october_clean.csv")
# оставляем только разработчиков
tt <- tt[tt$Специализация!="Project manager" & tt$Специализация != "QA/Testing",]
tt <- tt[tt$Зарплата.за.последний.месяц != NA,]
tt <- tt[tt$Зарплата.за.последний.месяц >= 150 & tt$Зарплата.за.последний.месяц < 3501,]


png(width=640, filename="dev_cmp%d.png")
plot(tt$Зарплата.за.последний.месяц ~ tt$Город, ylab="Salary net, USD", xlab="", main="Wages Oct 2010")
abline(h=1500, col=2)
boxplot(devel$salary ~ devel$loc, ylim=c(0,3500), ylab="Salary net, USD", xlab="", main="Wages May 2011")
abline(h=1600, col=2)

dev.off()

png(width=640, filename="dev_title.png")
bwplot(devel$salary ~ devel$title, ylab="salary (net), USD", main="DEV salaries by title", varwidth=TRUE, ylim=c(0, 4000), scales=list(x=list(rot=45)))

dev.off()

# regions

r <- dd[dd$Город != "Киев" & dd$Город !="Львов" & dd$Город!="Харьков" & dd$Город != "Днепр.",]
cities <- tapply(r$Город, r$Город, length)
r <- r[r$Город %in% names(cities)[cities >= 10],]
r$Город<- factor(r$Город)

# lm.model
salary.model <- lm(salary ~ Образование + exp + Уровень.английского + Должность + Город + Пол + Язык.программирования + Валюта + Предметная.область + Размер.компании + Возраст)
