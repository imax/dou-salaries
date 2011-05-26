dd <- read.csv("~/Documents/salary.data.csv")

# чтобы убрать factors сначала преобразуем в строки
dd$exp <- as.numeric(as.character(dd$Общий.опыт.работы))
dd[dd$Общий.опыт.работы == "10 и более лет",c("exp")] <- 15
dd[dd$Общий.опыт.работы == "меньше 3 месяцев",c("exp")] <- 0

dd$title <- substr(dd$Должность, 1, 20)
top_cities <- c("Киев", "Харьков", "Львов", "Днепропетровск", "Одесса", "Донецк", "other")
dd$loc <- sapply(dd$Город, function(city) { factor(if (city %in% top_cities) city else "other", levels=top_cities) })

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


dd$salary <- dd$Средняя.зарплата.в.месяц
dd[dd$salary > 5000 & dd$Возраст<26,c("Валюта")] <- "h"
# dd[dd$salary > 4000,c("Валюта", "salary", "exp", "loc", "title")]
dd$salary[dd$Валюта == "h"] <- dd$Средняя.зарплата.в.месяц[dd$Валюта == "h"] / 8.0


# классификация дерева должностей по группам
dd$cls <- ""
dd[dd$Должность %in% pm_titles, c("cls")] <- "PM"
dd[dd$Должность %in% dev_titles, c("cls")] <- "DEV"
dd[dd$Должность %in% qa_titles, c("cls")] <- "QA"
dd$cls <- factor(dd$cls)

# write.table(dd,file="~/Projects/dou-salaries/data/2011_may_clean.csv", sep=",")

# Пример графика: программист NET в Киев, зп от стажа
boxplot(dd$salary[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET"] ~ dd$exp[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET"])
hist(dd$salary[dd$cls=="DEV" & dd$Язык.программирования == "C#/.NET"], xlab="зарплата в USD", main="Программист .NET Киев")