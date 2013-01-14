
clean.salary.csv <- function(infile, outfile) {

dd <- read.csv(infile)

# чтобы убрать factors сначала преобразуем в строки
dd$exp <- as.numeric(as.character(dd$Общий.опыт.работы))
dd[dd$Общий.опыт.работы == "10 и более лет",c("exp")] <- 10
dd[dd$Общий.опыт.работы == "меньше 3 месяцев",c("exp")] <- 0

# сокращаем "Днепропетровск" до "Днепр.", для графиков
dd$Город <- as.character(dd$Город)
dd[dd$Город == "Днепропетровск",c("Город")] <- "Днепр."
dd$Город <- factor(dd$Город)

# dd$title <- substr(dd$Должность, 1, 20) # Укорачиваем для графиков
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
dd <- dd[!dd$salary<250,]
dd <- dd[!dd$salary>7000,]

pm_titles = c("Team lead", "Project manager")
spm_titles = c( "Senior Project Manager / Program Manager", 	
	"Director of Engineering / Program Director")
dev_titles = c("Junior Software Engineer", "Software Engineer",
	"Senior Software Engineer")
sdev_titles = c("Technical Lead", "System Architect")
qa_titles = c("Junior QA engineer", "QA engineer",
	"Senior QA engineer", "QA Tech Lead")
adm_titles = c("DBA / Администратор баз данных", "Системный администратор") 
other_titles = c("Верстальщик", "Гейм-дизайнер", "Дизайнер", "Технический писатель")


# классификация дерева должностей по группам
dd$cls <- ""
dd[dd$Должность %in% pm_titles, c("cls")] <- "PM"
dd[dd$Должность %in% spm_titles, c("cls")] <- "SPM"
dd[dd$Должность %in% dev_titles, c("cls")] <- "DEV"
dd[dd$Должность %in% sdev_titles, c("cls")] <- "SDEV"
dd[dd$Должность %in% qa_titles, c("cls")] <- "QA"
dd[dd$Должность %in% adm_titles, c("cls")] <- "ADM"
dd$cls <- factor(dd$cls)

dd$Возраст[dd$Возраст<15] <- NA
dd$Возраст[dd$Возраст>65] <- NA

write.table(dd, file=outfile, sep=",")
dd_mini <- dd[,c("Город", "salary", "Должность", "exp", "Язык.программирования")]
minifile <- sub("final", "mini", outfile)
write.table(dd_mini, file=minifile, sep=",")
}

#outfile <- sub("raw", "final", infile)
source <- "data/2012_dec_raw.csv"
final <- sub("raw", "final", source)
mini <- sub("raw", "mini", source)

clean.salary.csv(source, final)
