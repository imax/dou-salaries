
read.salary <- function(fname="data/2011_dec_final.csv") {

  dd <- read.csv(fname)

  dd <- dd[dd$cls != "",]

  dd$cls <- factor(dd$cls,
    levels=c("QA", "DEV", "SDEV", "PM", "SPM"),
    labels=c("QA", "DEV", "Lead/Architect", "TL/PM", "Senior PM"))

  dd$title <- substr(dd$Должность, 1, 20) # Укорачиваем для графиков

  dd$Уровень.английского <- factor(dd$Уровень.английского,
    levels=c("продвинутый", "выше среднего", "средний",
      "ниже среднего", "элементарный"))

  dd$loc <- factor(dd$loc,
    levels=c("Киев", "Львов", "Харьков", "Днепр.", "other"),
    labels=c("Киев", "Львов", "Харьков", "Днепропетровск", "остальные"))

  dd$Размер.компании <- factor(dd$Размер.компании,
    levels=c("до 1000 человек", "до 200 человек",
      "до 100 человек", "до 50 человек", "до 10 человек"))

  dd$User.Agent <- as.character(dd$User.Agent)

  dd$Дата.заполнения <- strptime(
    dd$Дата.заполнения, "%d/%m/%Y %H:%M:%S", tz="EET")

  dd$exp2 <- as.numeric(as.character(dd$Опыт.работы.на.текущем.месте))
  dd$exp2[dd$Опыт.работы.на.текущем.месте == "10 и более лет"  ] <- 10
  dd$exp2[dd$Опыт.работы.на.текущем.месте == "меньше 3 месяцев"] <- 0

  dd$salaryJitter <- dd$salary  + runif(length(dd$salary ), -50, 50)
  dd$expJitter    <- dd$exp     + runif(length(dd$exp    ), -.3, .3)
  dd$ageJitter    <- dd$Возраст + runif(length(dd$Возраст), -.5, .5)

  return (dd)
}

