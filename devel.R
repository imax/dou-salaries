#!/usr/bin/env Rscript

source("read.salary.R")

library(lattice)
#library(RColorBrewer)

dd <- read.salary()

devel <- dd[dd$cls == "Разработчик"
          & dd$Язык.программирования != "",]

devel$Предметная.область <- factor(devel$Предметная.область,
  levels=levels(devel$Предметная.область)[2:10])

devel$Язык.программирования <- factor(devel$Язык.программирования,
  levels=names(sort(summary(devel$Язык.программирования),
    decreasing=T))[1:14])

devel$top7 <- factor(devel$Язык.программирования,
  levels=names(sort(summary(devel$Язык.программирования),
    decreasing=T))[1:7])

################################################################

setDomain <- function(dd, col, dom) {
  dd[,col] <- factor(NA, levels=names(dom))
  for (i in 1:length(dom)) {
    name <- names(dom)[i]
    dd[,col][dd$Язык.программирования
             %in% ifelse(is.na(dom[[i]]), name, dom[[i]])] <- name
  }
  return (dd)
}

devel <- setDomain(devel, "domain1", pairlist(
  "Web/scripting"=c("JavaScript", "Flex/Flash/AIR",
    "Ruby/Rails", "Python", "Perl", "PHP"),
  "C/C++"=c("C", "C++"),
  "Java/C#"=c("C#/.NET", "Java"),
  "Apple"="Objective-C", "SQL"=NA, "Delphi"=NA))

devel <- setDomain(devel, "domain2", pairlist(
  "C"=NA, "C++"=NA, "C#/.NET"=NA, "Java"=NA))

################################################################

png(filename="reports/dec2011/salary-curve.dev.%03d.png",
    width=1024, height=600, res=90)

palette.lty <- function(lvl, colors) {
  nlevels <- length(levels(lvl))
  ncolors <- min(nlevels, length(colors))
  return (list(col=colors[1:ncolors],
    lty=sort(rep(1:ceiling(nlevels/ncolors), ncolors))[1:nlevels]))
}

palette <- palette.lty(devel$Предметная.область,
  trellis.par.get("superpose.line")$col)

################################################################

xyplot(salary ~ Возраст, groups=Предметная.область, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
         lines=list(lwd=2, alpha=0.9, col=palette$col, lty=palette$lty),
         title="Предметная область",
         text=list(levels(devel$Предметная.область))),
       xlim=c(19,46), ylim=c(300,2700),
       xlab="Возраст, лет", ylab="Зарплата, $/мес")

xyplot(salary ~ exp, groups=Предметная.область, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
         lines=list(lwd=2, alpha=0.9, col=palette$col, lty=palette$lty),
         title="Предметная область",
         text=list(levels(devel$Предметная.область))),
       xlim=c(0,10), ylim=c(300,2700),
       xlab=expression(bold("Общий") * " опыт работы, лет"),
       ylab="Зарплата, $/мес")

xyplot(salary ~ exp2, groups=Предметная.область, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
         lines=list(lwd=2, alpha=0.9, col=palette$col, lty=palette$lty),
         title="Предметная область",
         text=list(levels(devel$Предметная.область))),
       xlim=c(0,10), ylim=c(900,3100),
       xlab=expression("Опыт работы на " * bold("текущем") * " месте, лет"),
       ylab="Зарплата, $/мес")

################################################################

palette <- palette.lty(devel$domain1,
  trellis.par.get("superpose.line")$col)

png(filename="reports/dec2011/dev.exp.%03d.png",
    width=1024, height=600, res=90)

xyplot(salary ~ exp, groups=top7, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
         lines=list(lwd=2, alpha=0.9, col=palette$col, lty=palette$lty),
         text=list(levels(devel$top7))),
       ylim=c(400,3500), xlim=c(0, 10),
       xlab="Опыт, лет", ylab="Зарплата, $/мес")

#palette <- palette.lty(devel$domain2, brewer.pal(6, "Paired")[3:6])

xyplot(salary ~ возраст, groups=domain2, data=devel,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
         lines=list(lwd=2, col=palette$col, lty=palette$lty),
         title="язык прогр.",
         text=list(levels(devel$domain2))),
       ylim=c(400,2600), xlim=c(19, 41),
       xlab="возраст, лет", ylab="зарплата, $/мес")

dev.off()

################################################################

png(filename="reports/dec2011/dev.age.%03d.png",
    width=1024, height=600, res=90)

densityplot(~ Возраст, groups=domain1, data=devel,
            lwd=2, alpha=0.7, type="", n=200,
            auto.key=list(space="right"), xlim=c(19,41))

dev.off()
