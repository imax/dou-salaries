

qa <- dd[dd$cls=="QA",]
qa <- qa[!is.na(qa$title),]
qa$title <- factor(qa$title)

bwplot(qa$salary~qa$title, subset=qa$loc=="Киев")
bwplot(dd$salary~dd$title, subset=dd$loc=="Киев"&dd$cls=="DEV")

summary(dd[dd$cls=="QA"&dd$loc=="Киев"&dd$exp<1,])
u1 <- qa[qa$loc=="Львов"&qa$exp>=1&qa$exp<=3,]
u2 <- dd[dd$cls=="QA"&dd$loc=="Харьков"&dd$exp>3,]
summary(qa$salary[qa$loc=="Киев"&qa$title=="QA Tech Lead"])

summary(dd$salary[dd$cls=="Lead/Architect"&dd$loc=="Львов"])

summary(dd$salary[dd$cls=="Senior PM"&dd$loc=="Киев"])


