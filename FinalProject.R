malariadata<-read.csv("MalariaData.csv")
malariadata$category<-factor(malariadata$category, levels=c("control","wild type","Scorpine"))
meanshift<-tapply(malariadata$sporozoites,malariadata$category,mean)
sdshift<-tapply(malariadata$sporozoites,malariadata$category,sd)
number<-tapply(malariadata$sporozoites,malariadata$category,length)
stripchart(main = "Sporozoite level in mosquitos",ylab = "Sporoziote level", xlab= "Mosquito type", sporozoites ~ category, data = malariadata,
           method = "jitter",col=c("orange","red","purple"), pch=16, vertical = TRUE)



malariaanova<-lm(sporozoites ~ category,data= malariadata)

anova(malariaanova)

malariaAnovaSummary <- summary(anova(malariaanova))
malariaAnovaSummary