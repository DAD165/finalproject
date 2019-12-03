malariadata<-read.csv("MalariaData.csv")
malariadata$category<-factor(malariadata$category, levels=c("control","wild type","Scorpine"))
meanshift<-tapply(malariadata$sporozoites,malariadata$category,mean)
sdshift<-tapply(malariadata$sporozoites,malariadata$category,sd)
number<-tapply(malariadata$sporozoites,malariadata$category,length)

malariaanova<-lm(sporozoites ~ category,data= malariadata)
testResult <- anova(malariaanova)

#Save results of ANOVA into a text file
cat("Comparison of mean values between mosquito groups", file = "results.txt")
cat("\n",file = "results.txt", append = TRUE)
capture.output(testResult, file = "results.txt", append = TRUE)
cat("\nThe null hypothesis can be rejected. There is a significant difference between the mean values of the three groups",file = "results.txt", append = TRUE)
#Create/Save Strip Chart
pdf("FinalProjectDataPlot.pdf")
stripchart(main = "Sporozoite Level in Mosquitos",ylab = "Log(Sporozoite count)", xlab= "Mosquito type", sporozoites ~ category, data = malariadata,
           method = "jitter",col=c("orange","red","purple"), pch=16, vertical = TRUE)
dev.off()

