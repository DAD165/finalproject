malariadata<-read.csv("MalariaData.csv")#read in CSV data file
malariadata$category<-factor(malariadata$category, levels=c("control","wild type","Scorpine"))#seperate the data into 3 categories based on labels
meanshift<-tapply(malariadata$sporozoites,malariadata$category,mean)#means of the three data groups
sdshift<-tapply(malariadata$sporozoites,malariadata$category,sd)#standard dev of groups
number<-tapply(malariadata$sporozoites,malariadata$category,length)#get length of each category

malariaanova<-lm(sporozoites ~ category,data= malariadata)#ANOVA
testResult <- anova(malariaanova)#run ANOVA and save data to testResult

#Save results of ANOVA into a text file
cat("Comparison of Log(Sporozoite count) between mosquito groups", file = "results.txt")#first line of the text file
cat("\n",file = "results.txt", append = TRUE)#naming file
capture.output(testResult, file = "results.txt", append = TRUE)#prints test result into new txt file
cat("\nThe null hypothesis can be rejected. There is a significant difference between the mean values of the three groups",file = "results.txt", append = TRUE)
#Create/Save Strip Chart
pdf("FinalProjectDataPlot.pdf")#Name pdf file
stripchart(main = "Sporozoite Level in Mosquitos",ylab = "Log(Sporozoite count)", xlab= "Mosquito type", sporozoites ~ category, data = malariadata,
           method = "jitter",col=c("orange","red","purple"), pch=16, vertical = TRUE)#added specific comments to change title, color, and fill
dev.off()#closes plot

