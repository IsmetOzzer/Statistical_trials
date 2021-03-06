exam <- read.delim("Exam Anxiety.dat", header = TRUE)

scar <- ggplot(exam, aes(Anxiety, Exam, color = Gender))
scar + geom_point(position = "jitter") + geom_smooth(method = "lm", se = F)

install.packages("Hmisc"); install.packages("ggm");
install.packages("ggplot2"); install.packages("polycor") 

library(boot); library(ggm); library(ggplot2); library(Hmisc);
library(polycor); library(Rcmdr)

#cor, does not give p value
cor(exam$Exam,                      #Take the correlation of exam in the exam data
    exam$Anxiety,                   #Compare it with the anxiety
    use = "pairwise.complete.obs",  #take only the complete values in both data
    method = "pearson")             #with the pearson coefficient

cor(exam[, c("Exam", "Anxiety", "Revise")], use = "complete.obs")

#cor.test
cor.test(exam$Exam, exam$Anxiety) #this is the most informative cor function


cor.test(exam$Revise, exam$Exam)

cor.test(exam$Revise, exam$Anxiety)
#R^2 almak i�in r  de�erini al�p (-0.7092493)^2 karesini al


liar <- read.delim("The Biggest Liar.dat", header = TRUE)

cor.test(liar$Creativity, liar$Position, 
         alternative = "less",  #korelasyonun - de�er alaca��n� d���nd���m�zden less yazd�k
         method = "spearman")   #yar��madaki pozisyon s�ral� oldu�u i�in (1,2,3,..) spearman kullan�yoruz


cor.test(liar$Creativity, liar$Position,
         alternative = "less", 
         method = "kendall")

#Partial Correlation
library(ggm)

exam2 <- exam[, c("Exam", "Anxiety", "Revise")]

pc <- pcor(c("Exam", "Anxiety", "Revise"), #take the partial correlation of the first two variables while controlling for the 3rd
           var(exam2))                     #in the variable exam2
pc

pcor.test(pc, 1, 103) #df ve p de�erlerini almak i�in bunu kullan

#Comparing r values

examMale <- subset(exam, Gender == "Male") 

cor.test(examMale$Anxiety, examMale$Exam, alternative = "less")

cor.test(~ Anxiety + Exam, data = exam, 
         alternative = "less", 
         subset = exam$Gender == "Male") #Yukar�daki i�lemle ayn� sonucu verir



examFemale <- subset(exam, Gender == "Female")

cor.test(examFemale$Anxiety, examFemale$Exam, alternative = "less")

cor.test(~ Anxiety + Exam, data = exam,
         alternative = "less", subset = Gender == "Female") 



by(exam, exam$Gender, 
   FUN = function(x) cor.test(~ Anxiety + Exam, data = exam, alternative = "less"))


#Sonra iki cor de�erinin de z scorunu al�p kar��lart�r




essay <- read.delim("EssayMarks.dat", header = TRUE)


hist(essay$essay)
hist(essay$hours)

stat.desc(essay$essay, basic = FALSE, norm = TRUE)

leveneTest(essay$essay, essay$grade)

stat.desc(essay$hours, basic = FALSE, norm = TRUE)

leveneTest(essay$hours, essay$grade)

scar <- ggplot(essay, aes(hours, essay, color = grade))
scar + geom_point(poisiton = "jitter") + stat_smooth(method = "lm", se = F)

by(essay, essay$grade, 
   FUN = function(x) cor.test(~essay + hours))

cor.test(~hours + essay, data = essay, alternative = "greater")

cor.test(~hours + grade, data = essay, alternative = "greater", method = "spearman")


essay$grade <- factor(essay$grade, levels = c("First Class", "Upper Second Class" ,"Lower Second Class", "Third Class"),
                      ordered = TRUE)
essay$rank <- 
  
  
bar <- ggplot(essay, aes(grade, hours))  
bar + stat_summary(fun = mean, geom = "bar", position = "dodge", 
                   color = "black", fill = "white") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .5)




