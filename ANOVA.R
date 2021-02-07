install.packages("compute.es")  
install.packages("car")         
install.packages("ggplot2")     
install.packages("multcomp")    
install.packages("pastecs")     
install.packages("WRS", repos="http://R-Forge.R-project.org") 

library(compute.es)   # For effect sizes
library(car)          # For Levene's test of homogeniety of variances
library(ggplot2)      # For graphs
library(multcomp)     # For post hoc tests
library(pastecs)      # For descriptive stats
library(WRS)          # For robust tests
library(dplyr)        # For data handling
library(tidyr)        # For data handling
library(pastecs)



deney <- read.csv("deney.csv", header = TRUE, stringsAsFactors = FALSE) # Loading data

# Arranging the columns appropriately for the data type
deney$Cinsiyet <- factor(deney$Cinsiyet, levels = c(1,2))
deney$Grup <- factor(deney$Grup, levels = c(1,2,3,4))
deney$GelirSeviye <- factor(deney$GelirSeviye, levels = c(1,2,3,4),
                            ordered = TRUE)
deney$Enuzunþehir <- factor(deney$Enuzunþehir, levels = c(1,2,3,4),
                            ordered = TRUE)
deney$Ýntihar <- factor(deney$Ýntihar, levels = c(1:10), 
                        ordered = TRUE)


# Eliminating participants with drug or alcohol dependency, as well as medical or psychological condition
deney <- subset(deney, deney$Alkol == 2)
deney <- subset(deney, deney$TýbbiSorun == 2)
deney <- subset(deney, deney$PsikoSorun == 2)
deney <- subset(deney, deney$Ýlaç == 2)


#Taking relevant columns only
deney <- deney %>% 
  dplyr::select(Grup, Toplam, Cinsiyet, Günlükstres, Depresifdüþünce, Ýntihar, ÞuanRahat)


# Giving experimental groups their appropriate names, instead of codes of numbers
deney$Grup <- factor(deney$Grup, labels = c("Meþru", "Kulaktan", "EhliKulaktan", "Kontrol"))

# Renaming the columns to English
Experiment <- deney %>% 
  rename(Group= Grup, Anxiety = Toplam ,  Sex = Cinsiyet, Daily.Stress =Günlükstres , Depressive.Thoughts=  Depresifdüþünce , Suicidal.Ideation= Ýntihar,
         Relaxed.AtM=ÞuanRahat)

# Renaming the groups to English
Experiment$Group <- factor(Experiment$Group, labels = c("Legitimate", "Overheard", "ApprOverheard", "Control"))


head(Experiment)

# Let's check for descriptive stats for each group
by(Experiment$Anxiety, Experiment$Group, stat.desc, basic = F, norm = T)

# It seems the anxiety levels for each group is distributed normally (-1 < skew.2SE & kurt.2SE < 1)
# Additionally, the Shapiro-Wilk normality test showns no deviance fromm normality for any group (normtest.p > .05)

# Now let's check the homoscedasticity (homogeniety of variances) assumption with Levene's test
leveneTest(Experiment$Anxiety, Experiment$Group, center = mean)

# The p value is greater than .05, therefore we accept the assumption that there variances between groups do not differ.

AnxModel <- aov(Anxiety ~ Group, data = Experiment)
summary(AnxModel)


# If the homoscedasticity assumption was violated, we could've used Welch's F
oneway.test(Toplam ~ Grup, data = deney)

#We need wide format for trimmed mean and bootstrapping
deneyWide <- unstack(deney, Toplam ~ Grup)

#trimmed mean
t1way(deneyWide, tr = .1) #%10 trim
#Ft (3, 75.2)= 1.34, p = .26, not significant still

#trimmed mean and bootstrappint
t1waybt(deneyWide, tr= .1, nboot = 1000) #%10 trim and 1000 samples
  #Ft = 1.3, p = .27, is not significant still

AnxModel <- aov(Toplam ~ Grup, deney)
summary.lm(AnxModel)

#Planned Contrasts#
contrasts(deney$Grup) <- contr.SAS(4) #First we set up the contrasts deafault mode in R
cont<- aov(Toplam ~ Grup, deney)        #Then we make our model
summary.lm(cont)                        #Then asks for the summary


#Creating contrasts manually
contrast1 <- c(1,1,1,-3)
contrast2 <- c(-2,1,1,0)
contrast3 <- c(0,-1,1,0)


contrasts(deney$Grup) <- cbind(contrast1, contrast2, contrast3) #ayrý ayrý contrast oluþturmak yerine c(1,1,1,-3) diye de yazabiirdik içine

deney$Grup #notice the contrasts table at the bottom

deneyPlannedCont <- aov(Toplam~Grup, deney)

summary.lm(deneyPlannedCont)
#The p values are two-tailed, divide by 2 for one-tailed (real) values



#Post hoc Analysis
#Bonferroni ve Holm gibi testleri bunla yaparýz
pairwise.t.test(deney$Toplam, deney$Grup, paired = F, p.adjust.method = "fdr")

#Tukey ve Dunnet gibi testleri bunla yaparýz
posthoc<- glht(aov(Toplam ~ Grup, deney), linfct= mcp(Grup = "Tukey"))
summary(posthoc)
confint(posthoc) #confidence intervals

#Tukey için hýzlýca bunu kullanabiliriz, confidence intervals ile beraber veriyor
TukeyHSD(AnxModel)
