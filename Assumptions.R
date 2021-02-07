dfl <- read.delim("DownloadFestival(No Outlier).dat", header = TRUE)

hist.day1 <- ggplot(dfl, aes(day1)
                    ) + geom_histogram(aes(y=..density..), #density, rather than frequency, is related to normality 
                                       color = "Black", fill = "White")

hist.day1 + stat_function(fun = dnorm, #stat_function komutu dnorm fonksiyonunu kullanarak normal eðrisini çiziyor
                          args = list(mean = mean(dfl$day1, na.rm = TRUE), #normal eðrisini mean ve sd argümanlarýný kullanarak çiz
                                      sd = sd(dfl$day1, na.rm = TRUE)),
                          color = "black", size =1)


hist.day2 <- ggplot(dfl, aes(day2)
                    ) + geom_histogram(aes(y= ..density..),
                                       color = "black", fill = "white")

hist.day2 + stat_function(fun = dnorm,
                          args = list(mean = mean(dfl$day2, na.rm = TRUE),
                                      sd = sd(dfl$day2, na.rm = TRUE)),
                          color = "black", size =1)

hist.day3 <- ggplot(dfl, aes(day3)
                    ) + geom_histogram(aes(y= ..density..),
                                       color = "black", fill = "white") 

hist.day3 + stat_function(fun = dnorm,
                          args = list(mean = mean(dfl$day3, na.rm = TRUE),
                                      sd = sd(dfl$day3, na.rm = TRUE)),
                          color="black", size = 1)



#qqplot teoretik olarak ideal olan bir normalite datasýný bizim sample datamýzla kýyaslayýp bir çizgi oluþturuyor
#bu çizgi ne kadar düzse bizim sample datamýz o kadar normaliteye yakýn demektir

qqplot.day1 <- qplot(sample = dfl$day1, stat = "qq")
qqplot.day1

qqplot.day2 <- qplot(sample = dfl$day2, stat = "qq")
qqplot.day2


describe(dfl$day1)
stat.desc(dfl[, c("day1", "day2", "day3")], 
          basic = FALSE, #basic datayý verme
          norm = TRUE) #norma dair olan datayý ver
#Skew2SE ve Kurt.2SE skew ve kurtosis deðerlerinin standart errorlarýnýn 2 katýna bölüyor
#Eðer bu deðerler +/-1 den büyükse skewness ve kurtosis var demektir
#Sample büyüdükçe deðerler 0 a yaklaþmaktadýr

describe(cbind(dfl$day1, dfl$day2, dfl$day3)) #cbind ile diðer samplelarý da gösterttik
describe(dfl[,c("day1", "day2", "day3")])

#Subset alma
rexam <- read.delim("RExam.dat", header = TRUE)
rexam$uni <- factor(rexam$uni, levels = c(0:1), labels = c("Duncetown Uni","Sussex Uni"))

round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), digits = 3)

histexam <- ggplot(rexam, aes(exam))
histexam + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE),
                                         sd = sd(rexam$exam, na.rm = TRUE)))


histcomp <- ggplot(rexam, aes(computer))
histcomp + geom_histogram(aes(y = ..density..), 
                          color = "black", fill = "white") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(rexam$computer, na.rm = TRUE),
                            sd = sd(rexam$computer, na.rm = TRUE)))

histlec <- ggplot(rexam, aes(rexam$lectures))
histlec + geom_histogram(aes(y=..density..), 
                          color = "black", fill = "white") + 
  stat_function(fun = dnorm,args = list(mean = mean(rexam$lectures, na.rm = TRUE), 
                                        sd = sd(rexam$lectures, na.rm = TRUE)))


b
histnum <- ggplot(rexam, aes(numeracy))
histnum + geom_histogram(aes(y = ..density..), 
                         color = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$numeracy, na.rm = TRUE),
                                         sd = sd(rexam$numeracy, na.rm = TRUE)))

examsc <- ggplot(rexam, aes(uni, exam))
examsc + stat_summary(fun = mean, geom = "bar", position = "dodge", 
                      color = "black", fill = "white") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .5)

examn <- ggplot(rexam, aes(computer, exam, color = uni))
examn + geom_point(poisiton = "jitter") + geom_smooth(method = "lm")


#Running anaylysis for different groups
#By() function

by(data = rexam$exam, INDICES = rexam$uni, FUN = round(stat.desc(basic = FALSE, norm = TRUE)), digits = 3)
by(rexam$exam, #rexamdaki exam datasýna
   rexam$uni,  #rexamdaki uni deðiþkenine göre
   stat.desc,  #stat.desc fonksiyonunu uygula
   basic = FALSE, norm = TRUE)  

by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE)
  

dunceData <- subset(rexam, #rexam datasýnda 
                    rexam$uni == "Duncetown Uni") #duncetown uni'ye eþit olan her veriyi seç

sussexData <- subset(rexam, rexam$uni =="Sussex Uni")



dunhist <- ggplot(dunceData, aes(exam))
dunhist + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + stat_function(fun = dnorm,
                                                                args = list(mean = mean(dunceData$exam, na.rm = TRUE),
                                                                sd = sd(dunceData$exam, na.rm = TRUE)))


#Shapiro-Wilks normality test
#Ayný mean ve sd'ye sahip normal bir datayý bizim datamýz ile kýyaslýyor
#Eðer data bizim datamýzdan anlamlý bir þekilde farklýlaþýyorsa (p<.05) bizim datamýz normal deðil demektir
#Büyük data setlerindeki ufak farklýlýklar anlamlý deðiþime yol açabilir.
#Bu testi yap ama histograma da bak kararý verirken.

stat.desc(rexam$exam, basic = FALSE, norm = TRUE)
#Normtest.w ve normtest.p (p deðeri) shapiro-wilks testini gösterir
#Normal daðýlýmlý bir datayý bizim datamýz ile kýyaslar ve p deðeri .05'den büyükse bizim datamýz normal bir
#datadan anlamlý bir þekilde farklýdýr, yani normal deðidir demektir.

shapiro.test(rexam$exam)

by(rexam$exam, rexam$uni, shapiro.test)

qplot(sample = rexam$exam, stat = "qq")


#Homogeniety of Varience
#bir ortalama deðer alýnýrken etrafýnda olabilecek (o ortalama deðeri oluþturan) her bir verinin birbirinden çok farklý olmamasý
#bu farklýlýðýn her bir grup için ayný seviyede olmasý
#Ölçümünde regresyon için grafikler, gruplu datalar için levene testi kullanýlýr
#levene tesi null hipotezi olan gruplarýn arasýndaki varyansýn eþit olduðunu savunur
#Eðer p < .05 ise gruplar arasý varyans anlamlý bir þekilde farklýlýk göstermiþ demektir ve homojen deðildir

leveneTest(rexam$exam, #levene testini exam datasýnda
           rexam$uni,  #farklý üni gruplarý için 
           center = mean) #mean'e göre yap, bunu yazmasak da mediana göre otomatik alýyor




#Transforming the data
#Eðer normal daðýlým varsayýmý ihlal edilmiþse datayý deðiþtirmek gerekir
#log alýnýr, sqrt alýnýr eðer positive skew varsa
#A robost test is a test which is still valid if its assumptions are not met
#F-test in ANOVA is an example 

dfl <- read.delim("DownloadFestival.dat", header = TRUE)

is.na(dfl$day1) #is any value missing?

rowMeans(cbind(dfl$day1, dfl$day2, dfl$day3), #Her bir row için mean day1/2/3 al
         na.rm = TRUE)

sqrt(dfl$day1)


dfl$logday1 <- log10(dfl$day1 + 1)
#Eðer datada 0 varsa deðerlere +1 ekledikten sonra log al log10(x + 1) gibi
#Log10(0) bir deðere sahip deðil

dfl$logday2 <- log10(dfl$day2 + 1)
dfl$logday3 <- log10(dfl$day3 + 1)

dfl$sqrtday1 <- sqrt(dfl$day1)
dfl$sqrtday2 <- sqrt(dfl$day2)
dfl$sqrtday3 <- sqrt(dfl$day3)

#Ifelse fonksiyonu
#ifelse(bir koþullu önerme, eðer önerme doðruysa bu deðer, eðer yanlýþsa bu deðeri yaz)

dfl$day1NoOutlier <- ifelse(dfl$day1 > 4, NA, dfl$day1)


#Eðer tüm bu transformations sýkýntýlý ise kullanýlabilecek iki metod var
#1. trimmed-mean, ortalamanýn yüzde 5/10/20'sini alttan ve üstten kýrparsak data skew olmaz
#Ama bu yüzdeyi belirlerken M-estimator kullanmalý, pcnin belirlemesi



flick <- read.delim("ChickFlick.dat", header = TRUE)

Bridget <- subset(flick, flick$film == "Bridget Jones' Diary")

Memento <- subset(flick, flick$film == "Memento")


memHist <- ggplot(Bridget, aes(arousal))
memHist + geom_histogram(aes(y= ..density..), 
                         color = "black", fill = "white") + 
  stat_function(fun = dnorm, args = list(mean = mean(Bridget$arousal, na.rm = TRUE),
                                         sd = sd (Bridget$arousal, na.rm = TRUE)))

leveneTest(flick$arousal, flick$film, center = mean)
  
  
bridHist <- ggplot(Bridget, aes(arousal)) 
bridHist + geom_histogram(aes(y = ..density..)) + stat_function(fun = dnorm,
                                                                args = list(mean = mean(Bridget$arousal, na.rm = TRUE),
                                                                            sd = sd(Bridget$arousal, na.rm = TRUE)))  
stat.desc(Bridget$arousal, basic = FALSE, norm = TRUE)  
  
  
flickBar <- ggplot(flick, aes(film, arousal, fill = gender))  

flickBar + stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = .9), width = .5)



