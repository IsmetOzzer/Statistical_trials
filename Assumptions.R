dfl <- read.delim("DownloadFestival(No Outlier).dat", header = TRUE)

hist.day1 <- ggplot(dfl, aes(day1)
                    ) + geom_histogram(aes(y=..density..), #density, rather than frequency, is related to normality 
                                       color = "Black", fill = "White")

hist.day1 + stat_function(fun = dnorm, #stat_function komutu dnorm fonksiyonunu kullanarak normal e�risini �iziyor
                          args = list(mean = mean(dfl$day1, na.rm = TRUE), #normal e�risini mean ve sd arg�manlar�n� kullanarak �iz
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



#qqplot teoretik olarak ideal olan bir normalite datas�n� bizim sample datam�zla k�yaslay�p bir �izgi olu�turuyor
#bu �izgi ne kadar d�zse bizim sample datam�z o kadar normaliteye yak�n demektir

qqplot.day1 <- qplot(sample = dfl$day1, stat = "qq")
qqplot.day1

qqplot.day2 <- qplot(sample = dfl$day2, stat = "qq")
qqplot.day2


describe(dfl$day1)
stat.desc(dfl[, c("day1", "day2", "day3")], 
          basic = FALSE, #basic datay� verme
          norm = TRUE) #norma dair olan datay� ver
#Skew2SE ve Kurt.2SE skew ve kurtosis de�erlerinin standart errorlar�n�n 2 kat�na b�l�yor
#E�er bu de�erler +/-1 den b�y�kse skewness ve kurtosis var demektir
#Sample b�y�d�k�e de�erler 0 a yakla�maktad�r

describe(cbind(dfl$day1, dfl$day2, dfl$day3)) #cbind ile di�er samplelar� da g�sterttik
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
by(rexam$exam, #rexamdaki exam datas�na
   rexam$uni,  #rexamdaki uni de�i�kenine g�re
   stat.desc,  #stat.desc fonksiyonunu uygula
   basic = FALSE, norm = TRUE)  

by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE)
  

dunceData <- subset(rexam, #rexam datas�nda 
                    rexam$uni == "Duncetown Uni") #duncetown uni'ye e�it olan her veriyi se�

sussexData <- subset(rexam, rexam$uni =="Sussex Uni")



dunhist <- ggplot(dunceData, aes(exam))
dunhist + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + stat_function(fun = dnorm,
                                                                args = list(mean = mean(dunceData$exam, na.rm = TRUE),
                                                                sd = sd(dunceData$exam, na.rm = TRUE)))


#Shapiro-Wilks normality test
#Ayn� mean ve sd'ye sahip normal bir datay� bizim datam�z ile k�yasl�yor
#E�er data bizim datam�zdan anlaml� bir �ekilde farkl�la��yorsa (p<.05) bizim datam�z normal de�il demektir
#B�y�k data setlerindeki ufak farkl�l�klar anlaml� de�i�ime yol a�abilir.
#Bu testi yap ama histograma da bak karar� verirken.

stat.desc(rexam$exam, basic = FALSE, norm = TRUE)
#Normtest.w ve normtest.p (p de�eri) shapiro-wilks testini g�sterir
#Normal da��l�ml� bir datay� bizim datam�z ile k�yaslar ve p de�eri .05'den b�y�kse bizim datam�z normal bir
#datadan anlaml� bir �ekilde farkl�d�r, yani normal de�idir demektir.

shapiro.test(rexam$exam)

by(rexam$exam, rexam$uni, shapiro.test)

qplot(sample = rexam$exam, stat = "qq")


#Homogeniety of Varience
#bir ortalama de�er al�n�rken etraf�nda olabilecek (o ortalama de�eri olu�turan) her bir verinin birbirinden �ok farkl� olmamas�
#bu farkl�l���n her bir grup i�in ayn� seviyede olmas�
#�l��m�nde regresyon i�in grafikler, gruplu datalar i�in levene testi kullan�l�r
#levene tesi null hipotezi olan gruplar�n aras�ndaki varyans�n e�it oldu�unu savunur
#E�er p < .05 ise gruplar aras� varyans anlaml� bir �ekilde farkl�l�k g�stermi� demektir ve homojen de�ildir

leveneTest(rexam$exam, #levene testini exam datas�nda
           rexam$uni,  #farkl� �ni gruplar� i�in 
           center = mean) #mean'e g�re yap, bunu yazmasak da mediana g�re otomatik al�yor




#Transforming the data
#E�er normal da��l�m varsay�m� ihlal edilmi�se datay� de�i�tirmek gerekir
#log al�n�r, sqrt al�n�r e�er positive skew varsa
#A robost test is a test which is still valid if its assumptions are not met
#F-test in ANOVA is an example 

dfl <- read.delim("DownloadFestival.dat", header = TRUE)

is.na(dfl$day1) #is any value missing?

rowMeans(cbind(dfl$day1, dfl$day2, dfl$day3), #Her bir row i�in mean day1/2/3 al
         na.rm = TRUE)

sqrt(dfl$day1)


dfl$logday1 <- log10(dfl$day1 + 1)
#E�er datada 0 varsa de�erlere +1 ekledikten sonra log al log10(x + 1) gibi
#Log10(0) bir de�ere sahip de�il

dfl$logday2 <- log10(dfl$day2 + 1)
dfl$logday3 <- log10(dfl$day3 + 1)

dfl$sqrtday1 <- sqrt(dfl$day1)
dfl$sqrtday2 <- sqrt(dfl$day2)
dfl$sqrtday3 <- sqrt(dfl$day3)

#Ifelse fonksiyonu
#ifelse(bir ko�ullu �nerme, e�er �nerme do�ruysa bu de�er, e�er yanl��sa bu de�eri yaz)

dfl$day1NoOutlier <- ifelse(dfl$day1 > 4, NA, dfl$day1)


#E�er t�m bu transformations s�k�nt�l� ise kullan�labilecek iki metod var
#1. trimmed-mean, ortalaman�n y�zde 5/10/20'sini alttan ve �stten k�rparsak data skew olmaz
#Ama bu y�zdeyi belirlerken M-estimator kullanmal�, pcnin belirlemesi



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



