# Supplementary text: Materials and Methods - Figures expression profiles (A case of insect female neoteny: E93 expression and links to the juvenile hormone in mealybugs)
Isabelle Vea  
September 28, 2017  

#Supplementary information 2.3. RNA extraction and quantitative RT-PCR
This document provides the R command lines used to produce the figures for E93 study in the Japanese mealybug. (REF)

#Needed packages
- ggplot2
- plyr
- tidyr

#Files

- sample2profile-Krh1E93.csv: qPCR data for rpL32, Kr-h1 and E93 isoforms for male and female expression profiles
- E93-Pyr-male.csv: qPCR data of JHM treatment on males
- E93-Pyr-N3D0.csv: qPCR data on JHM treatment on females



```r
#if packages not installed
library(ggplot2)
library(plyr)
library(tidyr)
```


#Expression profile of E93 during the post-oviposition development of male and female mealybugs
We first examined the expression profiles of PkKr-h1 and three identified isoforms of PkE93.

```r
#loading file expression profile
de<-read.csv(file="sample2profile-Krh1E93.csv",header = TRUE)
names(de)
```

```
##  [1] "cDNA.ID"               "previous.cDNA.."      
##  [3] "Sex"                   "Day.after.oviposition"
##  [5] "Stage"                 "rpL32"                
##  [7] "Kr.h1"                 "E93.1"                
##  [9] "E93.2"                 "E93.3"
```


This file includes the the second derivative maximum (SDM) values for Kr-h1, E93-1, E93-2, E93-3 and rpL32 used for expression profile analysis of males and females. The data was collected from RNA extraction performed on an independent sampling from a previous study on juvenile hormone variations in the Japanese mealybug (Vea et al., 2016 PLOS ONE):


##Kr-h1 expression between males and females (Figure S1)

```r
##Kr-h1 expression profile (Figure S1)
FigS1<-ggplot(de,aes(x=Day.after.oviposition, y=de$Kr.h1/de$rpL32, fill=Sex)) + 
  geom_point(aes(shape=Sex)) + 
  geom_line(aes(linetype=Sex))+
  scale_shape_manual(values=c(1, 19))+ #shape of dots
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Kr-h1 relative expression (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  ggtitle("Figure S1: Expression profile of PkKr-h1 during\nmale and female Japanese mealybug development after oviposition")+
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))
FigS1
```

![](RscriptE93_files/figure-docx/unnamed-chunk-3-1.png)<!-- -->
Our independent sampling showed a consistent expression pattern with the sampling carried out for Vea et al., 2016. PkKr-h1 expression shows a diverging expression when N2 start to differente between male and females. Male expression continues and drops later after the prepupal molt, while female N2 have their PkKr-h1 expression progressively decreasing, to remain unusually low until the adult molt.

##E93 isoform 1 (Figure 2A-top)

```r
##E93-1 expression profile (Figure 2A)
Fig2a1<-ggplot(de,aes(x=Day.after.oviposition, y=de$E93.1/de$rpL32, fill=Sex)) + 
  geom_point(aes(shape=Sex)) + 
  geom_line(aes(linetype=Sex) )+
  scale_shape_manual(values=c(1, 19))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("E93-1 relative expression (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  ggtitle("Figure 2A: Expression profile of PkE93-1 during\nmale and female Japanese mealybug development after oviposition")+
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))

Fig2a1
```

![](RscriptE93_files/figure-docx/unnamed-chunk-4-1.png)<!-- -->
PkE93-1 shows sexually dimorphic expression pattern starting from the middle of N2. PkE93-1 expression rapidly increases and peaks during the pupal stage, while in females, the levels are far lower, but with two small increases at the end of N2 and N3.


##E92 isoform 2 (Figure 2A-middle)

```r
##E93-2 expression profile (Figure S2)
Fig2a2<-ggplot(de,aes(x=Day.after.oviposition, y=de$E93.2/de$rpL32, fill=Sex)) + 
  geom_point(aes(shape=Sex)) + 
  geom_line(aes(linetype=Sex) )+
  scale_shape_manual(values=c(1, 19))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("E93-2 relative expression (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  ggtitle("Figure 2A: Expression profile of PkE93-2 during\nmale and female Japanese mealybug development after oviposition")+
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))

Fig2a2
```

![](RscriptE93_files/figure-docx/unnamed-chunk-5-1.png)<!-- -->
PkE93-2 shows similar patterns to PkE93-1.

##E92 isoform 3 (Fig 2A-bottom)

```r
#E93 -3 expression profile (Figure 2A)
Fig2a3<-ggplot(de,aes(x=Day.after.oviposition, y=de$E93.3/de$rpL32, fill=Sex)) + 
  geom_point(aes(shape=Sex)) + 
  geom_line(aes(linetype=Sex) )+
  scale_shape_manual(values=c(1, 19))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("E93-3 relative expression (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  ggtitle("Figure 2A: Expression profile of PkE93-3 during\nmale and female Japanese mealybug development after oviposition")+
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))

Fig2a3
```

![](RscriptE93_files/figure-docx/unnamed-chunk-6-1.png)<!-- -->

PkE93-3 has generally very low expression pattern but is expressed during embryogenesis and the differential expression is observed sexually at the end of development too.


#Comparison Kr-h1 and E93-1 (Figure 2B)

```r
head(de)
```

```
##   cDNA.ID previous.cDNA..  Sex Day.after.oviposition Stage  rpL32    Kr.h1
## 1    exp1              40 Male                     0   ED0 0.0188 3.40e-05
## 2    exp2              41 Male                     1   ED1 0.0111 4.73e-05
## 3    exp3              42 Male                     2   ED2 0.0218 1.22e-04
## 4    exp4              43 Male                     3   ED3 0.0362 1.96e-03
## 5    exp5              44 Male                     4   ED4 0.0279 1.18e-03
## 6    exp6              45 Male                     5   ED5 0.0298 3.06e-03
##      E93.1    E93.2    E93.3
## 1 6.23e-06 1.28e-06 4.44e-08
## 2 4.52e-06 8.95e-07 1.26e-07
## 3 4.64e-06 1.47e-06 3.08e-07
## 4 5.55e-05 4.80e-06 1.64e-06
## 5 3.30e-05 5.15e-06 1.31e-06
## 6 1.42e-04 1.05e-05 7.42e-06
```

```r
degather<-gather(de,Gene,SDM,7:10)
head(degather)
```

```
##   cDNA.ID previous.cDNA..  Sex Day.after.oviposition Stage  rpL32  Gene
## 1    exp1              40 Male                     0   ED0 0.0188 Kr.h1
## 2    exp2              41 Male                     1   ED1 0.0111 Kr.h1
## 3    exp3              42 Male                     2   ED2 0.0218 Kr.h1
## 4    exp4              43 Male                     3   ED3 0.0362 Kr.h1
## 5    exp5              44 Male                     4   ED4 0.0279 Kr.h1
## 6    exp6              45 Male                     5   ED5 0.0298 Kr.h1
##        SDM
## 1 3.40e-05
## 2 4.73e-05
## 3 1.22e-04
## 4 1.96e-03
## 5 1.18e-03
## 6 3.06e-03
```

```r
desub<-subset(degather, Gene=="Kr.h1"|Gene=="E93.1") #subset two genes
desub<-subset(desub, Day.after.oviposition >= 25) #just after N2

#with both male and female data 
Fig2B<-ggplot(desub,aes(x=Day.after.oviposition, y=(SDM/rpL32), fill=Gene)) + 
  geom_point(aes(shape=Gene), size=3) + 
  geom_line(aes(linetype=Gene) )+
  facet_wrap(~Sex, scales="free",nrow=1) +
  scale_shape_manual(values=c(3, 17))+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
ylab("Comparison relative expression E93-1 and Kr-h1 in males (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  ggtitle("Figure 2B: Comparison of PkKr-h1 and PkE93-1\nafter N2 in males and females separately")+
 theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))

Fig2B
```

![](RscriptE93_files/figure-docx/unnamed-chunk-7-1.png)<!-- -->



```r
#Fig 2B relative expression of E93-1 and PkKr-h1 in 100%
maleprepupa<-subset(de,de$Sex=="Male" & de$Day.after.oviposition >=27 &de$Day.after.oviposition <=35)

maleprepupa$E931_pct<-(maleprepupa$E93.1/maleprepupa$rpL32)/max(maleprepupa$E93.1/maleprepupa$rpL32)
maleprepupa$Krh1_pct<-(maleprepupa$Kr.h1/maleprepupa$rpL32)/max(maleprepupa$Kr.h1/maleprepupa$rpL32)

maleprepupagather<-gather(maleprepupa,Gene,SDM,7:12)

maleprepupa_pct<-subset(maleprepupagather, maleprepupagather$Gene=="E931_pct" |maleprepupagather$Gene=="Krh1_pct")

Fig2Bmale<-ggplot(maleprepupa_pct,aes(x=Day.after.oviposition, y=(SDM), fill=Gene)) + 
  geom_point(aes(shape=Gene), size=4) + 
  geom_line(aes(linetype=Gene) )+
  #facet_wrap(~Sex, scales="free",nrow=1) +
  scale_shape_manual(values=c(3, 17))+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#ylab("Comparison relative expression E93-1 and Kr-h1 in males (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) + 
  #ggtitle("Figure 2B: Comparison of PkKr-h1 and PkE93-1\nafter N2 in males and females separately")+
 theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))


Fig2Bmale
```

![](RscriptE93_files/figure-docx/unnamed-chunk-8-1.png)<!-- -->


#PkKr-h1 and PkE93-2 (Figure S2)

```r
#Kr-h1 and E93-2 (Figure S2)
desub2<-subset(degather, Gene=="Kr.h1"|Gene=="E93.2") #subset two genes
desub2<-subset(desub2, Day.after.oviposition >= 25) #just after N2
#with both male and female data
FigS2<-ggplot(desub2,aes(x=Day.after.oviposition, y=(SDM/rpL32), fill=Gene)) + 
  geom_point(aes(shape=Gene)) + 
  geom_line(aes(linetype=Gene) )+
  facet_wrap(~Sex, scales="free",nrow=1) +
  scale_shape_manual(values=c(3, 17))+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
ylab("Relative expression (/rpL32)") +xlab("days after oviposition") +
    ggtitle("Figure S2: Comparison of PkKr-h1 and PkE93-2\nafter N2 in males and females separately")+
  theme_classic(15) + 
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))
FigS2
```

![](RscriptE93_files/figure-docx/unnamed-chunk-9-1.png)<!-- -->

#PkKr-h1 and PkE93-3 (Figure S3)

```r
#Kr-h1 and E93-3 (Figure S3)
desub3<-subset(degather, Gene=="Kr.h1"|Gene=="E93.3") #subset two genes
desub3<-subset(desub3, Day.after.oviposition >= 25) #just after N2

#with both male and female data
FigS3<-ggplot(desub3,aes(x=Day.after.oviposition, y=(SDM/rpL32), fill=Gene)) + 
  geom_point(aes(shape=Gene)) + 
  geom_line(aes(linetype=Gene) )+
  facet_wrap(~Sex, scales="free",nrow=1) +
  scale_shape_manual(values=c(3, 17))+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
ylab("Relative expression (/rpL32)") +xlab("days after oviposition") +
  theme_classic(15) +
     ggtitle("Figure S3: Comparison of PkKr-h1 and PkE93-3\nafter N2 in males and females separately")+
  theme(axis.title.x = element_text(colour = "#242424",size=6), plot.title = element_text(size=8),axis.title.y = element_text(colour = "#242424",size=6))
FigS3
```

![](RscriptE93_files/figure-docx/unnamed-chunk-10-1.png)<!-- -->



#JH mimic treatments
JHM treatments were performed on both male and female and we assessed their effect on development and gene expression (see 2.4.JHM assays on male prepupae and female juvenile instars for details on the treatments)

##Effect of pyriproxyfen treatment on gene expression: data

```r
#loading file JHM treatment
#females
setwd("/Users/isabelle/Dropbox/_Github_reps/E93mealybug/")
dtf<-read.csv(file="E93-Pyr-N3D0.csv",header = TRUE)
names(dtf)
```

```
##  [1] "cDNA."          "Compound"       "Treatment"      "TreatmentStage"
##  [5] "Sex"            "DAT"            "DayL"           "N"             
##  [9] "rpL32"          "Kr.h1"          "E93"            "PkE93.1"       
## [13] "PkE93.2"        "PkE93.3"
```

```r
head(dtf)
```

```
##   cDNA. Compound Treatment TreatmentStage    Sex DAT DayL N   rpL32
## 1   203 Methanol   Control           N3D0 female   1    A 1 0.00281
## 2   204 Methanol   Control           N3D0 female   1    A 1 0.00405
## 3   205 Methanol   Control           N3D0 female   1    A 1 0.01160
## 4   206 Methanol   Control           N3D0 female   1    A 1 0.00519
## 5   281 Methanol   Control           N3D0 female   2    B 1 0.01247
## 6   282 Methanol   Control           N3D0 female   2    B 1 0.00844
##       Kr.h1       E93   PkE93.1   PkE93.2   PkE93.3
## 1 4.220e-05 9.750e-06 1.765e-06 3.333e-07 6.944e-08
## 2 6.700e-05 1.810e-05 1.847e-06 3.363e-07 9.378e-08
## 3 9.580e-05 3.810e-05 5.162e-06 1.081e-06 1.435e-07
## 4 7.690e-05 2.780e-05 3.619e-06 5.923e-07 1.668e-07
## 5 4.730e-05 2.261e-05 1.528e-06 4.653e-07 1.791e-07
## 6 3.376e-05 1.156e-05 2.448e-07 1.614e-07 1.413e-07
```

```r
dtf<-gather(dtf,Gene,SDM,10:14)
head(dtf)
```

```
##   cDNA. Compound Treatment TreatmentStage    Sex DAT DayL N   rpL32  Gene
## 1   203 Methanol   Control           N3D0 female   1    A 1 0.00281 Kr.h1
## 2   204 Methanol   Control           N3D0 female   1    A 1 0.00405 Kr.h1
## 3   205 Methanol   Control           N3D0 female   1    A 1 0.01160 Kr.h1
## 4   206 Methanol   Control           N3D0 female   1    A 1 0.00519 Kr.h1
## 5   281 Methanol   Control           N3D0 female   2    B 1 0.01247 Kr.h1
## 6   282 Methanol   Control           N3D0 female   2    B 1 0.00844 Kr.h1
##         SDM
## 1 4.220e-05
## 2 6.700e-05
## 3 9.580e-05
## 4 7.690e-05
## 5 4.730e-05
## 6 3.376e-05
```

```r
dtf<-subset(dtf, Gene=="Kr.h1" | Gene=="PkE93.1"| Gene=="PkE93.2"| Gene=="PkE93.3")
dtf$Gene
```

```
##   [1] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##   [8] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [15] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [22] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [29] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [36] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [43] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [50] "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"   "Kr.h1"  
##  [57] "Kr.h1"   "Kr.h1"   "Kr.h1"   "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [64] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [71] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [78] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [85] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [92] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
##  [99] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
## [106] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1"
## [113] "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.1" "PkE93.2"
## [120] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [127] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [134] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [141] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [148] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [155] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [162] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [169] "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2" "PkE93.2"
## [176] "PkE93.2" "PkE93.2" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [183] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [190] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [197] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [204] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [211] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [218] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [225] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
## [232] "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3" "PkE93.3"
```

```r
dtf$DAT
```

```
##   [1] 1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8 8 1 1 1 1 2 2
##  [36] 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8 8 8 1 1 1 1 2 2 2 3 3 3 4
##  [71] 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8 8 1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6
## [106] 6 6 7 7 7 8 8 8 8 8 8 8 8 1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7
## [141] 8 8 8 8 8 8 8 1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8
## [176] 8 8 1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8 8 1 1 1 1
## [211] 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 8 8 8 8 8
```



```r
#males
dtm<-read.csv(file="E93-Pyr-male.csv",header = TRUE)
names(dtm)
```

```
## [1] "Sample.ID" "treatment" "Day"       "DayL"      "Gene"      "SDM.Gene" 
## [7] "SDM.rpL32"
```

```r
head(dtm)
```

```
##   Sample.ID treatment Day DayL  Gene SDM.Gene SDM.rpL32
## 1       299   Control   1    A Kr-h1 3.91e-05   0.00116
## 2       300   Control   2    B Kr-h1 1.05e-05   0.00148
## 3       301   Control   3    C Kr-h1 2.82e-06   0.00221
## 4       302   Control   4    D Kr-h1 3.03e-06   0.00206
## 5       303   Control   4    D Kr-h1 7.78e-06   0.00238
## 6       304     Mimic   4    D Kr-h1 1.50e-04   0.00197
```

```r
dtm<-subset(dtm, Gene=="Kr-h1" | Gene=="PkE93-1"| Gene=="PkE93-2"| Gene=="PkE93-3")
dtm$Gene
```

```
##   [1] Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1  
##   [9] Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1  
##  [17] Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1  
##  [25] Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1  
##  [33] Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1   Kr-h1  
##  [41] Kr-h1   Kr-h1   Kr-h1   Kr-h1   PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [49] PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [57] PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [65] PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [73] PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [81] PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1 PkE93-1
##  [89] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2
##  [97] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2
## [105] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2
## [113] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2
## [121] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-2
## [129] PkE93-2 PkE93-2 PkE93-2 PkE93-2 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## [137] PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## [145] PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## [153] PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## [161] PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## [169] PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3 PkE93-3
## Levels: Kr-h1 PkE93-1 PkE93-2 PkE93-3
```

```r
dtm$DayL
```

```
##   [1] A B C D D D D F E E E E E E E E E E E E D D F F F D D F F B B B B B C
##  [36] C C C C A A A A A A B C D D D D F E E E E E E E E E E E E D D F F F D
##  [71] D F F B B B B B C C C C C A A A A A A B C D D D D F E E E E E E E E E
## [106] E E E D D F F F D D F F B B B B B C C C C C A A A A A A B C D D D D F
## [141] E E E E E E E E E E E E D D F F F D D F F B B B B B C C C C C A A A A
## [176] A
## Levels: A B C D E F
```



Graphs of the effect of JHM treatments on Kr-h1 and E93 gene expression and statistical tests were obtained using the log10-transformed data (see statistical analysis file for more details [add a link to the file after github upload]).



```r
#Fig 3A: male normalized data by rpL32 transformed by log 10
dtmlog10box <- ggplot(dtm,aes(x=DayL,y=log10(SDM.Gene/SDM.rpL32))) + geom_boxplot(width=0.5, size=0.2, outlier.size = 0.1, aes(fill=treatment)) + 
  facet_wrap(~Gene,scales="free",nrow=2) + 
  theme_classic(base_size = 12) +
  ggtitle("Figure 3A: Effect of JHM treatment on Kr-h1\nand E93 expression in males transformed by log 10")+
  scale_fill_manual(name="", # Legend label, use darker colors
                  breaks=c("Control", "Mimic"),
                  labels=c("CT","5 mM"),
                  values=c("white", "grey")) +

  theme(strip.background=element_rect(fill="black"))+ 
  theme(strip.text=element_text(color="white", face="bold"))

dtmlog10box
```

![](RscriptE93_files/figure-docx/unnamed-chunk-13-1.png)<!-- -->



```r
#Fig 3B: females absolute values normalized by rpL32 and transformed by log10
dtflogbox <- ggplot(dtf,aes(x=DayL,y=log10(SDM/rpL32))) + geom_boxplot(width=0.5, size=0.2, outlier.size = 0.1, aes(fill=Treatment)) + 
  facet_wrap(~Gene, scales="free",nrow=2) + 
  theme_classic(base_size = 12) +
  scale_fill_manual(name="", # Legend label, use darker colors
                  breaks=c("Control", "Mimic"),
                  labels=c("CT","20 mM"),
                  values=c("white", "grey")) +
  ggtitle("Figure 3B: Effect of JHM treatment on Kr-h1 and\nE93 expression in females transformed log10")+
  theme(strip.background=element_rect(fill="black"))+ 
  theme(strip.text=element_text(color="white", face="bold"))

dtflogbox
```

![](RscriptE93_files/figure-docx/unnamed-chunk-14-1.png)<!-- -->
