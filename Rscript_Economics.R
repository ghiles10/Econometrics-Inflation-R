

library(RcmdrMisc)
library(lawstat)
library(sandwich)
library(car)
library(lmtest)
library(Hmisc)
library(tidyverse)



#### ouverture base de donnees
taux_d_interet <- read_csv("taux d'interet.csv")
salaire <- read_csv("salaire.csv")
prix_mp <- read_csv("prix mp.csv")
prix_logements <- read_csv("prix logements.csv")
inflation <- read_csv("inflation.csv")
chomage <- read_csv("chomage.csv")
importation <- read_csv("importation.csv")
croissance_PIB<-croissance_PIB <- read_csv("croissance PIB.csv")





################################################ Base de donnees finale####################################

#### inflation 
base_inflation<-inflation%>%select(LOCATION,Value)

#### chomage 
base_chomage<-chomage%>%select(LOCATION,Value)


#### importation 
base_importation<-importation%>%select(LOCATION,Value)


### prix MP
base_MP<-prix_mp%>%select(LOCATION,Value)
base_MP$Value<-base_MP$Value/100


#### prix logements
base_logements<-prix_logements%>%select(LOCATION,Value)
base_logements$Value<-base_logements$Value/100


#### salaire 
base_salaire<-salaire%>%select(LOCATION,Value)

#### taux d'interet
base_interet<-taux_d_interet%>%select(LOCATION,Value)

### croissance PIB
base_croissance_PIB<-croissance_PIB%>%select(LOCATION,Value)



### fusion des bases de donnees 

base_final<-merge(base_inflation,base_chomage,by="LOCATION")
base_final<-rename(base_final,"inflation"=Value.x)
base_final<-rename(base_final,"chomage"=Value.y)

base_final<-merge(base_final,base_importation,by="LOCATION")
base_final<-rename(base_final,"vari_importation"=Value)


base_final<-merge(base_final,base_MP,by="LOCATION")
base_final<-rename(base_final,"vari_MP"= Value)


base_final<-merge(base_final,base_logements,by="LOCATION")
base_final<-rename(base_final,"vari_logements"=Value)



base_final<-merge(base_final,base_salaire,by="LOCATION")
base_final<-rename(base_final,"salaire"=Value)



base_final<-merge(base_final,base_interet,by="LOCATION")
base_final<-rename(base_final,"taux_interet"=Value)



base_final<-merge(base_final,base_croissance_PIB,by="LOCATION")
base_final<-rename(base_final,"croissance_PIB"=Value)





##################1) description globale
summary(base_final)



########################################################### statistiques descriptives 

###la distribution de  l'inflation suit plus ou moins une loi normale 
hist(base_final$inflation, main = " hist inflation", xlab = "inflation",freq = F)
densite_inflation <- density(base_final$inflation)
lines(densite_inflation, col = "red",lwd=3) 


###la distribution du taux de chomage suit une loi log normale mais on ne peux pas le mettre en log car il est deja en %
hist(base_final$chomage,main = "hist chomage",xlab = "chomage")

#### on remarque que le distribution du salaire est asymetrique on transforme en log
hist(base_final$lsalaire, xlab = "log(salaire)", ylab = ("frequence"), main="histogramme log(salaire)")
hist(base_final$salaire, xlab = "salaire", ylab = "frequence", main="histogramme salaire")
base_final$lsalaire<-log(base_final$salaire)
hist(base_final$lsalaire,main = "hist salaire",xlab = "log(salaire)")

hist_lsalaire<-ggplot(base_final, aes(x=base_final$lsalaire)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="salaire")+labs(y="densité")+ labs(title="distribution log(salaire)")
hist_lsalaire

hist_salaire<-ggplot(base_final, aes(x=base_final$salaire)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="salaire")+labs(y="densité")+ labs(title="distribution salaire")
hist_salaire



### la distrubution de la variation du prix des importations ne suit pas une loi normale
hist(base_final$vari_importation,main = "hist Prix importation",xlab = "IPP",freq = F)
densite_importation <- density(base_final$vari_importation)
lines(densite_importation, col = "red",lwd=3) 

hist_importation<-ggplot(base_final, aes(x=vari_importation)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="vari_importation")+labs(y="densité")+ labs(title="distribution vari_importation")
hist_importation

### la distribution de la variation du prix des matieres premieres ne suit pas une loi normale ..
hist(base_final$vari_MP,main = "hist variation prix MP",xlab = "IPMP")
densite_mp <- density(base_final$vari_MP)
lines(densite_mp, col = "red",lwd=3) 

### distribution taux dinteret ne suit pas une loi normale, on ne peut pas la transformer car il est en %
hist(base_final$taux_interet)

hist_interet<-ggplot(base_final, aes(x=taux_interet)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="interet")+labs(y="densité")+ labs(title="distribution interet")
hist_interet

#### la distribution de la variation du prix des logements suit plus ou moins une loi normale
hist(base_final$vari_logements,main = "hist Prix logements",xlab = "IPL",freq = F)
densite45 <- density(base_final$vari_logements)
lines(densite45, col = "red",lwd=3)

#### hist croissance PIB ne suit pas une loi normale, on peut pas effectuer une transformation logarithmique car c'est deja en %
hist(base_final$croissance_PIB,freq = F)
densite22 <- density(base_final$croissance_PIB)
lines(densite22, col = "red",lwd=3) 




###############################2) statistiques bivariees 

#### on remarque que la relation entre le chomage et l'inflation est negative---> courbe de Phillips 
plot(base_final$chomage,base_final$inflation)
scatterplot(inflation~chomage,boxplots = F, regLine=F, xlab = "chomage",ylab =   "inflation" ,data=base_final, main="courbe de Phillips")


### on remarque une relation non linéaire mais plutot en forme de U --> model quadratique : ajouter lsalaire^2 ??? a voir
plot(base_final$inflation,base_final$lsalaire)
scatterplot(inflation~lsalaire,regLine = F,boxplots = F,data=base_final)



###  pas de lien significative entre inflation et taux d'intret... A voir
scatterplot(inflation~taux_interet,regLine=F, boxplots=F,smooth=F, data=base_final,xlab= " taux interet ",ylab =  "inflation", main= " inflation=f(taux_interet)")



#### on remarque que le lien entre l'inflation et la variation des prix des importations n'est pas linéaire ..
scatterplot(inflation~vari_importation,regLine=F, boxplots=F,smooth=F, data=base_final,xlab = "vari_importation",main="inflation=f(vari_importation)
          ")


#### la variation des prix des logements et linflation sont positivement correlés, forte correlation
scatterplot(inflation~vari_logements,regLine=T, boxplots=F, data=base_final)


#### relation non lineaire entre vari_MP et inflation... A voir, vari_MP^2 ??
scatterplot(inflation~vari_MP,regLine=F, boxplots=F, data=base_final) 


## relation coirssance PIB plutot croissante , PIB^2 a ajouter ??
scatterplot(inflation~croissance_PIB,regLine=F, boxplots=F, smooth=T ,data=base_final, xlab = "croissance_PIB", ylab = "inflation", main="inflation=f(croissance pib)")


rlang:: last_error()





#### matrice de correlation 
#### la correlation est significative entre croissane_PIB et vari_logements 
#### la correlation esr significative entre le taux d'interet et lsalaire
 
mcor<-rcorr.adjust(base_final[,c("inflation","chomage","vari_importation","vari_MP","vari_logements","lsalaire","taux_interet","croissance_PIB")], 
                   type="spearman") 
mcor


##### test significaivite de correlation

multi1<-rcorr(base_final$vari_logements,base_final$croissance_PIB,type = "spearman")
multi2<-rcorr(base_final$taux_interet,base_final$salaire,type = "spearman")
multi12<-cor(base_final$vari_logements,base_final$croissance_PIB)
multi22<-cor(base_final$taux_interet,base_final$lsalaire)



##### on rejette H0, l'inflation et le chomage sont significativement correlés
rcorr(base_final$inflation,base_final$chomage , type = "spearman")


### A priori pas de correlation significative entre inflation et var_importation
rcorr(base_final$inflation,base_final$vari_importation , type = "spearman")



### A correlation significative entre inflation et var_MP
rcorr(base_final$inflation,base_final$vari_MP, type = "spearman")


#### la correlation entre inflation et vari_logements est significative
rcorr(base_final$inflation,base_final$vari_logements, type = "spearman" )



### la correlation n est  pas significative  entre inflation et lsalaire
rcorr(base_final$inflation,base_final$lsalaire)

## la correlation est  pas significative entre inflation et taux_interet
rcorr(base_final$inflation,base_final$taux_interet, type = "spearman")


### la correlation entre croissance PIB reel et inflation est non significative
rcorr(base_final$inflation,base_final$croissance_PIB,type = "spearman")





################## ###############################Regression linéaire ###########
reg3<-lm(inflation~ chomage + taux_interet+ vari_logements + croissance_PIB + lsalaire + vari_MP  + vari_importation
         , base_final)
summary(reg3)
vif(reg3)


## regression avec taux_interet :
### taux_interet n'explique pas inflation car P_value >5%, aussi correlation non significative cf.mcorr.
reg45<-lm(inflation~ taux_interet ,base_final)
summary(reg45)


### on supprimant la variable taux d'interet qui n'est pas significativement corrélé et qui na pas de relation a priori avec l'inflation cf scatterplot :
### model plus significatif globalement, coefficients lsalaire significatif car purgée de l'effet de taux_interet cf.rccor adjust , R2 ajusté augemnte

reg3<-lm(inflation~ chomage + vari_logements + croissance_PIB + lsalaire + vari_MP  + vari_importation
         , base_final)
summary(reg3)
vif(reg3)

### regression avec PIB et PIB^2, PIB pas significatif 
base_final2$croissance_PIB2<-base_final2$croissance_PIB^2
lm566<-lm(inflation~  croissance_PIB+ croissance_PIB2,base_final2)
summary(lm566)
vif(lm566)

#### on peut essayer de  supprimer l'interception B0 , sauf que les coefficients seront baisées,
####probleme de multicolinéarité et enfin residus trés aléatoire ( pas autour de 0)



#### test normalité des residus ( reg3) 
#### on rejette H0 les residus ne sont pas normalement distribués : cf.hist + p-value<5%
hist(residuals(reg3),main = "hist residus",xlab = "residus",ylab = "frequence", freq = F)
densite1 <- density(residuals(reg3))
lines(densite1, col = "red",lwd=3) 
shapiro.test(residuals(reg3))


#### graphs residus : 
plot(residuals(reg3), main="residus")
plot(predict(reg3),residuals(reg3))


#### test valeurs infuentes
base_final$rstudent=rstudent(reg3)
base_final$test_rs[base_final$rstudent <(-2) | base_final$rstudent >2 ]<-"TRUE"
base_final$test_rs[base_final$rstudent <(2) & base_final$rstudent >(-2) ]<-"FALSE"
plot(base_final$rstudent, main = "valeurs rstudent", xlab = "index", ylab = "rstudent")
table(base_final$test_rs)
ggplot(base_final, aes(x=index(base_final$rstudent), y=base_final$rstudent)) + geom_point(colour = "deepskyblue",size=4)+ labs(x= "index", y= "rstudent", title = "rstudent")
                      


##### distance de cook
base_final$dcook=cooks.distance(reg3)
base_final$test_cook<-base_final$dcook>1
plot(base_final$dcook)
table(base_final$test_cook)
### supression des obsevations influantes

base_final2 <- subset(base_final, test_rs!=TRUE)
base_final2<-subset(base_final2, test_cook!=TRUE)






### MODEL beaucoup mieux ajusté apres avoir supprimé les observations influantes : lsalaire beaucoup plus significative, vari_MP aussi
#### R2 ajuste plus grand, p-value F statistique plus faible, residual standar error plus faible

reg5<-lm(inflation~  chomage + vari_logements + croissance_PIB + lsalaire + vari_MP  + vari_importation 
         , base_final2)
summary(reg5)
vif(reg5)


### test homoscedasticité 
### abscence d'heteroscédasticité car p-value>5%
bptest(reg5,~fitted(reg5)+I(fitted(reg5)^2))
bptest(reg5)

### test auto-correlation 
### pas d'auto-corelation des residus p-value>5% 
dwtest(reg5) 


##TEST DE RAINBOW H0 verifiée:le modèle est correctement specifie comme linéaire
raintest(reg5)

### normalité des residus : H0  verifieé les residus sont normalement distribués 
hist(residuals(reg5), freq = F)
shapiro.test(residuals(reg5))
densite2 <- density(residuals(reg5))
lines(densite2, col = "red",lwd=3) 

### rien n'explique le fait que croissance_PIB et vari_importation sont non significatifs, regardons la specification du modele :
### on remarque que vari_importation n'explique pas l'inflation : p-value>5%, R2 faible= 0.07 , residual standar error grand = 0.78


reg56<-lm(inflation~ vari_importation ,base_final2)
summary(reg56)

### de meme pour croissance_PIB : p_value>5%, R2 faible = 0.06, residual standart error grand = 0.80
reg66<-lm(inflation~ croissance_PIB,base_final)
summary(reg66)

#### on supprime les variables vari_ importation et croissance_PIB car au debut nous n'avons pas vraiment remarquer une relation avec inflation cf scatterplot,
##cf.mcorr  pas() de correlation significative aussi, ajustement faible.






########################################### modele 2 ( sans valeurs inflentes)
reg6<-lm(inflation~  chomage + vari_logements  + lsalaire + vari_MP   , base_final2)
summary(reg6)
vif(reg6)
#### R2 ajusté fort : 0.80, significatif globalement : p-value>5%, residuals standar error faible : 0.35

####; test de spécification du model : p-value>5% on accepte H0 model bien spécifié:
resettest(reg6)

### restestons la correlation : 

### correlation significative p-value<5%: 
rcorr(base_final2$inflation,base_final2$lsalaire)
rcorr(base_final2$inflation,base_final2$vari_MP,type="spearman")
rcorr(base_final2$inflation,base_final2$chomage)
rcorr(base_final2$inflation,base_final2$vari_logements)


#### pas de multicolinéarité :
vif(reg6)


#### MODEL globalement significative car p-value<1% et R2 ajusté = 0.80
#### MODEL bien spécifié et robuste car : p-value >5% :

### pas d'autocorrelation
dwtest(reg6)

### model linéaire, il s'adapte bien aux données : p-value>5%
raintest(reg6)

###residus normalement distibués : cf.graph et p-value>5%
hist(residuals(reg6),main = "hist residus")
densite <- density(residuals(reg6))
lines(densite, col = "red",lwd=3) 
shapiro.test(residuals(reg6))
hist_resi<-ggplot(reg6, aes(x=residuals(reg6))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="résidus")+labs(y="densité")+ labs(title="distribution résidus")
hist_resi
#### homoscédastiité H0 acceptée : p-value>5%

bptest(reg6,~fitted(reg6)+I(fitted(reg6)^2))
  bptest(reg6)
plot(predict(reg6),residuals(reg6),main = "nuage de points résidus")








######################################################### model2 avant suppresion des valeurs inflentes 
reg7<-lm(inflation~ chomage+ vari_logements + lsalaire + vari_MP, base_final)
summary(reg7)
vif(reg7)

### normalité des résidus : pas du tout normalement ditribués
hist(residuals(reg7),main = "hist residus")
densite7 <- density(residuals(reg7))
lines(densite7, col = "red",lwd=3) 
shapiro.test(residuals(reg7))
hist_resi7<-ggplot(reg7, aes(x=residuals(reg7))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="résidus")+labs(y="densité")+ labs(title="distribution résidus")
hist_resi7

### test homoscedasticité : H0 accepté
bptest(reg7,~fitted(reg7)+I(fitted(reg7)^2))
bptest(reg7)
plot(predict(reg7),residuals(reg7),main = "nuage de points résidus")



##### test auto-corrélation :H0 accepté
dwtest(reg7)


#### test linérité du model :H0 accepté
raintest(reg7)

resettest(reg7)













#############################################################" modele 1

#### regression courbe de phillips
regphillips0<-lm(inflation~ chomage + lsalaire,base_final)
summary(regphillips0)





#######regression linéaire courbe de phillips apres suppresion de valeurs influantes
### on remarque que lsalaire devient significatif a 1%, ajusted R plus grand, résiduals standar error plus faible


regressionphillips<-lm(inflation ~ chomage + lsalaire,base_final2)
summary(regressionphillips)

vif(regressionphillips)


###. modele bien specifié p>5 %
resettest(regressionphillips)

#### test linérité du model :H0 accepté
raintest(regressionphillips)

### normalité des résidus
hist(residuals(regressionphillips))
shapiro.test(residuals(regressionphillips))
densite3 <- density(residuals(regressionphillips))
lines(densite3, col = "red",lwd=3)

hist_r<-ggplot(regressionphillips, aes(x=residuals(regressionphillips))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="résidus")+labs(y="densité")+ labs(title="distribution résidus")
hist_r


##### test auto-corrélation :H0 accepté
dwtest(regressionphillips)

#### homoscédastiité H0 acceptée : p-value>5%

bptest(regressionphillips,~fitted(regressionphillips)+I(fitted(regressionphillips)^2))

plot(residuals(regressionphillips),main = "nuage de points résidus")










############ modele 1 avant suppresion de valeurs infuentes
### on remarque une que lsalaire est significatif a 5%
regressionphillips1<-lm(inflation~ chomage + lsalaire, base_final)
summary(regressionphillips1)

### normalité des résidus : pas du tout normalement ditribués
hist(residuals(regressionphillips1),main = "hist residus")
densite8 <- density(residuals(regressionphillips1))
lines(densite8, col = "red",lwd=3) 
shapiro.test(residuals(regressionphillips1))
hist_resi8<-ggplot(regressionphillips1, aes(x=residuals(regressionphillips1))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+labs(x="résidus")+labs(y="densité")+ labs(title="distribution résidus")
hist_resi8

### test homoscedasticité : H0 accepté
bptest(regressionphillips1,~fitted(regressionphillips1)+I(fitted(regressionphillips1)^2))
plot(predict(regressionphillips1),residuals(regressionphillips1),main = "nuage de points résidus")



##### test auto-corrélation :H0 accepté
dwtest(regressionphillips1)


#### test linérité du model :H0 accepté
raintest(regressionphillips1)


#### reset test
resettest(regressionphillips1)





