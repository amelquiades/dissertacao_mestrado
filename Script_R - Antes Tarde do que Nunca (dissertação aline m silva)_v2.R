#Primeiro script. Testes

#Upload de bancos de dados
library(readxl)
BD <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/BDmand.xlsx")
desvio_m <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/desvio.xlsx")
economic <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/economic1v2.xlsx")
ideologia <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/ideologia_mod.xlsx")
popularidade_m <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/popularidade_modv2.xlsx")
eleicao <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/eleicaodat.xlsx")
eleicao1<- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/eleicv2.1.xlsx")
eventos <- read_excel("C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/eventos1v2.xlsx")

require(chron)
origin=c(1,1,1995)

#Gera??o de datas - VD
BD$dtini <- julian(BD$mini, BD$dini, BD$aini)
BD$dtfim <- julian(BD$mfim, BD$dfim, BD$afim)

economic$dtecon<- julian(economic$mes, economic$dia, economic$ano)
ideologia$dtideo <- julian(ideologia$mes, ideologia$dia, ideologia$ano)
popularidade_m$dtpop <- julian(popularidade_m$mes, popularidade_m$dia, popularidade_m$ano)
desvio_m$dtdesv <- julian(desvio_m$mes, desvio_m$dia, desvio_m$ano)
eleicao1$dteleicao1 <- julian(eleicao1$mes, eleicao1$dia, eleicao1$ano)
eleicao$dteleicao <- julian(eleicao$mes, eleicao$dia, eleicao$ano)
eleicao$dtdatel <- julian(eleicao$meselei, eleicao$diaelei, eleicao$anoelei)
eleicao$dtdatimp <- julian(eleicao$meseleim, eleicao$diaeleim, eleicao$anoeleim)
eventos$dteventos <- julian(eventos$mes, eventos$dia, eventos$ano)

###tmerge
aa<-tmerge(BD,BD, id=id,tstart = dtini, tstop = dtfim, endpt=event(dtfim,status))
a2<-tmerge(aa,desvio_m, id=id,tamanho=tdc(dtdesv,tamanho),prop_cam=tdc(dtdesv,proporc_cam),prop_gab=tdc(dtdesv,proporc_gab),desvio=tdc(dtdesv,desvio),propcam=tdc(dtdesv,proporcao),propgab=tdc(dtdesv,gabinete),desvio_sempres=tdc(dtdesv,des_sempres))
a3<- tmerge(a2,ideologia, id=id, ideo1417=tdc(dtideo,ideo_inter1417),ideo1017=tdc(dtideo,ideo_interp1017))
a4<- tmerge(a3,economic, id=id,pibtri=tdc(dtecon,pib_tri),pibacum=tdc(dtecon,pib_acum4),infla3=tdc(dtecon,inflacao_3m),infla12=tdc(dtecon,inflacao_12m))
a5<- tmerge(a4,popularidade_m, id=id,pop=tdc(dtpop,pop_ib_otimo),poplb=tdc(dtpop,pop_lb))
a6<- tmerge(a5,eventos, id=id, ecrit=tdc(dteventos,event_crit))

a6$mandato <- ifelse(a6$aini >= 1999 & a6$aini <=2002, 1,
                     ifelse(a6$aini >= 2007 & a6$aini <= 2010, 1,
                            ifelse(a6$dtini >= 16436 & a6$dtfim <= 16932, 1, 0)))
a6$eleicao <- ifelse(a6$tstart >= 10227 & a6$tstop <= 10591, 1,
                     ifelse(a6$tstart >= 11688 & a6$tstop <= 12052, 1,
                            ifelse(a6$tstart >= 13149 & a6$tstop <= 13513, 1,
                                   ifelse(a6$tstart >= 14610 & a6$tstop <= 14974, 1,
                                          ifelse(a6$tstart >= 16071 & a6$tstop <= 16435, 1,0)))))
a6$eleicaoimp <- ifelse(a6$tstart >= 10227 & a6$tstop <= 10591, 1,
                        ifelse(a6$tstart >= 11688 & a6$tstop <= 12052, 1,
                               ifelse(a6$tstart >= 13149 & a6$tstop <= 13513, 1,
                                      ifelse(a6$tstart >= 14610 & a6$tstop <= 14974, 1,
                                             ifelse(a6$tstart >= 16071 & a6$tstop <= 16435, 1,
                                                    ifelse(a6$tstart >= 16801 & a6$tstop <= 16932, 1, 0))))))


data.final<- tmerge(a6,eleicao, id=id, mandato1=tdc(dteleicao, mandato1), eleicao1=tdc(dteleicao, eleicao1), eleicaoimp1=tdc(dteleicao, eleicaoimp1), diaseleicao=tdc(dteleicao, dtdatel), diaseleicaoimp=tdc(dteleicao, dtdatimp))

data.final$dias_eleicao[data.final$dias_eleicao < 0] <- 0
data.final$dias_eleicaoimp[data.final$dias_eleicaoimp<0] <- 0

data.final<- tmerge(data.final,eleicao1, id=id,dias_eleicao=tdc(dteleicao1,dias_eleic), dias_eleicaoimp=tdc(dteleicao1,dias_elei_imp))

data.final$datini<-as.Date(data.final$tstart, origin=as.Date("1970/01/01"))
data.final$datend<-as.Date(data.final$tstop, origin=as.Date("1970/01/01"))

data.final$diaseleicao<- as.Date(data.final$diaseleicao, origin=as.Date("1970/01/01"))
data.final$diaseleicaoimp<- as.Date(data.final$diaseleicaoimp, origin=as.Date("1970/01/01"))
#data.final$datend<- format(data.final$datend, "%d-%m-%Y")
#data.final$diaseleicao<- format(data.final$diaseleicao, "%d-%m-%Y")
#data.final$diaseleicaoimp<- format(data.final$diaseleicaoimp, "%d-%m-%Y")

###V?rias vari?veis foram exclu?das e recolocadas nesse processo de merge

#library(lubridate)
data.final$diaseleicao<- as.numeric(difftime(data.final$diaseleicao ,data.final$datini , units = c("days")))
data.final$diaseleicaoimp<- as.numeric(difftime(data.final$diaseleicaoimp ,data.final$datini , units = c("days")))
data.final$diaseleicao[data.final$diaseleicao < 0] <- 0
data.final$diaseleicaoimp[data.final$diaseleicaoimp<0] <- 0

#Cria??o de vari?vel de tempo total de observa??o
data.final$tempo_tri<-(data.final$tstop-data.final$tstart)
data.final$tempo_total <- (data.final$stop - data.final$dtini)

#Indica??o de vari?vel e label
data.final$eleicao1<-factor(data.final$eleicao1, labels = c("N?o","Sim"))
data.final$eleicaoimp1<-factor(data.final$eleicaoimp1, labels = c("N?o","Sim"))
data.final$mandato1<-factor(data.final$mandato1, labels = c("N?o","Sim"))

#Summary
summary(data.final)

#Excluir missings da contagem.
data.nomissing<-data.final[complete.cases(data.final[,c("desvio","pop","dias_eleicao","desvio_sempres")]),]
excluir<- c("datend")
data.final<- data.final[,!(names(data.final)%in% excluir)]
View(data.final)
View(data.nomissing)

#fun??o densidade
d <- density(data.nomissing$pibtri, na.rm=TRUE)
plot(d)

#quantil
quantile(data.nomissing$pibtri, na.rm=TRUE)
#boxplot e hist
boxplot(data.final$dias_eleicao, data=data.final,main="PIB Trimestral",ylab="Y",xlab="X")
par(mfrow=c(3,2))
hist(data.final$pop, breaks=12, main = "Histograma do Desvio",ylab="Frequ?ncia",xlab = "Desvio")

###Save
write.csv(data.final, "C:/Users/amelq/Dropbox/MESTRADO/Disserta??o/Dados/BD/dissertacao_aline1.csv")

###############################################################################
#######################################################################################


#In?cio do segundo Script
dados<- read.csv("dissertacao_aline1.csv", header = TRUE)
View(dados)


#Cria??o de categorias para vari?veis
summary(dados$taxa_desvio)
quantile(dados$taxadesvio)
dados$desvio_cat <- ifelse(dados$desvio > 0.31 & dados$desvio <= 1.5438881, 1,
                           ifelse(dados$desvio > 1.5438881 & dados$desvio <= 2.2099448, 2,
                                  ifelse(dados$desvio > 2.2099448 & dados$desvio <= 3.5094340, 3,
                                         ifelse(dados$desvio > 3.509434 & dados$desvio <=6.6498741, 4, NA))))


dados$desvio_cat<-factor(dados$desvio_cat, labels = c("Sobrerepresentado","M?dia Representa??o","Subrepresentado","Baix?ssima representa??o"))
###
#id para presidente/desvio/pop/pib....
dados$idpres <- ifelse(dados$presidente=="fhc", 1,
                       ifelse(dados$presidente == "lula", 2,
                              ifelse(dados$presidente=="dilma", 3,
                                     ifelse(dados$presidente=="temer", 4, NA))))
dados$idpres<-factor(dados$idpres, labels = c("FHC","Lula","Dilma","Temer"))

summary(dados$desvio)
quantile(dados$desvio)
dados$desvio_cat <- ifelse(dados$desvio > 0.3180516 & dados$desvio <= 1.5438881, 1,
                           ifelse(dados$desvio > 1.5438881 & dados$desvio <=  2.2099448, 2,
                                  ifelse(dados$desvio >  2.2099448 & dados$desvio<= 3.5094340, 3,
                                         ifelse(dados$desvio > 3.5094340 & dados$desvio <=6.6498741, 4, NA))))

summary(dados$ideo1017)
quantile(dados$ideo1017)
dados$ideo_cat <- ifelse(dados$ideo1017 > 0.0025 & dados$ideo1017 <= 0.7525, 1,
                         ifelse(dados$ideo1017 > 0.7525 & dados$ideo1017 <= 2.37, 2,
                                ifelse(dados$ideo1017 > 2.37 & dados$ideo1017<= 3.4775, 3,
                                       ifelse(dados$ideo1017 > 3.4775 & dados$ideo1017 <=4.85, 4, NA))))

quantile(dados$pop)
dados$pop_cat <- ifelse(dados$pop > 3 & dados$pop <= 27, 1,
                        ifelse(dados$pop > 27 & dados$pop <= 42, 2,
                               ifelse(dados$pop > 42 & dados$pop<= 56, 3,
                                      ifelse(dados$pop > 56 & dados$pop <= 77, 4, NA))))

quantile(dados$pibtri, na.rm = T)
dados$pib_cat <- ifelse(dados$pibtri > -5.6 & dados$pibtri <= 0.4, 1,
                        ifelse(dados$pibtri > 0.4 & dados$pibtri <= 2.5, 2,
                               ifelse(dados$pibtri > 2.5 & dados$pibtri<= 4.7, 3,
                                      ifelse(dados$pibtri > 4.7 & dados$pibtri <= 9.2, 4, NA))))

quantile(dados$infla3)
dados$infla_cat <- ifelse(dados$infla3 > -0.850 & dados$infla3 <= 1.05, 1,
                          ifelse(dados$infla3 > 1.05 & dados$infla3 <= 1.420, 2,
                                 ifelse(dados$infla3 > 1.420 & dados$infla3<= 1.990, 3,
                                        ifelse(dados$infla3 > 1.990 & dados$infla3 <= 7.540, 4, NA))))

quantile(dados$ecrit)
dados$ecrit_cat <- ifelse(dados$ecrit > 0 & dados$ecrit<= 0.375, 1,
                          ifelse(dados$ecrit > 0.375 & dados$ecrit <= 0.500, 2,
                                 ifelse(dados$ecrit > 0.500 & dados$ecrit<= 0.750, 3,
                                        ifelse(dados$ecrit > 0.750 & dados$ecrit <= 2.800, 4, NA))))

quantile(dados$dias_eleicao)
dados$anos_mandato <- ifelse(dados$dias_eleicao > 1095 & dados$dias_eleicao<= 1460, 4,
                             ifelse(dados$dias_eleicao > 730 & dados$dias_eleicao <= 1095, 3,
                                    ifelse(dados$dias_eleicao > 365 & dados$dias_eleicao<= 730, 2,
                                           ifelse(dados$dias_eleicao > 0 & dados$dias_eleicao <= 365, 1, NA))))


#lagged variable
lg <- function(x)c(NA, x[1:(length(x)-1)])
dados$lagpib<- unlist(tapply(dados$pibtri, dados$id, lg))
dados$lagpop<- unlist(tapply(dados$pop, dados$id, lg))
dados$laginfla<- unlist(tapply(dados$infla3, dados$id, lg))
dados$lageventos<- unlist(tapply(dados$ecrit, dados$id, lg))

#Save
library(openxlsx)
write.xlsx(dados , file =" dados_aline.xslx ")
#p/ Mudança manual do PFL para ano de eleição Sim na saída.
dados <- read.xlsx(xlsxFile = "dados_aline .xslx", detectDates =
                     T, colNames = T, rowNames = T)
write.csv(dados , "C:/ Users/amelq/Dropbox/MESTRADO/Dissertação/Dados/BD/dissertacao_aline1.csv")

########################################################################################
############################################################################################

#An?lise de sobreviv?ncia
#SurvObj
library(survival)
library(KMsurv)
library(survminer)

#Cria??o de coluna com start, end e status
dados$SurvObj <- with(dados, Surv(tstart,tstop,endpt == 1))
View(dados$SurvObj)


#Tabela de sobreviv?ncia e plot (diferen?a entre duas curvas (survdiff))

sfit <- survfit(Surv(tempo_total,endpt)~1, data=dados)
xtable(survfit(Surv(tempo_total,endpt)~1, data=dados))
range(dados$tempo_total)
seq(0,1460,365)
summary(sfit, times=seq(0,1460,365))
plot(sfit, mark.time=T, conf.int = T, lwd=2 ,xlab="Total de Dias de Mandato",
     ylab = "Probabilidade de Sobreviv?ncia(%)", ymin = 0.97, yscale = 100)
title("Fun??o de Sobrevida (Kaplan Meier)
      para o mandato presidencial")
abline(h=(seq(0.97,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

sfit_ano<- survfit(Surv(tstart,tstop,endpt)~1, data=dados)
summary(sfit_ano,times=seq(9131,17531,365))
plot(sfit_ano, mark.time=T, conf.int = T, lwd=2,firstx = 9131, xlab="Anos",
     ylab = "Probabilidade de Sobreviv?ncia", xscale = 365,xmax = 17531)
title("Fun??o de Sobrevida (Kaplan Meier)
      entre 1995 - 2017")
abline(h=(seq(0.0,1.0,0.2)), col="lightgray", lty="dotted")
abline(v=(seq(9131,17531,365)), col="lightgray", lty="dotted")

#Para ano de elei??o
kapeleicao<-survfit(Surv(tempo_total,endpt)~eleicao1,data=dados)
summary(kapeleicao, times=seq(0,1460,365))
plot(kapeleicao,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia(%)",mark.time=T,
     yscale = 100,lty=c(2,1), lwd=2,ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c("N?o","Sim"),lty=c(2,1), lwd=2,title="Ano de Elei??o?",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      para Ano de Elei??o")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

kapeleicaoimp<-survfit(Surv(tempo_total,endpt)~eleicaoimp1,data=dados)
summary(kapeleicaoimp, times=seq(0,1460,365))
plot(kapeleicaoimp,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia(%)",mark.time=T,
     yscale = 100,lty=c(2,1), lwd=2,ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c("N?o","Sim"),lty=c(2,1), lwd=2,title="Ano de Elei??o?",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      para Ano de Elei??o ou Impeachment")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_tri, endpt) ~ eleicaoimp1, data=dados) # log-rank test

#mandato
kapmandato<-survfit(Surv(tempo_total,endpt)~mandato1,data=dados)
summary(kapmandato,times=seq(0,1460,365))
plot(kapmandato,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia(%)",mark.time=F,yscale = 100,
     lty=c(2,1), lwd=2,ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c("N?o","Sim"),lty=c(2,1),lwd=2,title="Segundo Mandato?",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      para 1? e 2? Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ mandato1, data=dados) # log-rank test

#Presidente
kappres<-survfit(Surv(tempo_total,endpt)~idpres,data=dados)
summary(kappres)
plot(kappres,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=T,lty=c(1),lwd=2,
     col=c("chartreuse3","blue3","red2","black"),ymin = 0.95, xscale=365)
legend(x="bottomleft",legend=c("Dilma","FHC","Lula","Temer"),lty=c(1),lwd=2,
       col=c("chartreuse3","blue3","red2","black"),title="Presidente",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      dos Presidente/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ idpres, data=dados) # log-rank test

#Desvio
kapdesvio<-survfit(Surv(tempo_total,endpt)~desvio_cat,data=dados)
summary(kapdesvio)
plot(kapdesvio,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c("1 (>0.3181 - <=1.5439)","2 (>1.5439 - <= 2.2099)",
                               "3 (> 2.2099 - <= 3.5094)","4 (>3.5094 - <= 6.6499)"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Desvio",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pelo Desvio/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ desvio_cat, data=dados) # log-rank test

#ideologia
kapideo<-survfit(Surv(tempo_total,endpt)~ideo_cat,data=dados)
summary(kapideo)
plot(kapideo,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.95,xscale=365)
legend(x="bottomleft",legend=c(">0.002 - <=0.752",">0.752 - <=2.37",
                               ">2.37 - <= 3.477",">3.477 - <= 4.85"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Dist?ncia Id?ologica",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pela Ideologia/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ ideo_cat, data=dados) # log-rank test

#Popularidade
kappop<-survfit(Surv(tempo_total,endpt)~pop_cat,data=dados)
summary(kappop)
plot(kappop,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.95, xscale = 365)
legend(x="bottomleft",legend=c(">3 - <=27",">27 - <=42",
                               ">42 - <= 56",">56 - <= 77"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Popularidade",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pela Popularidade/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ pop_cat, data=dados) # log-rank test

#PIB
kappib<-survfit(Surv(tempo_total,endpt)~pib_cat,data=dados)
summary(kappib)
plot(kappib,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c(">-5.6 - <=0.4",">0.4 - <=2.5",
                               ">2.5 - <= 4.7",">4.7 - <= 9.2"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Varia??o do PIB",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pelo PIB/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ pib_cat, data=dados) # log-rank test

#Infla??o
kapinfla<-survfit(Surv(tempo_total,endpt)~infla_cat,data=dados)
summary(kapinfla)
plot(kapinfla,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.95,xscale = 365)
legend(x="bottomleft",legend=c(">-0.850 - <=1.05",">1.05 - <=1.42",
                               ">1.42 - <= 1.99",">1.99 - <= 7.54"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Taxa de Infla??o",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pela Infla??o/Anos de Mandato")
abline(h=(seq(.95,100,0.005)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ infla_cat, data=dados) # log-rank test

#eventos
kapecrit<-survfit(Surv(tempo_total,endpt)~ecrit_cat,data=dados)
summary(kapecrit)
plot(kapecrit,xlab="Anos",ylab="Probabilidade de Sobreviv?ncia",mark.time=F,lty=c(1),lwd=2,
     col=c("blue3","red2","chartreuse3","black"),ymin = 0.93,xscale = 365)
legend(x="bottomleft",legend=c(">0 - <=0.375",">0.375 - <=0.500",
                               ">0.500 - <= 0.750",">0.750 - <= 2.800"),
       lty=c(1),lwd=2,col=c("blue3","red2","chartreuse3","black"),
       title="Eventos Cr?ticos",bty="n")
title("Fun??o de Sobrevida (Kaplan Meier)
      pelos Eventos Cr?t./Anos de Mandato")
abline(h=(seq(.93,100,0.01)), col="lightgray", lty="dotted")
abline(v=(seq(0,1460,365)), col="lightgray", lty="dotted")

survdiff(Surv(tempo_total, endpt) ~ ecrit_cat, data=dados) # log-rank test


#Kaplan: Cumulativo
#kaplan1<-survfit(formula=Surv(tstart,tstop,endpt)~1, data=dados)
#plot(kaplan1, conf.int = F,fun = "cumhaz", mark.time=F, firstx = 9131, 
#xlab="Dias", xscale = 365, yscale = 100)


############**Erro nos res?duos score quando tem missing no modelo.
#No missing
dados2<-dados[complete.cases(dados[,c("desvio","pop","dias_eleicao","pibtri")]),]
View(dados2)
summary(dados2)

#Modelos

modelo1<- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1417,data = dados2)
summary(modelo1)
modelo2<- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1417+pibtri+infla3,data = dados2)
summary(modelo2)
modelo3<- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1417+pibtri+infla3+ecrit+dias_eleicao,data = dados2)
summary(modelo3)
anova(modelo1,modelo2,modelo3)

modelo2.1<- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1017+pop+ecrit,data = dados2)
summary(modelo2.1)
modelo3.1<- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1017+pop+dias_eleicao*tamanho,data = dados2)
summary(modelo3.1)
anova(modelo1,modelo2.1,modelo3.1)

#Modelos agrupados por id (erros padr?o robusto)

#sig:desvio (contr?rio), PIB e Dias para eleicao
modelo4 <- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1017+ecrit+pibtri+infla3+dias_eleicao+cluster(id),data = dados2)
summary(modelo4)

#Modelos com hip?tese interativa

#sig: desvio (contr?rio) e dias para elei??o
modelo5 <- coxph(Surv(tstart,tstop,endpt)~desvio+ideo1417+ecrit+pop+dias_eleicao*tamanho+cluster(id),data = dados2) 
summary(modelo5)

#sig:desvio (contr?rio), pib (contr?rio), infla??o, e interativa  (sem cluster nenhum sig)
modelo6 <- coxph(Surv(tstart,tstop,endpt)~pibtri+infla3+dias_eleicao*mandato1+cluster(id),data = dados2) 
summary(modelo6)

#Modelo com vari?veis defasadas em 1 trimestre
modelo7<- coxph(Surv(tstart,tstop,endpt)~desvio_cat+lageventos+lagpib+laginfla+lagpop+cluster(id),,data = dados2)
summary(modelo7) #Sem cluster n?o tem sig.

X<- is.na(dados2$dias_eleicao) | is.nan(dados2$dias_eleicao) | is.infinite(dados2$dias_eleicao)
X

#Res?duo de Schoenfeld - 
resid_m1<- cox.zph(modelo1, transform = "identity")
resid_m1
par(mfrow=c(3,2))
plot(resid_m1[1], main="Desvio") 
abline(h=0,lty=2,col=2)
plot(resid_m1[2], main="Ideologia (14-17)") 
abline(h=0,lty=2,col=2)

modelo2$call
resid_m2<- cox.zph(modelo2, transform = "identity")
resid_m2
par(mfrow=c(3,2))
plot(resid_m2[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m2[2], main="Ideologia(14-17)")
abline(h=0,lty=2,col=2)
plot(resid_m2[3], main="PIB")
abline(h=0,lty=2,col=2)
plot(resid_m2[4], main="Infla??o")
abline(h=0,lty=2,col=2)

modelo3$call
resid_m3<- cox.zph(modelo3, transform = "identity")
resid_m3
par(mfrow=c(3,2))  #Desvio um pouco oscilante
plot(resid_m3[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m3[2], main="Ideologia(14-17)")
abline(h=0,lty=2,col=2)
plot(resid_m3[3], main="PIB")
abline(h=0,lty=2,col=2)
plot(resid_m3[4], main="Infla??o")
abline(h=0,lty=2,col=2)
plot(resid_m3[5], main="Eventos Cr?ticos")
abline(h=0,lty=2,col=2)
plot(resid_m3[6], main="Dias para Elei??o")
abline(h=0,lty=2,col=2)

modelo2.1$call
resid_m2.1<- cox.zph(modelo2.1, transform = "identity")
resid_m2.1
par(mfrow=c(3,2))
plot(resid_m2.1[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m2.1[2], main="Ideologia(10-17)")
abline(h=0,lty=2,col=2)
plot(resid_m2.1[3], main="Popularidade")
abline(h=0,lty=2,col=2)
plot(resid_m2.1[4], main="Eventos Cr?ticos")
abline(h=0,lty=2,col=2)

modelo3.1$call #Desvio perde proporcionalidade
resid_m3.1<- cox.zph(modelo3.1, transform = "identity")
resid_m3.1
par(mfrow=c(4,2))
plot(resid_m3.1[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[2], main="Ideologia(10-17)")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[3], main="Popularidade")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[4], main="Eventos Cr?ticos")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[5], main="Dias para Elei??o")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[6], main="Tamanho")
abline(h=0,lty=2,col=2)
plot(resid_m3.1[5], main="Dias p/ Elei??o * Tamanho")
abline(h=0,lty=2,col=2)

modelo4$call
resid_m4<- cox.zph(modelo4, transform = "identity")
resid_m4
par(mfrow=c(3,2))
plot(resid_m4[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m4[2], main="Ideologia(10-17)")
abline(h=0,lty=2,col=2)
plot(resid_m4[3], main="Eventos Cr?ticos")
abline(h=0,lty=2,col=2)
plot(resid_m4[4], main="PIB")
abline(h=0,lty=2,col=2)
plot(resid_m4[5], main="Infla??o")
abline(h=0,lty=2,col=2)
plot(resid_m4[6], main="Dias at? Elei??o")
abline(h=0,lty=2,col=2)

modelo5$call
resid_m5<- cox.zph(modelo5, transform = "identity")
resid_m5
par(mfrow=c(4,2))
plot(resid_m5[1], main="Desvio")
abline(h=0,lty=2,col=2)
plot(resid_m5[2], main="Ideologia(14-17)")
abline(h=0,lty=2,col=2)
plot(resid_m5[3], main="Eventos Cr?ticos")
abline(h=0,lty=2,col=2)
plot(resid_m5[4], main="Popularidade")
abline(h=0,lty=2,col=2)
plot(resid_m5[5], main="Dias at? Elei??o")
abline(h=0,lty=2,col=2)
plot(resid_m5[6], main="Tamanho")
abline(h=0,lty=2,col=2)
plot(resid_m5[7], main="Dias at? Elei??o*Tamanho")
abline(h=0,lty=2,col=2)

modelo6$call
resid_m6<- cox.zph(modelo6, transform = "identity")
resid_m6
par(mfrow=c(3,2))
plot(resid_m6[1], main="PIB")
abline(h=0,lty=2,col=2)
plot(resid_m6[2], main="Infla??o")
abline(h=0,lty=2,col=2)
plot(resid_m6[3], main="Dias at? Elei??o")
abline(h=0,lty=2,col=2)
plot(resid_m6[4], main="Mandato")
abline(h=0,lty=2,col=2)
plot(resid_m6[5], main="Dias at? Elei??o*Mandato")
abline(h=0,lty=2,col=2)

modelo7$call
resid_m7<- cox.zph(modelo7, transform = "identity")
resid_m7
par(mfrow=c(3,2))
plot(resid_m7[1], main="Eventos (lagged)")
abline(h=0,lty=2,col=2)
plot(resid_m7[2], main="PIB (lagged)")
abline(h=0,lty=2,col=2)
plot(resid_m7[3], main="Infla??o (lagged)")
abline(h=0,lty=2,col=2)
plot(resid_m7[4], main="Popularidade (lagged)")
abline(h=0,lty=2,col=2)

#Res?duos de Martingale para modelo

martin_m1<- resid(modelo1,type = "martingale")
plot(martin_m1, xlab = "?ndice", ylab = "Res?duo",main = "Modelo1")
abline(h=0,lty=1, col=2)

martin_m2<- resid(modelo2,type = "martingale")
plot(martin_m2, xlab = "?ndice", ylab = "Res?duo",main = "Modelo2")
abline(h=0,lty=2, col=2)

martin_m3<- resid(modelo3,type = "martingale")
plot(martin_m3, xlab = "?ndice", ylab = "Res?duo",main = "Modelo3")
abline(h=0,lty=2, col=2)

martin_m2.1<- resid(modelo2.1,type = "martingale")
plot(martin_m2.1, xlab = "?ndice", ylab = "Res?duo",main = "Modelo2.1")
abline(h=0,lty=2, col=2)

martin_m3.1<- resid(modelo3.1,type = "martingale")
plot(martin_m3.1, xlab = "?ndice", ylab = "Res?duo",main = "Modelo3.1")
abline(h=0,lty=2, col=2)

martin_m4<- resid(modelo4,type = "martingale")
plot(martin_m4, xlab = "?ndice", ylab = "Res?duo",main = "Modelo4")
abline(h=0,lty=2, col=2)

martin_m5<- resid(modelo5,type = "martingale")
plot(martin_m5, xlab = "?ndice", ylab = "Res?duo",main = "Modelo5")
abline(h=0,lty=2, col=2)

martin_m6<- resid(modelo6,type = "martingale")
plot(martin_m6, xlab = "?ndice", ylab = "Res?duo",main = "Modelo6")
abline(h=0,lty=2, col=2)

martin_m7<- resid(modelo7,type = "martingale")
plot(martin_m7, xlab = "?ndice", ylab = "Res?duo",main = "Modelo7")
abline(h=0,lty=2, col=2)

#Modelo nulo contra vari?vel
par(mfrow=c(2,2))
modelonulo<- coxph(Surv(tstart,tstop,endpt)~1, data=dados2)
resnulo<- resid(modelonulo, type="martingale")
plot(dados2$dias_eleicao,resnulo)
lines(lowess(dados2$dias_eleicao,resnulo,iter = 0),lty=1,col=4)
lines(lowess(dados2$dias_eleicao,resnulo),lty=4, col=2)
legend("bottom",lty=c(1,4),col=c(4,2),legend = c("Com Outlier","Sem Outlier"))

modelonulo<- coxph(Surv(tstart,tstop,endpt)~1, data=dados2)
resnulo<- resid(modelonulo, type="martingale")
plot(dados2$desvio,resnulo)
lines(lowess(dados2$desvio,resnulo,iter = 0),lty=1,col=4)
lines(lowess(dados2$desvio,resnulo),lty=4, col=2)
legend("bottom",lty=c(1,4),col=c(4,2),legend = c("Com Outlier","Sem Outlier"))

modelonulo<- coxph(Surv(tstart,tstop,endpt)~1, data=dados2)
resnulo<- resid(modelonulo, type="martingale")
plot(dados2$ecrit,resnulo)
lines(lowess(dados2$ecrit,resnulo,iter = 0),lty=1,col=4)
lines(lowess(dados2$ecrit,resnulo),lty=4, col=2)
legend("bottom",lty=c(1,4),col=c(4,2),legend = c("Com Outlier","Sem Outlier"))

#M1 Deviance (ponto mais distante 350, 469 e 486)
res.dev<-resid(modelo1,type = "deviance")
plot(res.dev, predict(modelo1))
abline(h=0)
identify(res.dev, predict(modelo1))
#M2 Deviance (ponto mais distante 528)
res.dev2<-resid(modelo2,type = "deviance")
plot(res.dev2, predict(modelo2))
abline(h=0)
identify(res.dev2, predict(modelo2))
#M3
res.dev3<-resid(modelo3,type = "deviance")
plot(res.dev3, predict(modelo3))
abline(h=0)
identify(res.dev3, predict(modelo3))
#m2.1
res.dev2.1<-resid(modelo2.1,type = "deviance")
plot(res.dev2.1, predict(modelo2.1))
abline(h=0)
identify(res.dev2.1, predict(modelo2.1))
#m3.1
res.dev3.1<-resid(modelo3.1,type = "deviance")
plot(res.dev3.1, predict(modelo3.1))
abline(h=0)
identify(res.dev3.1, predict(modelo3.1))
#M4 (ponto mais distante 556,563)
res.dev4<-resid(modelo4,type = "deviance")
plot(res.dev4, predict(modelo4))
abline(h=0)
identify(res.dev4, predict(modelo4))
#M5(ponto mais distante 65)
res.dev5<-resid(modelo5,type = "deviance")
plot(res.dev5, predict(modelo5))
abline(h=0)
identify(res.dev5, predict(modelo5))
#M6(ponto mais distante 549,556,563)
res.dev6<-resid(modelo6,type = "deviance")
plot(res.dev6, predict(modelo6))
abline(h=0)
identify(res.dev6, predict(modelo6))
#M7(ponto mais distante 65)
res.dev7<-resid(modelo7,type = "deviance")
plot(res.dev7, predict(modelo7))
abline(h=0)
identify(res.dev7, predict(modelo7))

#Res?duos Score 
#modelo1
modelo1$call
score_m1<-resid(modelo1,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m1[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1417,score_m1[,2],
     xlab = "Ideologia",ylab = "Res?duos")

#modelo2
modelo2$call
score_m2<-resid(modelo2,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m2[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1417,score_m2[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$pibtri,score_m2[,3],
     xlab = "PIB",ylab = "Res?duos")
plot(dados2$infla3,score_m2[,4],
     xlab = "Infla??o",ylab = "Res?duos")
#modelo3
modelo3$call
score_m3<-resid(modelo3,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m3[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1417,score_m3[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$pibtri,score_m3[,3],
     xlab = "PIB",ylab = "Res?duos")
plot(dados2$infla3,score_m3[,4],
     xlab = "Infla??o",ylab = "Res?duos")
plot(dados2$ecrit,score_m3[,5],
     xlab = "Eventos Cr?ticos",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m3[,6],
     xlab = "Dias para elei??o",ylab = "Res?duos")

#modelo2.1
modelo2.1$call
score_m2.1<-resid(modelo2.1,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m2.1[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1017,score_m2.1[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$pop,score_m2.1[,3],
     xlab = "Popularidade",ylab = "Res?duos")
plot(dados2$ecrit,score_m2.1[,4],
     xlab = "Eventos Crit",ylab = "Res?duos")

#modelo3.1
modelo3.1$call
score_m3.1<-resid(modelo3.1,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m3.1[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1017,score_m3.1[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$pop,score_m3.1[,3],
     xlab = "Popularidade",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m3.1[,4],
     xlab = "Dias Elei??o",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m3.1[,5],
     xlab = "Tamanho",ylab = "Res?duos")

#modelo4
modelo4$call
score_m4<-resid(modelo4,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m4[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1017,score_m4[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$ecrit,score_m4[,3],
     xlab = "Eventos Cr?ticos",ylab = "Res?duos")
plot(dados2$pibtri,score_m4[,4],
     xlab = "PIB",ylab = "Res?duos")
plot(dados2$infla3,score_m4[,5],
     xlab = "Infla??o",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m4[,6],
     xlab = "Dias para Elei??o",ylab = "Res?duos")

#modelo5
modelo5$call
score_m5<-resid(modelo5,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$desvio,score_m5[,1],
     xlab = "Desvio de Representa??o",ylab = "Res?duos")
plot(dados2$ideo1417,score_m5[,2],
     xlab = "Ideologia",ylab = "Res?duos")
plot(dados2$ecrit,score_m5[,3],
     xlab = "Eventos Cr?ticos",ylab = "Res?duos")
plot(dados2$pop,score_m5[,4],
     xlab = "Popularidade",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m5[,5],
     xlab = "Dias para Elei??o",ylab = "Res?duos")
#modelo6
modelo6$call
score_m6<-resid(modelo6,type = "dfbetas")
par(mfrow=c(3,2))
plot(dados2$pibtri,score_m6[,1],
     xlab = "PIB",ylab = "Res?duos")
plot(dados2$infla3,score_m6[,2],
     xlab = "Infla??o",ylab = "Res?duos")
plot(dados2$dias_eleicao,score_m6[,3],
     xlab = "Dias para Elei??o",ylab = "Res?duos")
plot(dados2$mandato1,score_m6[,4],
     xlab = "2? Mandato",ylab = "Res?duos")

