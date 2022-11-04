##################################################
## Project: AR
## Script purpose: Preparar les dades de persistència 
##                 pel congrés. Seguiment 2018:2021
## Date: 2022-09-28
## Author:@s.mendoza
##################################################


# selecció de pacients Naïf resultat de creuar univers de consums  --------

PS.1<-df.cons.presc2 %>% 
  filter(!is.na(`Prescripció ID`)) %>%
  select(Nhc,Cip,Episodi,Medicament.Codi,Medicament.AGRUPAT,Medicament.Descripció,Especialitat..Codi.nacional.,Data.Consum,Quantitat.Ecofin,`Freqüència Descripció`,Posologia.dies,`Data Inici Prescripció`,
         `Data Fi Prescripció`
  ) %>% 
  mutate(
    Data.Fi.Prescripció.V2=replace_na(`Data Fi Prescripció`,today()) #NA a avui per filtrar
  ) %>% 
  filter(Data.Consum>=`Data Inici Prescripció`&
           Data.Consum<=Data.Fi.Prescripció.V2) %>% 
  distinct() %>% 
  select(-Data.Fi.Prescripció.V2) %>% 
  group_by(Nhc) %>% 
  dplyr::mutate(
    Tractament.Naïf=ifelse(n_distinct(Medicament.AGRUPAT)==1,'Si','No') #únic tractament
  ) %>% ungroup() %>% 
  filter(Tractament.Naïf=='Si')

#Creuem amb processos per tenir sexe i edat

PS.processos<-df.proc %>% 
  mutate(NHC..sense.ceros.=str_trim(NHC..sense.ceros.)) %>% 
  select(NHC..sense.ceros.,Sexe,Data.naixement) %>% distinct()


PS.2<-PS.1 %>% left_join(PS.processos,by=c('Nhc'='NHC..sense.ceros.')) %>% 
  group_by(Nhc) %>% 
  dplyr::mutate(Any.inici.tractament=year(first(Data.Consum))) %>% ungroup() %>% 
  dplyr::mutate(Edat.inici.tractament=Any.inici.tractament-year(Data.naixement)) %>% 
  mutate(              
         Grups.edat=case_when(
           Edat.inici.tractament<20 ~'Menors de 20 anys',
           (Edat.inici.tractament>=20 &  Edat.inici.tractament<=40) ~ '20-40 anys',
           (Edat.inici.tractament>40 &  Edat.inici.tractament<=50) ~ '41-50 anys',
           (Edat.inici.tractament>50 &  Edat.inici.tractament<=60) ~ '51-60 anys',
           (Edat.inici.tractament>60 &  Edat.inici.tractament<=70) ~ '61-70 anys',
           (Edat.inici.tractament>70 &  Edat.inici.tractament<=80) ~ '71-80 anys',
           Edat.inici.tractament>80 ~'>80 anys',
           TRUE ~ 'Edat no disponible'
         ) %>% factor(levels = c('Menors de 20 anys','20-40 anys','41-50 anys','51-60 anys',
                                 '61-70 anys','71-80 anys','>80 anys', 'Edat no disponible'))
   ) %>% 
  select(Nhc,Cip,Sexe,Edat.inici.tractament,Grups.edat,everything())




#Número de pacients Naïf:
nrow(PS.2%>% distinct(Nhc)) #976 pacients Naïf

#Número de pacients per fàrmac durant el seguiment
PS.Pac.farm<-PS.2 %>% group_by(Medicament.AGRUPAT) %>% 
  dplyr::summarise(Pacients=n_distinct(Nhc)) %>% ungroup() %>% 
  mutate('%'=round(Pacients/sum(Pacients)*100,1)) %>% 
  bind_rows(summarise_all(.,~if (is.numeric(.)) sum(.) else 'Totals'))

X2_PS.Pac<-chisq.test(table(PS.2$Medicament.AGRUPAT,useNA = 'no'))


PS.Pac.sexe<-PS.2 %>% group_by(Medicament.AGRUPAT,Sexe) %>% 
  dplyr::summarise(Pacients=n_distinct(Nhc)) %>% ungroup() %>% 
  spread(Sexe,Pacients) %>% 
  replace(is.na(.),0) %>% 
  bind_rows(summarise_all(.,~if (is.numeric(.)) sum(.) else 'Totals'))

#Trec l'Apremilast pq no té dades
X2_df<-PS.2 %>% filter(Medicament.AGRUPAT!='APREMILAST')

X2_PS.Pac.sexe<-chisq.test(table(X2_df$Medicament.AGRUPAT,X2_df$Sexe,useNA = 'no'),simulate.p.value = F)


PS.Pac.edat<-PS.2 %>% group_by(Medicament.AGRUPAT,Grups.edat) %>% 
  dplyr::summarise(Pacients=n_distinct(Nhc)) %>% ungroup() %>% 
  arrange(Medicament.AGRUPAT,Grups.edat) %>% 
  spread(Grups.edat,Pacients) %>% 
  replace(is.na(.),0) %>% 
  bind_rows(summarise_all(.,~if (is.numeric(.)) sum(.) else 'Totals'))

X2_PS.Pac.edat<-chisq.test(table(X2_df$Medicament.AGRUPAT,X2_df$Grups.edat,useNA = 'no'),simulate.p.value = F)


# Càlcul de temps en tractament per medicament ----------------------------

PS.3<-PS.2 %>% 
  group_by(Nhc,Medicament.AGRUPAT) %>% 
  arrange(Data.Consum) %>% 
  dplyr::mutate(
    Mesos.desde.primer.consum=time_length(
      interval(start=first(Data.Consum),end =lag(Data.Consum)),unit = 'months'
    ) %>% round(.,2),
    Ordre=row_number(),
    max=last(Ordre),
    Finalitza=ifelse(Ordre==max,TRUE,FALSE)
   ) %>% ungroup() %>%
  mutate(Mesos.desde.primer.consum=ifelse(is.na(Mesos.desde.primer.consum),0,Mesos.desde.primer.consum)) %>% 
  select(-Ordre,-max)



# Kaplan-Meier ------------------------------------------------------------

#Selecciono els més freqüents per fer el KM
library(survminer)
library(survival)

Medicaments.freq<-PS.Pac.farm %>% 
  slice(-nrow(.)) %>% 
  arrange(desc(Pacients)) %>% 
  head(4) %>% 
  select(1)







#comparatiu top 4

PS.3.plot<-PS.3 %>% 
  filter(Medicament.AGRUPAT%in%Medicaments.freq$Medicament.AGRUPAT[1:4]) %>% 
  rename(c('Medicament.AGRUPAT'='Drug')) %>% as.data.frame()
survObject2<-Surv(time =PS.3.plot$Mesos.desde.primer.consum,event =PS.3.plot$Finalitza)
fit2<-survfit(survObject2~Drug,data = PS.3.plot,type='kaplan-meier')

KM_Pers_plot_4<-ggsurvplot(fit2,data = PS.3.plot,
                    conf.int = F,
                    xlab='Time (months)',
                    ylab='Continuation treatment',
                    legend='right',
                    legend.title='Estimates',
                    pval.method = TRUE,
                    pval=TRUE,
                    risk.table = F,
                    surv.median.line = 'hv',
                    ggtheme = theme_light(),
                    break.x.by=(5),
                    cumevents = T,
                    font.x=c(12,'bold','black'),
                    font.y=c(12,'bold','black'),
                    font.tickslab = c(10, "plain", "black"),
                    )



#Diferències estadísticament significatives en la probabilitat de discontinuar el tractament->Log-rank
logrank4<-survdiff(survObject2~Drug,data = PS.3.plot)
pval4 <- 1 - pchisq(logrank4$chisq, length(logrank4$n) - 1) 

#Els dos primers:
  
PS.3.plot<-PS.3 %>% 
  filter(Medicament.AGRUPAT%in%Medicaments.freq$Medicament.AGRUPAT[1:2]) %>% 
  rename(c('Medicament.AGRUPAT'='Drug')) %>% as.data.frame()
survObject2<-Surv(time =PS.3.plot$Mesos.desde.primer.consum,event =PS.3.plot$Finalitza)
fit2<-survfit(survObject2~Drug,data = PS.3.plot,type='kaplan-meier')

KM_Pers_plot_1_2<-ggsurvplot(fit2,data = PS.3.plot,
                             conf.int = F,
                             xlab='Time (months)',
                             ylab='Continuation treatment',
                             legend='right',
                             legend.title='Estimates',
                             pval.method = TRUE,
                             pval=TRUE,
                             risk.table = F,
                             surv.median.line = 'hv',
                             ggtheme = theme_light(),
                             break.x.by=(5),
                             cumevents = T,
                             font.x=c(12,'bold','black'),
                             font.y=c(12,'bold','black'),
                             font.tickslab = c(10, "plain", "black"),
)




#Diferències estadísticament significatives en la probabilitat de discontinuar el tractament->Log-rank
logrank1_2<-survdiff(survObject2~Drug,data = PS.3.plot)
pval1_2 <- 1 - pchisq(logrank1_2$chisq, length(logrank1_2$n) - 1) 


#comparatiu 1 vs 3
PS.3.plot<-PS.3 %>% 
  filter(Medicament.AGRUPAT%in%Medicaments.freq$Medicament.AGRUPAT[c(1,3)]) %>% 
  rename(c('Medicament.AGRUPAT'='Drug')) %>% as.data.frame()
survObject2<-Surv(time =PS.3.plot$Mesos.desde.primer.consum,event =PS.3.plot$Finalitza)
fit2<-survfit(survObject2~Drug,data = PS.3.plot,type='kaplan-meier')

KM_Pers_plot_1_3<-ggsurvplot(fit2,data = PS.3.plot,
                             conf.int = F,
                             xlab='Time (months)',
                             ylab='Continuation treatment',
                             legend='right',
                             legend.title='Estimates',
                             pval.method = TRUE,
                             pval=TRUE,
                             risk.table = F,
                             surv.median.line = 'hv',
                             ggtheme = theme_light(),
                             break.x.by=(5),
                             cumevents = T,
                             font.x=c(12,'bold','black'),
                             font.y=c(12,'bold','black'),
                             font.tickslab = c(10, "plain", "black"),
)




#Diferències estadísticament significatives en la probabilitat de discontinuar el tractament->Log-rank
logrank1_3<-survdiff(survObject2~Drug,data = PS.3.plot)
pval1_3 <- 1 - pchisq(logrank1_3$chisq, length(logrank1_3$n) - 1) #p-val=0.02

#comparatiu 1 vs 4
PS.3.plot<-PS.3 %>% 
  filter(Medicament.AGRUPAT%in%Medicaments.freq$Medicament.AGRUPAT[c(1,4)]) %>% 
  rename(c('Medicament.AGRUPAT'='Drug')) %>% as.data.frame()
survObject2<-Surv(time =PS.3.plot$Mesos.desde.primer.consum,event =PS.3.plot$Finalitza)
fit2<-survfit(survObject2~Drug,data = PS.3.plot,type='kaplan-meier')

KM_Pers_plot_1_4<-ggsurvplot(fit2,data = PS.3.plot,
                             conf.int = F,
                             xlab='Time (months)',
                             ylab='Continuation treatment',
                             legend='right',
                             legend.title='Estimates',
                             pval.method = TRUE,
                             pval=TRUE,
                             risk.table = F,
                             surv.median.line = 'hv',
                             ggtheme = theme_light(),
                             break.x.by=(5),
                             cumevents = T,
                             font.x=c(12,'bold','black'),
                             font.y=c(12,'bold','black'),
                             font.tickslab = c(10, "plain", "black"),
)




#Diferències estadísticament significatives en la probabilitat de discontinuar el tractament->Log-rank
logrank1_4<-survdiff(survObject2~Drug,data = PS.3.plot)
pval1_4 <- 1 - pchisq(logrank1_4$chisq, length(logrank1_4$n) - 1) #p-val=0.02



# Print -------------------------------------------------------------------

wbPer<-createWorkbook()
nrow<-1
addWorksheet(wbPer,sheetName = 'Descriptiu',gridLines = F)
writeData(wbPer,1,'Número de Nhc de pacients amb AR per medicament agrupat. Anys 2018-2021',startCol = 1,startRow = nrow)
nrow<-nrow+2
writeDataTable(wbPer,1,PS.Pac.farm,startCol = 1,startRow = nrow)
nrow<-nrow+nrow(PS.Pac.farm)+2
#writeData(wbPer,1,X2_PS.Pac,startCol = 1,startRow = nrow)
nrow<-nrow+5
writeData(wbPer,1,'Número de Nhc de pacients amb AR per medicament agrupat i sexe. Anys 2018-2021',startCol = 1,startRow = nrow)
nrow<-nrow+2
writeDataTable(wbPer,1,PS.Pac.sexe,startCol = 1,startRow = nrow)
nrow<-nrow+nrow(PS.Pac.sexe)+2
writeData(wbPer,1,'Número de Nhc de pacients amb AR per medicament agrupat i grups d\'edat. Anys 2018-2021',startCol = 1,startRow = nrow)
nrow<-nrow+2
writeDataTable(wbPer,1,PS.Pac.edat,startCol = 1,startRow = nrow)

nrow<-1
addWorksheet(wbPer,sheetName = 'Kaplan-Meier',gridLines = F)
writeData(wbPer,2,'Probabilitat de persistència dels pacients tractats amb els 4 medicaments més freqüents (n>70) per Artritis Reumatoide. Anys 2018-2021',startCol = 1,startRow = nrow)
nrow<-nrow+2
print(KM_Pers_plot_4)
insertPlot(wbPer,sheet=2,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = nrow)
dev.off()
nrow<-nrow+45

writeData(wbPer,2,
          paste0('Probabilitat de persistència. Detall comparatiu ',Medicaments.freq$Medicament.AGRUPAT[1],' vs ',Medicaments.freq$Medicament.AGRUPAT[2],'. Anys 2018-2021'),
          startCol = 1,startRow = nrow)
nrow<-nrow+2
print(KM_Pers_plot_1_2)
insertPlot(wbPer,sheet=2,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = nrow)
dev.off()
nrow<-nrow+45

writeData(wbPer,2,
          paste0('Probabilitat de persistència. Detall comparatiu ',Medicaments.freq$Medicament.AGRUPAT[1],' vs ',Medicaments.freq$Medicament.AGRUPAT[3],'. Anys 2018-2021'),
          startCol = 1,startRow = nrow)
nrow<-nrow+2
print(KM_Pers_plot_1_3)
insertPlot(wbPer,sheet=2,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = nrow)
dev.off()
nrow<-nrow+45

writeData(wbPer,2,
          paste0('Probabilitat de persistència. Detall comparatiu ',Medicaments.freq$Medicament.AGRUPAT[1],' vs ',Medicaments.freq$Medicament.AGRUPAT[4],'. Anys 2018-2021'),
          startCol = 1,startRow = nrow)
nrow<-nrow+2
print(KM_Pers_plot_1_4)
insertPlot(wbPer,sheet=2,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = nrow)
dev.off()
nrow<-nrow+45

saveWorkbook(wbPer,paste0('Resultats/',avui,'_Estudi_Persistència.xlsx'),overwrite = T)


