##################################################
## Project: Resultats en Salut_ AR
## Script purpose: Determinar la probabilitat de no remissió,
##                 en la cohort d'AR tractats amb els diferents medicaments(anys 2014-2021)
## Date: 2022-11-03
## Author:@s.mendoza
##################################################

library(survival)
library(survminer)

#Selecció cohort d'estudi entre els anys 2014 a 2021
df.estudi<-df.dif.pas.2 %>% 
  mutate(Any.seguiment=year(Pas.Alliberat.Data)) %>% 
  filter(Any.seguiment%in%c(min(Any.seguiment):2021))#seguiment entre els anys 2014:2021

#Numero de pacients i de tractament per medicament
pacients.medicament<-df.estudi %>% 
  group_by(RPT.ATC.Descripció) %>% 
  dplyr::summarise(Num.pacients=n_distinct(NHC),
                   Num.tractaments=n_distinct(Procés.ID)) %>% 
  ungroup() %>% 
  arrange(desc(Num.pacients))

#filtro els més freqüents per fer el print
pacients.medicament.2<-pacients.medicament %>% 
  filter(Num.pacients>80) #aquells que tenen més de 80 pacients

  
#Event:temps que passa fins que el pacient entra en remissió
df<-df.estudi %>% 
  select(Valor.Variable.Codi,Mesos.desde.primer.pas,RPT.ATC.Descripció) %>% 
  filter(RPT.ATC.Descripció%in%pacients.medicament.2$RPT.ATC.Descripció) %>% 
  mutate(Es.cura=ifelse(Valor.Variable.Codi<=2.6,TRUE,FALSE)) %>% #event es cura
  rename(.,c('Valor.Variable.Codi'='DAS28','Mesos.desde.primer.pas'='Temps','RPT.ATC.Descripció'='Medicament'))

write.xlsx(df,'QC/DF_estudi_supervivència.xlsx')

survObject<-Surv(df$Temps,df$Es.cura)
fit<-survfit(survObject~Medicament,data = df,type='kaplan-meier')


KM_plot<-ggsurvplot(fit,data = df,
           conf.int = T,
           title='Corbes Kaplan-Meier. Probabilitat de no remissió (DAS28>=2.6). Anys 2014-2021',
           xlab='Temps (mesos)',
           ylab='Probabilitat de seguir malalt (DAS>=2.6)',
           legend='right',
           legend.title='Estimacions',
           pval.method = TRUE,
           pval=TRUE,
           risk.table = F,
           surv.median.line = 'hv',
           ggtheme = theme_light(),
           break.x.by=(10),
           cumevents = T)


#Diferències estadísticament significatives en la probabilitat de curar-se-->Log-rank
logrank<-survdiff(survObject~Medicament,data = df)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1) #p-val=0.02

stil<-createStyle(
  fontSize= 14,
  textDecoration="bold"
)

wbSup<-createWorkbook()
nRow<-1
addWorksheet(wbSup,'Probabilitat de no remissió',gridLines = T)
writeData(wbSup,1,
          'Número de pacients i número de tractaments per Medicament en pacients d\'AR. Anys 2014-2021',
          startCol = 1,startRow = nRow,
          headerStyle = stil)
nRow<-nRow+2
writeDataTable(wbSup,1,pacients.medicament,startCol = 1,startRow = nRow)
nRow<-nRow+nrow(pacients.medicament)+2
writeData(wbSup,1,
          'Kaplan-Meier per determinar la probabilitat de no remissió de la malaltia pels 4 Medicaments amb més número de pacients (N>80)',
          startCol = 1,startRow = nRow)
nRow<-nRow+2

print(KM_plot)
insertPlot(wbSup,sheet=1,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = nRow)
dev.off()
nRow<-nRow+45
writeData(wbSup,1,'Log-Rank estadístic',startCol = 1,startRow = nRow)
writeData(wbSup,1,logrank,startCol = 1,startRow = nRow+2)
saveWorkbook(wbSup,paste0('Resultats/',avui,'_Estudi_Remissió_Kaplan_Meier.xlsx'),overwrite = T)
