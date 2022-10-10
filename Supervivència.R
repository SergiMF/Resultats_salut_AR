#Kaplan-Meyer
#df:

library(survival)
library(survminer)

#Selecciono medicaments amb major n



df.estudi<-df.dif.pas.2 %>% 
  mutate(Any.seguiment=year(Pas.Alliberat.Data)) %>% 
  filter(Any.seguiment%in%c(min(Any.seguiment):2021))#seguiment entre els anys 2014:2021

pacients.medicament<-df.estudi %>% #seguiment entre els anys 2014:2021
  group_by(RPT.ATC.Descripció) %>% 
  dplyr::summarise(Num.pacients=n_distinct(NHC),
                   Num.tractaments=n_distinct(Procés.ID)) %>% 
  ungroup() %>% 
  filter(Num.pacients>80) %>%  #aquells que tenen més de 80 pacients
  arrange(desc(Num.pacients))
  
#Event:temps que passa fins que el pacient entra en remissió
df<-df.estudi %>% 
  select(Valor.Variable.Codi,Mesos.desde.primer.pas,RPT.ATC.Descripció) %>% 
  filter(RPT.ATC.Descripció%in%pacients.medicament$RPT.ATC.Descripció) %>% 
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
           risk.table = F,
           surv.median.line = 'hv',
           ggtheme = theme_light(),
           break.x.by=(10),
           cumevents = T)


#Diferències estadísticament significatives en la probabilitat de curar-se-->Log-rank
logrank<-survdiff(survObject~Medicament,data = df)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1) #p-val=0.02

wbSup<-createWorkbook()
addWorksheet(wbSup,'Kaplan_Meier',gridLines = F)
writeData(wbSup,1,'Número de pacients i tractaments per Medicament',startCol = 1,startRow = 1)
writeDataTable(wbSup,1,pacients.medicament,startCol = 1,startRow = 3)
print(KM_plot)
insertPlot(wbSup,sheet=1,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = 9)
dev.off()
writeData(wbSup,1,'Log-Rank estadístic',startCol = 1,startRow = 50)
writeData(wbSup,1,logrank,startCol = 1,startRow = 53)
saveWorkbook(wbSup,'Resultats/Kaplan_Meier.xlsx',overwrite = T)
