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
  filter(Num.pacients>80) #aquells que tenen més de 80 pacients
  
#Event:temps que passa fins que el pacient entra en remissió
df<-df.estudi %>% 
  select(Valor.Variable.Codi,Mesos.desde.primer.pas,RPT.ATC.Descripció) %>% 
  filter(RPT.ATC.Descripció%in%pacients.medicament$RPT.ATC.Descripció) %>% 
  mutate(Es.cura=ifelse(Valor.Variable.Codi<=2.6,TRUE,FALSE)) %>% 
  rename(.,c('Valor.Variable.Codi'='DAS28','Mesos.desde.primer.pas'='Temps','RPT.ATC.Descripció'='Medicament'))

survObject<-Surv(df$Temps,df$Es.cura)
fit<-survfit(survObject~Medicament,data = df,type='kaplan-meier')

fortify(survObject)

ggsurvplot(fit,data = df,conf.int = F,
           title='Corbes Kaplan-Meier. Probabilitat de no remissió (DAS28>=2.6). Anys 2014-2021',
           xlab='Temps (mesos)',
           ylab='Probabilitat de seguir malalt (DAS>=2.6)',
           legend.title='Estimacions',
           pval.method = TRUE,
           risk.table = TRUE)


#Diferències estadísticament significatives en la probabilitat de curar-se-->Log-rank
logrank<-survdiff(survObject~Medicament,data = df)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1) #p-val=0.02
