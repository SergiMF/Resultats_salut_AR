# Seguiment --------------------------------------------------------------
df.Adherencia.pas<-df.var %>%
  select(NHC:RPT.ATC.Descripció,Procés.Inici.Data,Pas.Codi,Pas.Número.dins.el.procés,Pas.Alliberat.Data) %>% 
  filter(year(Procés.Inici.Data) %in% c((year(today())-3):year(today()))) %>% 
  dplyr::group_by(NHC,Procés.ID) %>% 
  dplyr::mutate(
    Any.inici.proces=year(Procés.Inici.Data),
    Diferencial.temps.pas=time_length(
      interval(start=lag(Pas.Alliberat.Data),end = Pas.Alliberat.Data),unit = 'days'
    ) %>% floor()) %>% ungroup() %>% 
  select(Any.inici.proces,everything()) %>% 
  arrange(Procés.ID,Pas.Número.dins.el.procés)

df.Adherencia.proces<-df.var %>% 
  group_by(NHC,Procés.ID) %>%
  dplyr::mutate(
    Any.inici.proces=year(Procés.Inici.Data),
    Primer.seguiment=first(Pas.Alliberat.Data),
    Ultim.seguiment=last(Pas.Alliberat.Data),
    Dies.en.seguiment=time_length(
      interval(start=first(Pas.Alliberat.Data),end = last(Pas.Alliberat.Data)),unit = 'days'
    ) %>% floor()
  ) %>% ungroup() %>% 
  filter( Any.inici.proces %in% c((year(today())-3):year(today()))) %>% 
  select(Any.inici.proces,everything(),- contains('Indicació'),- contains('Pas.'),- contains('Variable')) %>% 
  distinct()

#Exemple: 
# Processos abatacept any 2020
Ex1<-df.Adherencia.proces %>% 
  filter(Any.inici.proces%in%2020,RPT.ATC.Descripció=='Abatacept') %>% 
  gather('Tipus','Data',Procés.Inici.Data,Procés.Fi.Data) %>% 
  mutate(Data=as.Date(Data))

Ex1.plot<-ggplot(Ex1,aes(x=Data,y=factor(Procés.ID)))+
  geom_line(size=2,color='purple')+
  scale_y_discrete(name='Processos')+
  scale_x_date(date_breaks = '1 month',date_labels = "%b%Y")+
  ggtitle('Processos Abatacept iniciats al 2020')+
  theme_classic()+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = 3,color = 'lightgray'),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

#facet per tots els fàrmacs a l'any 2020
Ex2.gather<-df.Adherencia.proces %>% 
  filter(Any.inici.proces%in%2020) %>%
  gather('Tipus','Data',Procés.Inici.Data,Procés.Fi.Data) %>% 
  mutate(Data=as.Date(Data),
         Label=ifelse(grepl('Fi',Tipus),
                      ifelse(Dies.en.seguiment>0,paste0(Dies.en.seguiment,' dies'),NA)
                      ,NA))






# ggplot(TR2)+
#   geom_col(aes(x=as.factor(RPT.ATC.Descripció),y=n,fill=Resultat.proces))+
#   #scale_fill_viridis(discrete=TRUE,direction = T)+
#   scale_fill_manual(values = c('red3','green3'))+
#   ylim(-30,100)+
#   #geom_segment()
#   theme_classic()+
#   theme(
#     legend.position = 'none',
#     axis.text = element_blank(),
#     axis.title= element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-1,4),'cm')
#     #axis.text.x = element_text(angle = 90,hjust = 1,size = 10)
#   )+
# coord_polar()




medicaments_2020<-Ex2.gather$RPT.ATC.Descripció %>% unique()

facet.plot.2020<-ggplot(Ex2.gather %>% filter(RPT.ATC.Descripció%in%medicaments_2020[1:6]),aes(x=Data,y=factor(Procés.ID)))+
  geom_line(size=1.8,color='purple',na.rm = T)+
  geom_text(aes(label=Label),size=2,hjust=-0.2,check_overlap = T)+
  scale_y_discrete(name='Processos')+
  scale_x_date(date_breaks = '1 month',date_labels = "%b%Y")+
  ggtitle('Processos iniciats ISS. Any 2020 (part1)')+
  lemon::facet_rep_wrap(~RPT.ATC.Descripció,repeat.tick.labels = T,ncol=2)+
  theme_classic()+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90,size = 8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = 3,color = 'lightgray'),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = 'lightgray')
  )
ggsave('Resultats/Processos_2020.jpeg',facet.plot.2020,width = 21,height = 29.7,units = 'cm')




df.dif.pas<-df.var %>% dplyr::group_by(NHC,Procés.ID) %>% 
  dplyr::mutate(
    Any.inici.proces=year(Procés.Inici.Data),
    DAS28.Activitat.pas=case_when(
      Valor.Variable.Codi<2.6 ~ "Remissió",
      (Valor.Variable.Codi>=2.6 & Valor.Variable.Codi<=3.2) ~ "Activitat baixa",
      (Valor.Variable.Codi>3.2 & Valor.Variable.Codi<=5.1) ~ "Activitat moderada",
      Valor.Variable.Codi>5.1 ~ "Activitat alta"
    ),
    Diferencial.pas= Valor.Variable.Codi-lag(Valor.Variable.Codi),
    Diferencial.abs.proces= first(Valor.Variable.Codi)-min(Valor.Variable.Codi),#Valor origen i el mínim aconseguit
    Resultat.tractament.proces=ifelse(Diferencial.abs.proces<0,'Exit','Fracàs'),
    Diferencial.temps.pas=time_length(
      interval(start=lag(Pas.Alliberat.Data),end = Pas.Alliberat.Data),unit = 'month'
    ) %>% floor(),
    Diferencial.temps.proces.mes=time_length(
      interval(start=first(Pas.Alliberat.Data),end = last(Pas.Alliberat.Data)),unit = 'month'
    ) %>% floor(),
    Diferencial.temps.proces.setmana=time_length(
      interval(start=first(Pas.Alliberat.Data),end = last(Pas.Alliberat.Data)),unit = 'week'
    ) %>% floor(),
  ) %>% ungroup()


ggplot(rpart::kyphosis, aes(Age, Kyphosis)) +
  geom_jitter(height = 0.05) +
  binomial_smooth()

df<-datasets::cars
#Creates a linear model
my_linear_model <- lm(dist~speed,data = df)

#Prints the model results 
my_linear_model

#Creating a data frame
variable_speed<-data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#fiting the linear model
liner_model<-lm(dist~speed,data = df)

#predicts the future values
predict(liner_model,newdata = variable_speed)


#mirem quant de temps ha de passar pq la DAS28 estigui a 2.5 (remissióp)
Das28.remis<-round(model1$coefficients[[1]]+(model1$coefficients[[2]]*2.5),1)
#mirem quant de temps ha de passar pq la DAS28 estigui a 3.1 (baixa)
Das28.baixa<-round(model1$coefficients[[1]]+(model1$coefficients[[2]]*3.1),1)
#mirem quant de temps ha de passar pq la DAS28 estigui a 3.1 (baixa)
Das28.moderada<-round(model1$coefficients[[1]]+(model1$coefficients[[2]]*5.0),1)



df1<-data.frame()
for(i in seq_along(df.cons.amb.prescr.reduït$Nhc)){
fila<-i 
frequ<-Taula.prescripcions$Freqüencia.dies[which(
  Taula.prescripcions$Especialitat..Codi.nacional.==df.cons.amb.prescr.reduït$Especialitat..Codi.nacional.[i] &
    Taula.prescripcions$NHC==df.cons.amb.prescr.reduït$Nhc[i] &
    Taula.prescripcions$Data.Inici.Prescripció<= df.cons.amb.prescr.reduït$Data.Consum[i] &
    Taula.prescripcions$Data.Fi.Prescripció>=df.cons.amb.prescr.reduït$Data.Consum[i])]
frequ<-ifelse(length(frequ)==0,NA,frequ)
df2<-data.frame(fila,frequ)
df1<-rbind(df1,df2)
}


Taula.prescripcions$Freqüencia.dies[which(
  Taula.prescripcions$Especialitat..Codi.nacional.==df.cons.amb.prescr.reduït$Especialitat..Codi.nacional.&
    Taula.prescripcions$NHC==df.cons.amb.prescr.reduït$Nhc &
    Taula.prescripcions$Data.Inici.Prescripció<=df.cons.amb.prescr.reduït$Data.Consum &
    Taula.prescripcions$Data.Fi.Prescripció>=df.cons.amb.prescr.reduït$Data.Consum)]



#Anterior forma d'afegir les posologies (substitueixo per join)
df.cons.amb.prescr.reduït<-df.cons.amb.prescr %>% 
  select(Nhc:Dies.entre.dispensacions) %>% distinct() %>% 
  group_by(Nhc,Especialitat..Codi.nacional.,Data.Consum) %>% 
  dplyr::mutate(
    Prescripcio.dies=Taula.prescripcions$Freqüencia.dies[which(
      as.numeric(Taula.prescripcions$Especialitat..Codi.nacional.)==as.numeric(Especialitat..Codi.nacional.)&
        as.numeric(Taula.prescripcions$NHC)==as.numeric(Nhc) &
        Taula.prescripcions$Data.Inici.Prescripció<=Data.Consum &
        Taula.prescripcions$Data.Fi.Prescripció>=Data.Consum)] %>% 
      ifelse(length(.)==0,NA,.)
  ) %>% ungroup()





Prova.join<-df.cons.amb.prescr %>% 
  select(Nhc:Dies.entre.dispensacions) %>% distinct() %>% 
  dplyr::mutate(
    across(.cols = c(Nhc,Especialitat..Codi.nacional.),.fns = as.numeric)
  )

Prova.join.prescripcions<-Taula.prescripcions %>% 
  select(NHC,Especialitat..Codi.nacional.,Freqüencia.dies,`Data Inici Prescripció`,Data.Fi.Prescripció) %>% 
  dplyr::mutate(
    across(.cols = c(NHC,Especialitat..Codi.nacional.),.fns = as.numeric)
  )
Join1<-Prova.join %>% 
  left_join(Prova.join.prescripcions,by=c('Nhc'='NHC','Especialitat..Codi.nacional.'='Especialitat..Codi.nacional.')) %>% 
  filter(Data.Consum>=`Data Inici Prescripció`&
           Data.Consum<=Data.Fi.Prescripció)


Join2<-df.cons.presc %>% 
  filter(!is.na(`Prescripció ID`)) %>%
  select(Nhc,Cip,Especialitat..Codi.nacional.,Data.Consum,`Freqüència Descripció`,Freqüencia.dies,`Data Inici Prescripció`,
                `Data Fi Prescripció`
         ) %>% 
  mutate(
    Data.Fi.Prescripció.V2=replace_na(`Data Fi Prescripció`,today()) #NA a avui per filtrar
  ) %>% 
  filter(Data.Consum>=`Data Inici Prescripció`&
           Data.Consum<=Data.Fi.Prescripció.V2) %>% 
  distinct() %>% 
  select(-Data.Fi.Prescripció.V2)
  
