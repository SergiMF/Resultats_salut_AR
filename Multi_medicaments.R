##################################################
## Project:AR 
## Script purpose:Visualitzar les dades dels pacients
##                 que són tractats amb més d'un medicament
## Date: 2022-09-20
## Author:@s.mendoza
##################################################

library(ggalluvial)

# Dataset -----------------------------------------------------------------

dfM.1<-df.cons.presc %>% 
  arrange(Nhc,Data.Consum) %>% 
  group_by(Nhc) %>% 
  dplyr::mutate(N.medicaments=n_distinct(Medicament.AGRUPAT),
                Naïf=ifelse(N.medicaments==1,'Si','No')
                ) %>% 
  ungroup() %>% 
  filter(Naïf=='No') %>%
  group_by(Nhc,Medicament.AGRUPAT) %>% 
  dplyr::mutate('Temps.medicacio(mesos)'=time_length(
    interval(start=first(Data.Consum),end = last(Data.Consum)),unit = 'month') %>% round(1)
    ) %>%ungroup() %>%  
  select(Nhc,Medicament.AGRUPAT,`Temps.medicacio(mesos)`) %>%
  distinct(Nhc,Medicament.AGRUPAT,.keep_all = T) %>% 
  group_by(Nhc) %>% 
  dplyr::mutate(Ordre.Medicament=row_number()) %>% 
  ungroup()

#Agrupats per NHC i medicaments

dfM.2<-dfM.1 %>% 
  pivot_wider(names_from =Ordre.Medicament,values_from =  c('Medicament.AGRUPAT','Temps.medicacio(mesos)'),
              names_sep = '') %>% 
  unite(col = 'Union',starts_with('Medicament'),sep = '+',remove = F,na.rm = T) %>% 
  group_by(Union) %>% 
  dplyr::mutate(N=n()) %>% ungroup() %>% 
  select(Nhc,Union,N,contains(as.character(c(1:9)))) %>% 
  arrange(desc(N)) %>% 
  select(-Nhc) %>% 
  group_by(Union) %>% distinct() %>% 
  dplyr::mutate(across(.cols = starts_with('Temps'),.fns = ~ round(mean(.),1),.names = 'Mitjana_{.col}'),
                across(.cols = starts_with('Temps'),.fns = ~ round(sd(.),1),.names = 'Sd_{.col}'),
                across(.cols = starts_with('Temps'),.fns = min,.names = 'min_{.col}'),
                across(.cols = starts_with('Temps'),.fns = max,.names = 'max_{.col}')
                       ) %>% 
  ungroup() %>% 
  select(-starts_with('Temps')) %>% 
  distinct() %>%
  select(Union,N,contains(as.character(c(1:9))))

dfM.2.plot<-dfM.2 %>% head(20) %>% 
  dplyr::mutate(across(.cols = starts_with('Medicament'),.fns = ~factor(.))) %>% 
  select(Union,N,contains(as.character(c(1:2))))

plot.alluvium<-ggplot(dfM.2.plot,
       aes(axis1=Medicament.AGRUPAT1,
           axis2=Medicament.AGRUPAT2,
           y=N))+
  geom_alluvium(aes(fill=Medicament.AGRUPAT1),color='gray50',curve_type = 'sigmoid')+
  geom_stratum(alpha=0.85,color='gray55',size=0.5,linetype='longdash')+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size=4)+
  scale_fill_brewer(type = "qual", palette = "Set1",direction = 1)+
  scale_x_discrete(limits = c("Medicament1", "Medicament2"), expand = c(.05, .05)) +
  scale_y_continuous(name='Freqüència',
                     breaks =seq(0,1000,100))+
  theme_classic() +
  guides(fill=guide_legend(title=''))+
  ggtitle('Pacients amb més d\'una línia de tractament',subtitle = 'Top 20 combinacions més freqüents')+
  theme(
    plot.title = element_text(face = 'bold',hjust = 0.5,size = 16),
    plot.subtitle = element_text(hjust = 0.5,face = 'bold',size = 12),
    axis.text =  element_text(size = 11,face = 'bold'),
    axis.title.y.left = element_text(face = 'bold',size=11)
  )

# Print -------------------------------------------------------------------

wbMM<-createWorkbook()
addWorksheet(wbMM,'Top_20_freqs',gridLines = F,tabColour = 'red')
print(plot.alluvium)
insertPlot(wbMM,sheet=1,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = 1)
dev.off()
writeDataTable(wbMM,1,dfM.2.plot,startCol = 1,startRow = 45)
addWorksheet(wbMM,'Tots_MultiMedic',gridLines = F)
writeDataTable(wbMM,2,dfM.2,startCol = 1,startRow = 1)
addWorksheet(wbMM,'Raw_data',gridLines = F)
writeDataTable(wbMM,3,dfM.1,startCol = 1,startRow = 1)
saveWorkbook(wbMM,file = 'Resultats/Pacients_Multi_medicament.xlsx',overwrite = T)


























#taula de freqüències
prova<-dfM.2 %>% select(N,starts_with('Medicament.AGRUPAT'))
llistat<-lapply(c(1:9), function(x){
  df<-prova %>% select(x+1,1) %>% 
    group_by_at(1) %>% 
    dplyr::summarise(N=sum(N)) %>% as.data.frame()
  names(df)[2]<-paste0('N_',x)
}) %>%


dfM.3<-dfM.2 %>% select(N,starts_with('Medicament.AGRUPAT')) %>% 
  gather('Ordre','Medicament',2:ncol(.)) %>% 
  group_by(Ordre,Medicament) %>% 
  dplyr::summarise(Casos=sum(N)) %>% 
  ungroup() %>% 
  filter(!is.na(Medicament)) %>% 
  spread(Ordre,Casos) %>% 
  replace(is.na(.),0) %>% 
  gather('Medicament','N',2:ncol(.)) %>% 
  mutate(Ordre=str_extract(Ordre,'\\d$'))




Colors.stratum<-list(`ADALIMUMAB`='#E41A1C',
                     `ETANERCEPT`='#377EB8',
                     `INFLIXIMAB`='#4DAF4A',
                     `SECUKINUMAB`='#984EA3',
                     `USTEKINUMAB`='#FF7F00',
                     `VEDOLIZUMAB`='#FFFF33',
                     `CERTOLIZUMAB`='#A65628',
                     `GOLIMUMAB`='#F781BF',
                     `GUSELKUMAB`='#999999',
                     `IXEKIZUMAB`='#8b37e4',
                     `RISANKIZUMAB`='#a8e6cf',
                     `TOCILIZUMAB`='#cef310'
  
)






#Fem tauleta amb freqüències:

  

#'gray88'
 
