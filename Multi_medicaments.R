##################################################
## Project:AR 
## Script purpose:Visualitzar les dades dels pacients
##                 que són tractats amb més d'un medicament
## Date: 2022-09-20
## Author:@s.mendoza
##################################################

library(ggalluvial)

# Dataset -----------------------------------------------------------------

dfM.1<-df.cons.presc2 %>% 
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
  unite(col = 'Combinació',starts_with('Medicament'),sep = '+',remove = F,na.rm = T) %>% 
  group_by(Combinació) %>% 
  dplyr::mutate(N=n()) %>% ungroup() %>% 
  select(Nhc,Combinació,N,contains(as.character(c(1:9)))) %>% 
  arrange(desc(N)) %>% 
  select(-Nhc) %>% 
  group_by(Combinació) %>% distinct() %>% 
  dplyr::mutate(across(.cols = starts_with('Temps'),.fns = ~ round(mean(.),1),.names = 'Mitjana_{.col}'),
                across(.cols = starts_with('Temps'),.fns = ~ round(sd(.),1),.names = 'Sd_{.col}'),
                across(.cols = starts_with('Temps'),.fns = min,.names = 'min_{.col}'),
                across(.cols = starts_with('Temps'),.fns = max,.names = 'max_{.col}')
                       ) %>% 
  ungroup() %>% 
  select(-starts_with('Temps')) %>% 
  distinct() %>%
  select(Combinació,N,contains(as.character(c(1:9))))

dfM.2.plot<-dfM.2 %>% head(20) %>% 
  dplyr::mutate(across(.cols = starts_with('Medicament'),.fns = ~factor(.))) %>% 
  select(Combinació,N,contains(as.character(c(1:2))))

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
writeData(wbMM,1,
          'Estudi de pacients amb AR amb més d\'una linia de tractament. Top 20 de combinacions més freqüents. Anys 2014-2021',
          startCol = 1,startRow = 1)
print(plot.alluvium)
insertPlot(wbMM,sheet=1,width =30,height = 20,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = 3)
dev.off()
writeData(wbMM,1,
          'Taula:Anàlisi 20 combinacions de medicaments més freqüents administrades a pacients amb AR. Detall dels temps de cada medicació. Anys 2014-2021',
          startCol = 1,startRow = 45)
writeDataTable(wbMM,1,dfM.2.plot,startCol = 1,startRow = 47)

addWorksheet(wbMM,'Tots_MultiMedic',gridLines = T)
writeData(wbMM,2,
          'Estudi de pacients amb AR amb més d\'una linia de tractament. Totes les combinacions.Anys 2014-2021',
          startCol = 1,startRow = 1)

writeDataTable(wbMM,2,dfM.2,startCol = 1,startRow = 3)
addWorksheet(wbMM,'Raw_data',gridLines = T)
writeData(wbMM,3,
          'Estudi de pacients amb AR amb més d\'una linia de tractament. Detall per NHC. Anys 2014-2021',
          startCol = 1,startRow = 1)

writeDataTable(wbMM,3,dfM.1,startCol = 1,startRow = 3)
saveWorkbook(wbMM,file = paste0('Resultats/',avui,'_Pacients_Multi_medicament.xlsx'),overwrite = T)