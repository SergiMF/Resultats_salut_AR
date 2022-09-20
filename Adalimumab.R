##################################################
## Project: AR
## Script purpose: Mirar les optimitzacions per pacients
##  amb la seva prescripció               
## Date: 2022-09-19
## Author:@s.mendoza
##################################################

# Secció1: Pacients tractats amb Adalimumab. Classificats segons N --------


#Raw data amb Nhc i els medicaments que prenen
df.1<-df.cons.presc %>% 
  group_by(NHC) %>% 
  dplyr::mutate(N.medicaments=n_distinct(Medicament.AGRUPAT),
                Naïf=ifelse(N.medicaments==1,'Si','No'),) %>% 
  ungroup() %>% 
  filter(Medicament.AGRUPAT=='ADALIMUMAB') %>% 
  group_by(NHC,Medicament.Codi) %>% 
  dplyr::mutate(N.dosis.posologia=n_distinct(`Dosi Prescrita`,`Freqüència Codi`),
                Pac.optimitzat=ifelse(N.dosis.posologia==1,'No','Si')
                ) %>% 
  ungroup() %>% 
  select(Nhc,Cip,Medicament.Codi,Medicament.Descripció,Data.Consum,Quantitat.Ecofin,`Dosi Prescrita`,
         `Freqüència Codi`,`Freqüència Descripció`,Naïf,Pac.optimitzat)


# Secció2: Pacients optimitzats -------------------------------------------

  
#Raw data amb Nhc i els medicaments que prenen (format gather)
df.2<-df.1 %>% 
  filter(Pac.optimitzat=='Si') %>% 
  group_by(Nhc,Medicament.Codi,Medicament.Descripció,`Dosi Prescrita`,`Freqüència Codi`,`Freqüència Descripció`) %>% 
  dplyr::summarise(N=n()) %>% 
  ungroup() %>% 
  select(-N) %>% 
  distinct() %>% ungroup() %>% 
  unite(col="Medicació_Dosi_Posologia",c('Medicament.Descripció','Dosi Prescrita',
                                           'Freqüència Codi'),sep = '_',remove=T)
#Raw data amb Nhc i els medicaments que prenen
df.3<-df.2 %>% 
  select(-Medicament.Codi,-`Freqüència Descripció`) %>% 
  distinct() %>% 
  group_by(Nhc) %>% 
  mutate(Name=paste0('Medicament_Dosi_Posologia_',row_number())) %>% 
  spread(Name,Medicació_Dosi_Posologia) %>% 
  select(Nhc,num_range('Medicament_Dosi_Posologia_',1:ncol(.)-1)) %>% 
  ungroup()

#Taula agregada per combinació de medicament i número de pacients
df.4<-df.3 %>% 
  unite(col = 'Unified',Medicament_Dosi_Posologia_1:Medicament_Dosi_Posologia_14,na.rm = T,sep = ' + ',remove = F) %>% 
  group_by(Unified) %>% 
  dplyr::mutate(N=n()) %>% 
  ungroup() %>% 
  select(-Nhc) %>% distinct() %>% 
  select(-Unified) %>% select(N,everything()) %>% 
  arrange(desc(N)) %>% rename(.,c('N'='Número de pacients'))

wbOpt<-createWorkbook()
addWorksheet(wbOpt,'Combinacions_pacients',gridLines = F,tabColour = 'red')
writeData(wbOpt,1,'Taula agregada. Número de pacients per combinacions de prescripcions d\'Adalimumab en pacients optimitzats',
          startCol = 1,startRow = 1)
writeDataTable(wbOpt,1,df.4,startCol = 1,startRow = 3,tableStyle = 'tableStyleMedium9')

addWorksheet(wbOpt,'Raw_data_optimitzats',gridLines = F)
writeData(wbOpt,2,'Llistat de pacients optimitzats i les combinacions de prescripcions',
          startCol = 1,startRow = 1)
writeDataTable(wbOpt,2,df.3,startCol = 1,startRow = 3,tableStyle = 'tableStyleMedium9')

addWorksheet(wbOpt,'Raw_data_tots',gridLines = F)
writeData(wbOpt,3,'Llistat de pacients classificats per Naïf i per optimització',
          startCol = 1,startRow = 1)
writeDataTable(wbOpt,3,df.1,startCol = 1,startRow = 3,tableStyle = 'tableStyleMedium9')
saveWorkbook(wbOpt,'Resultats/Adalimumab_pacients_optimitzats.xlsx')

