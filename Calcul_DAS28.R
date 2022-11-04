##################################################
## Project: Artritis Reumatoide
## Script purpose: Calcular millora de DAS28 per pacient
##                 
## Date: 2022-07-01
## Author:@s.mendoza
##################################################


# Càrrega paquets ---------------------------------------------------------
suppressMessages(suppressWarnings(if (!require("pacman") == TRUE) { install.packages("pacman", quietly=T) }))
suppressMessages(suppressWarnings(pacman::p_load(tidyverse,lubridate,openxlsx,plyr,readxl,eeptools,viridis,flextable,officer)))
`%notin%` <- Negate(`%in%`)
avui<-today() %>% format("%d_%m_%Y")

# Càrrega dades -----------------------------------------------------------
Arxiu.proc<-list.files('Dataset/Processos/',pattern = '*.csv',full.names = T)
Arxiu.consums<-list.files('Dataset/Consums/',pattern = '*.csv',full.names = T)
Arxiu.Prescripcions<-list.files('Dataset/Prescripcions/',pattern = '*.csv',full.names = T)

#Extreure data real d'inici tractament (variable.codi==21643)
df.proc.data.Inic<-read.csv2(Arxiu.proc) %>% 
  filter(Variable.Codi=='21643') %>% #codi SAP de la variable
  dplyr::mutate(across(.cols = contains('Data',ignore.case = T),.fns = ymd_hms),
                across(.cols = starts_with('Valor.Variable.Codi'),.fns = ymd),
                across(.cols = starts_with('Nhc'),.fns = as.character)
  ) %>% 
  select(NHC..sense.ceros.,Procés.ID,Valor.Variable.Codi) %>% 
  rename(c('Valor.Variable.Codi'='Data.Inici.Real'))




df.proc<-read.csv2(Arxiu.proc) %>% 
  filter(Variable.Codi=='6458') %>% 
  dplyr::mutate(across(.cols = contains('Data',ignore.case = T),.fns = ymd_hms),
                across(.cols = starts_with('Valor.Variable'),.fns = as.numeric),
                across(.cols = starts_with('Nhc'),.fns = as.character)
  ) %>% 
  left_join(df.proc.data.Inic,by=c('NHC..sense.ceros.','Procés.ID')) %>% #afegim dates d'inici reals
  select(1:Procés.ID,Data.Inici.Real,everything())

#QC
#write.xlsx(df.proc,paste0('QC/',avui,'_Dades_RPT.xlsx'),overwrite = T)


df.cons<-read.csv2(Arxiu.consums) %>% 
  dplyr::mutate(across(.cols = contains('Data',ignore.case = T),.fns = ymd_hms)) %>% 
  filter(Nhc %notin% c('N/D'), !is.na(Nhc))

df.presc<-readr::read_csv2(Arxiu.Prescripcions,) %>% 
    dplyr::mutate(across(.cols = contains('Data',ignore.case = T),.fns = ymd_hms)) %>% 
    filter(NHC %notin% c('N/D'), !is.na(NHC)) #Afegeixo Dades sense Medicament_Agregat

#Anys de treball:
Any.inici<-year(today())-4
Any.final<-year(today())-1
# Anàlisi DAS -------------------------------------------------------------

df.var<-df.proc %>% select(NHC,NHC..sense.ceros.,CIP,Procés.ID,RPT.ATC.Codi,RPT.ATC.Descripció,RPT.Indicació.Codi,RPT.Indicació.Descripció,
                           Data.Inici.Real,Procés.Inici.Data,Procés.Fi.Data,
                           Pas.Codi,Pas.Alliberat.Data,Pas.Número.dins.el.procés,Pas.següent.data,Pas.previ.data,
                           Variable.Descripció.Llarga,Valor.Variable.Codi) %>% 
  arrange(NHC,Procés.ID,Pas.Alliberat.Data,Pas.Número.dins.el.procés) #Numero.dins.procés??No coincideix amb dates

#Els valors de DAS28>10 són errors. Els separem
Outlayers.DAS28<-df.var %>% filter(Valor.Variable.Codi>10) #21 registres

#Càlcul Increments de DAS i temps entre Pas

df.dif.pas<-df.var%>% dplyr::group_by(NHC,Procés.ID) %>% 
  #left_join(df.dictamen,by=c('RPT.ATC.Codi'='Codi.ATC')) %>% #afegeixo data dictàmen
  dplyr::mutate(
    Any.inici.proces=year(Procés.Inici.Data),
    DAS28.Activitat.pas=case_when(
      Valor.Variable.Codi<2.6 ~ "Remissió",
      (Valor.Variable.Codi>=2.6 & Valor.Variable.Codi<=3.2) ~ "Activitat baixa",
      (Valor.Variable.Codi>3.2 & Valor.Variable.Codi<=5.1) ~ "Activitat moderada",
      (Valor.Variable.Codi>5.1 & Valor.Variable.Codi<=10)~ "Activitat alta"
    ),
    Diferencial.pas= dplyr::lag(Valor.Variable.Codi,n = 0)-first(Valor.Variable.Codi), #diferencials respecte inici
    Diferencial.max.proces= min(Diferencial.pas[Diferencial.pas!=0]),
    N=n(),
    Dies.fins.DAS.mínim=time_length(
      interval(start=first(Pas.Alliberat.Data),end = Pas.Alliberat.Data[which.min(Valor.Variable.Codi)]),unit = 'days'
    ) %>% floor(),
    Mesos.desde.primer.pas=time_length(
      interval(start=first(Pas.Alliberat.Data),end = lag(Pas.Alliberat.Data,n=0,default = 0)),unit = 'month'
    ) %>% floor(),
    Resultat.tractament.proces=case_when(
      N==1 ~ 'Sense seguiment',
      Diferencial.max.proces<0 ~'Èxit',
      (is.infinite(Diferencial.max.proces) & N>1) ~'Sense canvis',
      Diferencial.max.proces>0 ~'Fracàs'
    ),
    Error.inici=ifelse(first(Valor.Variable.Codi<3.2)|any(Mesos.desde.primer.pas==0 & Valor.Variable.Codi<3.2) ,'Si','No') #Comencen amb DAS28 baixa
  ) %>% ungroup() %>% 
  group_by(NHC) %>% #controls
  dplyr::mutate(
    Tractament.pur=ifelse(n_distinct(RPT.ATC.Codi)==1,'Si','No') #únic tractament
  ) %>% ungroup() %>% 
  mutate(
    Medicament.AGRUPAT= str_replace(RPT.ATC.Descripció,'Certolizumab pegol','Certolizumab') %>%
      str_to_upper() #per poder creuar amb la resta de df
  ) %>% 
  filter(Valor.Variable.Codi<=10) #outlyers


# NETEJA DE DADES (Out of scoping)
# -Pacients que comencen amb DAS<3.2  
# -Pacients que tenen més d'un tractament
df.dif.pas.2<-df.dif.pas %>% 
  filter(Tractament.pur=='Si') %>% 
  filter(Error.inici=='No')
  #filter(Fora.dictamen=='No')#n=8303(-32.91%)

#--------------------------------------------------------------------------------------------------------------
# Dades en seguiment  -----------------------------------------------------

#Número de pacients a l'estudi:
nrow(df.dif.pas.2 %>% distinct(NHC..sense.ceros.)) #1166 pacients
#Número de tractaments:
nrow(df.dif.pas.2 %>% distinct(Procés.ID)) #1262
#Mirem els anys de seguiment dels Valors de DAS28
Any.min.Das28<-min(df.dif.pas.2$Any.inici.proces,na.rm = T) #2014
Any.max.Das28<-max(df.dif.pas.2$Any.inici.proces,na.rm = T) #2022
#-------------------------------------------------------------------------------------------------------------- 


df.resultats.proces<-df.dif.pas.2 %>% 
  select(NHC,CIP,Any.inici.proces,Procés.ID,Medicament.AGRUPAT,RPT.ATC.Descripció,RPT.Indicació.Codi,RPT.Indicació.Descripció,Variable.Descripció.Llarga,
         Diferencial.max.proces,N,Resultat.tractament.proces,Dies.fins.DAS.mínim) %>% 
  distinct()

#Taula Resum resultats DAS28
TR.DAS28<-df.resultats.proces %>%
  filter(N>1) %>% #trec els que no tenen seguiment
  dplyr::group_by(Any.inici.proces,RPT.ATC.Descripció,RPT.Indicació.Codi,RPT.Indicació.Descripció) %>% 
  dplyr::summarise(
    Num.processos.exit=sum(Resultat.tractament.proces=='Èxit'),
    Num.processos.fracas=sum(Resultat.tractament.proces=='Fracàs'),
    Num.processos.sense.canvis=sum(Resultat.tractament.proces=='Sense canvis'),
    'Temps.fins.DAS.mínim.exitosos(dies)'=round(mean(Dies.fins.DAS.mínim[which(Resultat.tractament.proces=='Èxit')]),1)
  ) %>%
  dplyr::mutate(
    across(.cols = starts_with('Num'),.fns = ~round(./(Num.processos.exit+
                                                                Num.processos.fracas+
                                                                Num.processos.sense.canvis)*100,1),.names ="%{.col}" )
  ) %>% 
  ungroup() %>% 
  select(1:4,contains(c('Num.processos.exit','Num.processos.fracas','Num.processos.sense.canvis')
                        ),everything())

#Preparació per flextable
table1.data<-TR.DAS28 %>% filter(Any.inici.proces%in%c((year(today())-3):year(today())-1)) %>% 
  dplyr::mutate(across(.cols = starts_with('%'),.fns = ~paste0(.,'%'))) %>% 
  select(-RPT.Indicació.Descripció)

set_flextable_defaults(background.color = "white")

Taula1.ft<-colformat_double(flextable(table1.data),big.mark = '',digits = 0) %>% 
  merge_v(part='body',j=1) %>% 
  style(pr_p = fp_par(text.align='center',padding=3),j=c(1,3:ncol(table1.data)),part = 'body') %>% 
  hline(border= fp_border(width = 0.5,style = 'solid',color = 'lightgray'),part='body') %>% 
  vline(border= fp_border(width = 0.5,style = 'dashed',color = 'lightgray'),part='all') %>% 
  autofit()
if(require('webshot')){
  save_as_image(Taula1.ft,path = 'Resultats/Taula1_ft.png',webshot = 'webshot')
}
  

#Creem el plot per visualitzar els resultats. Agrupem per fàrmac i any, sense indicacions
#Creem dataframes:
TR.DAS28.plot<-TR.DAS28 %>% select(-starts_with('Temps'),-contains('Indicació'),-starts_with('%')) %>% 
  group_by(Any.inici.proces,RPT.ATC.Descripció) %>%
  dplyr::summarise(across(.cols = starts_with('Num'),.fns = sum)) %>% ungroup() %>% 
  dplyr::mutate(Suma=rowSums(select(.,starts_with('Num')))) %>% 
    dplyr::mutate(across(.cols = starts_with('Num'),.fns = ~ round(.*100/Suma,1),
                      .names = '%{.col}')
                ) %>% select(-Suma) %>% 
  filter(Any.inici.proces %in% c((year(today())-3):year(today())-1))

#Df per resultats absoluts
TR.DAS28.plot.gather.n<-TR.DAS28.plot %>% select(-starts_with('%')) %>% 
  gather('Tipus resultat(n)','N',starts_with('Num')) %>% 
  mutate(Label=ifelse(N>3,N,NA),
         Label.P=ifelse(N>0,paste0('n=',N),NA),
         `Tipus resultat(n)`=factor(`Tipus resultat(n)`,
            levels = c('Num.processos.sense.canvis',
                'Num.processos.fracas','Num.processos.exit'))
         ) %>% 
  arrange(Any.inici.proces,RPT.ATC.Descripció,`Tipus resultat(n)`)

colors<-c('Num.processos.exit'='green3',
          'Num.processos.fracas'='red3',
          'Num.processos.sense.canvis'='yellow3'
          )

Plot.resultats.n<-
ggplot(TR.DAS28.plot.gather.n,aes(x=RPT.ATC.Descripció,y=N,fill=factor(`Tipus resultat(n)`)))+
  geom_bar(stat='identity',position = 'stack',width = 0.8,color='white')+
  geom_text(aes(label=Label),size = 3,
            position = position_stack(vjust = 0.5),
            na.rm = T,show.legend = FALSE)+
  scale_fill_manual(values = colors)+
  scale_y_continuous(name = 'Numero de processos')+
                     #labels = scales::percent)+
  scale_x_discrete(name='Medicament',
                   labels=function(RPT.ATC.Descripció){str_wrap(RPT.ATC.Descripció,15)})+
  ggtitle('RPT resultats dels tractaments ambulatoris en termes de variables com exit/fracàs (DAS28)',
          subtitle ='Número de processos agregats per medicament i any d\'inici del tractament' )+
  theme_classic()+
  lemon::facet_rep_wrap(~Any.inici.proces,repeat.tick.labels = T,scales = 'free_y')+
  theme(
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5,size =9,face = 'bold'),
    legend.position = 'top',
    legend.title = element_blank(),
    plot.title = element_text(hjust=0.5,face = 'bold.italic'),
    plot.subtitle = element_text(hjust = 0.5,face = 'bold.italic')
  )

Plot.resultats.Percentatge<-
  ggplot(TR.DAS28.plot.gather.n,aes(x=RPT.ATC.Descripció,y=N,fill=factor(`Tipus resultat(n)`)))+
  geom_bar(stat='identity',position = 'fill',width = 0.8,color='white')+
  geom_text(aes(label=Label.P),size = 3,
             position = position_fill(vjust = 0.5),
             na.rm = T,show.legend = FALSE)+
  scale_fill_manual(values = colors)+
  scale_y_continuous(name = '% de processos',
                    labels = scales::percent)+
  scale_x_discrete(name='Medicament',
                   labels=function(RPT.ATC.Descripció){str_wrap(RPT.ATC.Descripció,15)})+
  ggtitle('RPT resultats dels tractaments ambulatoris en termes de variables com exit/fracàs (DAS28)',
          subtitle ='Proporció (%) de processos agregats per medicament i any d\'inici del tractament' )+
  theme_classic()+
  lemon::facet_rep_wrap(~Any.inici.proces,repeat.tick.labels = T,scales = 'free_y')+
  theme(
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5,size =9,face = 'bold'),
    legend.position = 'top',
    legend.title = element_blank(),
    plot.title = element_text(hjust=0.5,face = 'bold.italic'),
    plot.subtitle = element_text(hjust = 0.5,face = 'bold.italic')
  )


# Variació DAS28 al llarg del temps per Medicament ------------------------


medicaments<-df.dif.pas.2 %>% select(RPT.ATC.Codi,RPT.ATC.Descripció) %>% distinct()

# Gràfic regressions lineals per medicament i DAS28:
funcio.DAS28<-function(x){
  #dataset:
  df.variacio.DAS<-df.dif.pas.2 %>% 
    filter(RPT.ATC.Codi%in%medicaments$RPT.ATC.Codi[x],
           Resultat.tractament.proces%in%'Èxit') %>% 
    mutate(DAS28.Activitat.pas=factor(DAS28.Activitat.pas,
                                      levels = c('Activitat alta',
                                                 'Activitat moderada',
                                                 'Activitat baixa',
                                                 'Remissió')))
  
  #Coeficient de correlació de Pearson:
  Pearson<-cor.test(df.variacio.DAS$Valor.Variable.Codi,df.variacio.DAS$Mesos.desde.primer.pas  )
  # Model amb regressió linial
  model1<-lm(Valor.Variable.Codi~Mesos.desde.primer.pas,data = df.variacio.DAS)
  
  summary(model1)
  model1$coefficients  #y=34.714-4.27x
  
  Das28.prediccio1<-data.frame(
    Mesos.desde.primer.pas=seq(0,max(df.variacio.DAS$Mesos.desde.primer.pas),1)
  ) 
  
  Das28.prediccio2<- Das28.prediccio1 %>% 
    mutate(
      Valor.Variable.Codi=predict(model1,Das28.prediccio1,level = 0.95))  
  
  df.canvis<-Das28.prediccio2 %>% 
      slice(
        which.min(Das28.prediccio2$Valor.Variable.Codi>=2.5)-1,
        which.min(Das28.prediccio2$Valor.Variable.Codi>=3.1)-1
        #which.min(Das28.prediccio2$Valor.Variable.Codi>=5.0)-1
      )
    if(nrow(df.canvis)>0){
      df.canvis<-df.canvis %>% 
        mutate(
        Activitat=case_when(
          Valor.Variable.Codi<2.6~'Remissió',
          (Valor.Variable.Codi>=2.6 & Valor.Variable.Codi<3.2)~'Activ.Baixa'
        ),  
        #Activitat=c('Remissió','Activ.Baixa'),
        Label=paste0(Activitat,': ',Mesos.desde.primer.pas,'mesos'))
    }
    
    #grafiquem  
    grafic<-
      if(nrow(df.canvis)>0){
          df.variacio.DAS %>% 
            ggplot(aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi
            ))+
            geom_point(aes(color=DAS28.Activitat.pas))+
            stat_smooth(aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi),
                        se = T,method ='lm',
                        color='yellow',size=1,fill='black') +
            geom_point(data=df.canvis,aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi))+
            geom_segment(aes(x=0,xend=df.canvis$Mesos.desde.primer.pas[1],
                             y=df.canvis$Valor.Variable.Codi[1],yend=df.canvis$Valor.Variable.Codi[1]),
                         linetype='longdash',size=1)+
            geom_segment(aes(x=0,xend=df.canvis$Mesos.desde.primer.pas[2],
                             y=df.canvis$Valor.Variable.Codi[2],yend=df.canvis$Valor.Variable.Codi[2]),
                         linetype='longdash',size=1)+
            geom_segment(aes(x=df.canvis$Mesos.desde.primer.pas[1],xend=df.canvis$Mesos.desde.primer.pas[1],
                             y=0,yend=df.canvis$Valor.Variable.Codi[1]),
                         linetype='longdash',size=1)+
            geom_segment(aes(x=df.canvis$Mesos.desde.primer.pas[2],xend=df.canvis$Mesos.desde.primer.pas[2],
                             y=0,yend=df.canvis$Valor.Variable.Codi[2]),
                             linetype='longdash',size=1)+
            ggrepel::geom_label_repel(data =df.canvis ,aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi,
                             label=Label),fill='#7db3e1',size=3,color='#000000',
                             nudge_x = 1,nudge_y = 1)+
            scale_y_continuous(name = 'Valor DAS28',expand = c(0,0),
                               limits = c(0,10),
                               breaks = seq(0,10,1)
                               )+
            scale_x_continuous(name='Temps (mesos)',expand = c(0,0),
                               limits = c(0,max(df.variacio.DAS$Mesos.desde.primer.pas)+2),
                               breaks = seq(0,max(df.variacio.DAS$Mesos.desde.primer.pas)+5,5))+
            scale_color_brewer(name='Activitat DAS28',palette = 'Set1')+
            ggtitle(paste0('Valors de DAS28 en processos tractats amb ',medicaments$RPT.ATC.Descripció[x],' al llarg del temps'))+
            theme_classic()+
            theme(
              plot.title = element_text(hjust = 0.5,face = 'bold',size = 14)
            )
        }else{
          df.variacio.DAS %>% 
            ggplot(aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi
            ))+
            geom_point(aes(color=DAS28.Activitat.pas))+
            stat_smooth(aes(x=Mesos.desde.primer.pas,y=Valor.Variable.Codi),
                        se = T,method ='lm',
                        color='yellow',size=1,fill='black') +
                        scale_y_continuous(name = 'Valor DAS28',expand = c(0,0),
                               limits = c(0,max(df.variacio.DAS$Valor.Variable.Codi)+2),
                               breaks = seq(0,max(df.variacio.DAS$Valor.Variable.Codi)+2,2)
            )+
            scale_x_continuous(name='Temps (mesos)',expand = c(0,0),
                               limits = c(0,max(df.variacio.DAS$Mesos.desde.primer.pas)+2),
                               breaks = seq(0,max(df.variacio.DAS$Mesos.desde.primer.pas)+5,5))+
            scale_color_brewer(name='Activitat DAS28',palette = 'Set1')+
            ggtitle(paste0('Valors de DAS28 en processos tractats amb ',medicaments$RPT.ATC.Descripció[x],' al llarg del temps'))+
            theme_classic()+
            theme(
              plot.title = element_text(hjust = 0.5,face = 'bold',size = 14)
            )
        }
    assign(paste0('grafic.',medicaments$RPT.ATC.Descripció[x]),grafic) %>% return()
}

llistats.grafics.DAS28<-lapply(seq_along(medicaments$RPT.ATC.Codi),try(funcio.DAS28))      

names(llistats.grafics.DAS28)<-paste0('Evolució del valor DAS28 en pacients tractats amb ',medicaments$RPT.ATC.Descripció,
                                   ' durants els anys ',Any.min.Das28,'-',Any.max.Das28)



# Càlcul del proportion of days covered (PDC) -----------------------------

#Consums

Consums<-df.cons %>% 
  select(Nhc,Cip,Medicament.Codi,Medicament.Descripció,Medicament.AGRUPAT,Episodi,
         Especialitat..Codi.nacional.,Especialitat.Descripció,Forma.Medicament.Descripció,Data.Consum,Quantitat.Ecofin) %>% 
  arrange(Nhc,Data.Consum) %>% 
  group_by(Nhc,Medicament.Descripció ) %>% 
  dplyr::mutate(
    Dies.entre.dispensacions=time_length(
    interval(start=lag(Data.Consum),end = Data.Consum),unit = 'days'
  ) %>% floor(),
  Nhc=as.character(str_replace(Nhc,'^0+',''))
  ) %>% ungroup() %>% 
  filter(Nhc%in%df.dif.pas.2$NHC..sense.ceros.) %>%
  filter(Quantitat.Ecofin!=0) #trec aquelles entrades sense consums



#prescripcions
df.presc.base<-df.presc %>% 
  select(`Centre Codi`,`NHC (sense ceros)`,NHC,CIP,`Prescripció ID`,`Prescripció ID Inicial`,`Medicament Codi`,`Especialitat Codi Nacional`,
         `Dosi Prescrita`:`Freqüència Tipus Descripció`,`Data Inici Prescripció`,`Data Fi Prescripció`) %>% 
  mutate(Posologia.dies=case_when(
    grepl(pattern = 'MES',`Freqüència Codi`)~ as.numeric(str_extract(`Freqüència Codi`,'^\\d'))*30,
    `Freqüència Tipus Codi`=='4' & grepl('^SET',`Freqüència Codi`)~7,
    `Freqüència Tipus Codi`=='4' & grepl('\\d+SET',`Freqüència Codi`)~as.numeric(str_extract(`Freqüència Codi`,'^\\d+'))*7,
    `Freqüència Tipus Codi`=='4' & grepl('D[MT]|\\d+D',`Freqüència Codi`)~as.numeric(str_extract(`Freqüència Codi`,'^\\d+')),
    `Freqüència Tipus Codi`=='4' & grepl('^\\d+[MTN]',`Freqüència Codi`)~as.numeric(str_extract(`Freqüència Codi`,'^\\d+'))/24,
    `Freqüència Tipus Codi`=='4' & grepl('^\\d+$',`Freqüència Codi`)~as.numeric(str_extract(`Freqüència Codi`,'^\\d+')),
    `Freqüència Tipus Codi`=='7'~1,
    `Freqüència Tipus Codi`=='5' & grepl('^c/\\d+h',`Freqüència Descripció`)~round(as.numeric(str_extract(`Freqüència Descripció`,'\\d+'))/24,2),
    `Freqüència Tipus Codi`=='5' & grepl('^LU|^MA|^MI|^JU|^VI|^SA|^DO|^SET',`Freqüència Codi`)~7,
    `Freqüència Tipus Codi`=='5' & grepl('^A\\d+|^[EDNS]S?$|^E$',`Freqüència Codi`)~1,
    `Freqüència Tipus Codi`=='5' & grepl('EDS',`Freqüència Codi`)~round(1/3,1),
    `Freqüència Tipus Codi`=='5' & grepl('\\d+-\\d+',`Freqüència Codi`)~round(1/2,1),
    `Freqüència Tipus Codi`=='5' & grepl('^\\d+SET',`Freqüència Codi`)~round(7/as.numeric(str_extract(`Freqüència Descripció`,'^\\d+')),1),
    `Freqüència Tipus Codi`=='6' & grepl('\\d+',`Freqüència Codi`)~round(24/as.numeric(str_extract(`Freqüència Descripció`,'\\d+')),1),
    `Freqüència Tipus Codi`=='0' & grepl('1 DOSIS',`Freqüència Codi`)~1
  )) %>% 
  mutate(`Especialitat Codi Nacional`=as.integer(str_sub(`Especialitat Codi Nacional`,start = 1,end = -2)),
         `NHC (sense ceros)`=as.character(`NHC (sense ceros)`)) %>% 
  filter(`NHC (sense ceros)`%in%df.dif.pas.2$NHC..sense.ceros.) %>% #només pacients amb RPT
  select(`NHC (sense ceros)`,NHC:`Freqüència Tipus Descripció`,Posologia.dies,everything())


#--------------------------------------------------------------------------------------------------------------------
# QC: Pacients sense Consums i/o sense prescripcions ----------------------


Pacients.sense.consums<-df.dif.pas.2 %>%
  filter(Any.inici.proces<year(today())) %>% #mirem consums en pacients fins el 2021
  filter(NHC..sense.ceros.%notin%Consums$Nhc)
#Numero Nhc sense consums
nrow(Pacients.sense.consums %>% distinct(NHC..sense.ceros.)) #61

Pacients.sense.prescripcions<-df.dif.pas.2 %>% 
  filter(Any.inici.proces<year(today())) %>% #mirem prescripcions en pacients fins el 2021
  filter(NHC..sense.ceros.%notin%df.presc.base$`NHC (sense ceros)`)
#Numero Nhc sense prescripcions
nrow(Pacients.sense.prescripcions %>% distinct(NHC..sense.ceros.)) #26

Pacients.consum.sense.prescripcio<-df.dif.pas.2 %>%
  filter(Any.inici.proces<year(today())) %>% #mirem prescripcions en pacients fins el 2021
  filter(NHC..sense.ceros.%in%Consums$Nhc) %>% 
  filter(NHC..sense.ceros.%notin%df.presc.base$`NHC (sense ceros)`)
#Numero Nhc amb consum i sense prescripcions
nrow(Pacients.consum.sense.prescripcio %>% distinct(NHC..sense.ceros.)) #24


Pacients.prescripcio.sense.consum<-df.dif.pas.2 %>% 
  filter(Any.inici.proces<year(today())) %>% #mirem prescripcions en pacients fins el 2021
  filter(NHC..sense.ceros.%in%df.presc.base$`NHC (sense ceros)`) %>% 
  filter(NHC..sense.ceros.%notin%Consums$Nhc)
#Numero Nhc amb consum i sense prescripcions
nrow(Pacients.prescripcio.sense.consum %>% distinct(NHC..sense.ceros.)) #59


WbQC<-createWorkbook()
addWorksheet(WbQC,'Pacients_sense_consums')
writeData(WbQC,'Pacients_sense_consums','NHC que no hi consten a l\'univers de Consums, però sí tenen RPT',startCol =1 ,startRow =1)
writeDataTable(WbQC,'Pacients_sense_consums',Pacients.sense.consums,startCol =1 ,startRow =3)

addWorksheet(WbQC,'Consum_Si_Presc_No')
writeData(WbQC,'Consum_Si_Presc_No','NHC que tenen un consum, un RPT, però no hi consten a l\'univers de Prescripcions',startCol =1 ,startRow =1 )
writeDataTable(WbQC,'Consum_Si_Presc_No',Pacients.consum.sense.prescripcio,startCol =1 ,startRow =3 )

addWorksheet(WbQC,'Presc_Si_Consum_No')
writeData(WbQC,'Presc_Si_Consum_No','NHC que tenen un RPT, una prescripció, però no hi consten a l\'univers de Consums',startCol =1 ,startRow =1 )
writeDataTable(WbQC,'Presc_Si_Consum_No',Pacients.prescripcio.sense.consum,startCol =1 ,startRow =3)
saveWorkbook(WbQC,file = paste0('QC/',avui,'_Pacients_sense_Consums_prescripcions.xlsx'),overwrite = T)

  
#--------------------------------------------------------------------------------------------------------------------

#versió creuant per codi medicament
df.cons.presc<-Consums %>% 
  left_join(df.presc.base,
            by=c('Nhc'='NHC (sense ceros)',
                 'Medicament.Codi'='Medicament Codi')) #%>% 
  #select(NHC,CIP,Procés.ID,Procés.Inici.Data)




# Creuament processos + consums + prescripcions ---------------------------
#com que no podem filtrar consums i prescripcions per indicació. Seleccionem els consums pels NHc dels pacients

df.cons.presc2<-df.cons.presc %>% filter(Nhc%in%df.resultats.proces$NHC) #consums i prescripcions dels pacients d'estudi


df.cons.amb.prescr.reduït<-df.cons.presc2 %>% 
  filter(!is.na(`Prescripció ID`)) %>%
  select(Nhc,Cip,Episodi,Medicament.AGRUPAT,Medicament.Descripció,Especialitat..Codi.nacional.,Data.Consum,Quantitat.Ecofin,`Freqüència Descripció`,Posologia.dies,`Data Inici Prescripció`,
         `Data Fi Prescripció`
  ) %>% 
  mutate(
    Data.Fi.Prescripció.V2=replace_na(`Data Fi Prescripció`,today()) #NA a avui per filtrar
  ) %>% 
  filter(Data.Consum>=`Data Inici Prescripció`&
           Data.Consum<=Data.Fi.Prescripció.V2) %>% 
  distinct() %>% 
  select(-Data.Fi.Prescripció.V2)



#Calculem els valors de PDC. Per cada NHC calculem el PDC per cada prescripció i farem la mitja
PDC1<-df.cons.amb.prescr.reduït %>% 
  group_by(Nhc,Especialitat..Codi.nacional.,Posologia.dies) %>% 
  dplyr::mutate(Unitats.dispensades=sum(Quantitat.Ecofin),
                Dies.medicació=time_length(
                  interval(start=first(Data.Consum),end = last(Data.Consum)),unit = 'days'
                ) %>% floor(),
                PDC.disgregat=round( #diferents posologies durante el tractament. Es farà la mitja de PDC
                  (Unitats.dispensades/(1/Posologia.dies))/Dies.medicació,1)
                ) %>% 
  ungroup() %>% 
  filter(!is.na(PDC.disgregat),is.finite(PDC.disgregat)) %>% 
  group_by(Nhc,Especialitat..Codi.nacional.) %>% 
  dplyr::mutate(PDC=round(mean(PDC.disgregat),1)) %>% 
  ungroup()

#Reduïm columnes per treure dades per Nhc, i especialitat
levels.PDC<-c('Consum únic','<0.5','0.5-0.8','0.8-0.9','>=0.9')
PDC2<-PDC1 %>% 
  select(-Data.Consum,-Quantitat.Ecofin) %>% 
  distinct() %>% 
  filter(!is.na(PDC),!is.infinite(PDC)) %>% #netejo els que no tenen dies.medicacio
  mutate(
    Classificacio.PDC=case_when(
      Dies.medicació==1~'Consum únic',
      PDC>=0.9~'>=0.9',
      PDC>=0.8 & PDC<0.9~'0.8-0.9',
      PDC>=0.5 & PDC<0.8~'0.5-0.8',
      PDC<0.5~'<0.5'
    ) %>% factor(levels = levels.PDC)
  ) %>% 
  filter(Classificacio.PDC!='Consum únic')#treiem els que només han tingut un consum

#preparo dataset per adjuntar com a dades crues a l'informe
Taula.dades.PDC<-PDC2 %>% 
  select(-PDC.disgregat) %>% 
  distinct() %>% 
  rename(.,c('Posologia.dies'='Prescripció (Posologia en dies)'))

#agrupo per medicament
Agrupacio.PDC.Med<-PDC2 %>% 
  group_by(Medicament.AGRUPAT,Medicament.Descripció,Classificacio.PDC) %>% 
  dplyr::summarise(N=n()) %>% ungroup()
#Agrupació per medicament agrupat (MA):
Agrupacio.PDC.MA<-PDC2 %>% 
  group_by(Medicament.AGRUPAT,Classificacio.PDC) %>% 
  dplyr::summarise(N=n()) %>% ungroup() %>% 
  mutate(Label.P=ifelse(N>3,paste0('n=',N),NA)) %>% 
  arrange(Medicament.AGRUPAT,Classificacio.PDC) %>% 
  group_by(Medicament.AGRUPAT) %>% 
  dplyr::mutate(Suma=sum(N)) %>% 
  ungroup()

#Taula numérica per medicament agrupat:

Taula.resum.pacients<-PDC2 %>% 
  group_by(Medicament.AGRUPAT) %>% 
  dplyr::summarise('Pacients/Medicament'=n_distinct(Nhc))
Taula.Pacients.Totals.Das28<-data.frame(
  Var='Numero de pacients totals',
  N=nrow(PDC2 %>% distinct(Nhc))
)

Taula.PDC.MA<-Agrupacio.PDC.MA %>% select(-Label.P,-Suma) %>% 
  spread(Classificacio.PDC,N) %>% 
  replace(is.na(.),0) %>% 
  dplyr::mutate('Tractaments/Medicament'=rowSums(.[2:ncol(.)]),
         across(.cols = 2:ncol(.),.fns = ~round(./`Tractaments/Medicament`*100,1),.names = '% ({.col})'),
         across(.cols=starts_with('%'),.fns = ~paste0(.,'%'))
  ) %>% 
  select(1,contains(levels.PDC[2:length(levels.PDC)]),everything()) %>% 
  left_join(Taula.resum.pacients,by='Medicament.AGRUPAT') %>% 
  mutate('Tractaments/Pacient'=round(`Tractaments/Medicament`/`Pacients/Medicament`,1))


# Plots adherències per medicament agrupat ------------------------------------
# barres 
breaks<-(max(Agrupacio.PDC.MA$Suma)/100*10) %>% round_any(10,f=ceiling)
limit.sup<-max(Agrupacio.PDC.MA$Suma) %>% round_any(breaks,f=ceiling)
plot.MA.stack<-Agrupacio.PDC.MA %>% 
  ggplot(aes(x=Medicament.AGRUPAT,y=N,fill=Classificacio.PDC))+
  geom_bar(position = 'stack',stat = 'identity',color='white')+
  scale_fill_viridis_d(name='PDC',direction = -1,option = 'D')+
  scale_y_continuous(name='Número de tractaments',
                     limits = c(0,limit.sup),
                     breaks = seq(0,limit.sup,breaks))+
  scale_x_discrete(name='Medicament (Agrupat)')+
  ggtitle('Adherencia dels pacients per medicament agrupat i estimació de la PDC',
          subtitle = '(PDC = Proporció de dies coberts)')+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1,face = 'bold'),
    axis.title = element_text(face = 'bold',size = 12),
    plot.title = element_text(hjust = 0.5,face ='bold.italic',size = 14),
    plot.subtitle = element_text(hjust=0.5,face = 'italic',size = 12),
    legend.title = element_text(face = 'bold')
  )
#barres %
plot.MA.stack.percent<-Agrupacio.PDC.MA %>% 
  ggplot(aes(x=Medicament.AGRUPAT,y=N,fill=Classificacio.PDC))+
  geom_bar(position = 'fill',stat = 'identity',color='white')+
  geom_text(aes(label=Label.P),size=3,color='white',
            position = position_fill(vjust = 0.5),
            na.rm = T,
            show.legend = F)+
  scale_fill_viridis_d(name='PDC',direction = -1,option = 'D')+
  scale_y_continuous(name='% tractaments',
                     labels=scales::percent)+
  scale_x_discrete(name='Medicament (Agrupat)')+
  ggtitle('% Adherencia dels pacients per medicament agrupat i estimació del PDC',
          subtitle = '(PDC = Proporció de dies coberts)')+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1,face = 'bold'),
    axis.text.y = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold',size = 12),
    plot.title = element_text(hjust = 0.5,face ='bold.italic',size = 14),
    plot.subtitle = element_text(hjust=0.5,face = 'italic',size = 12),
    legend.title = element_text(face = 'bold')
  )


# Plots per medicament i especialitat -------------------------------------
llistat.medicament.agrupat<-Agrupacio.PDC.Med$Medicament.AGRUPAT %>% unique()

Grafics.Especialitat.stack<-lapply(llistat.medicament.agrupat,function(x){
  
  Df.Especialitat.PDC<-Agrupacio.PDC.Med %>%
    filter(Medicament.AGRUPAT%in%x) %>% 
    group_by(Medicament.Descripció) %>% 
    dplyr::mutate(Suma=sum(N))
  
  breaks<-(max(Df.Especialitat.PDC$Suma)/100*10) %>% round_any(10,f=ceiling)
  limit.sup<-max(Df.Especialitat.PDC$Suma) %>% round_any(breaks,f=ceiling)
  num.espec<-Df.Especialitat.PDC$Medicament.Descripció %>% unique() %>% length()
  colors<-c("<0.5" ='#FDE725FF',"0.5-0.8"='#35B779FF',
            "0.8-0.9"='#31688EFF',">=0.9" ='#440154FF') #Viridis D
  
  plot.especialitat.stack<-Df.Especialitat.PDC %>% 
    ggplot(aes(x=Medicament.Descripció,y=N,fill=Classificacio.PDC))+
    geom_bar(position = 'stack',stat = 'identity',color='white',width = ifelse(num.espec/10>1,1,num.espec/10))+
    scale_fill_manual(values = colors)+ #mantinc colors llegenda
    scale_y_continuous(name='Número tractaments',
                       limits = c(0,limit.sup),
                       breaks = seq(0,limit.sup,breaks))+
    scale_x_discrete(name='Especialitat',
                     labels=function(Medicament.Descripció) str_wrap(Medicament.Descripció,21))+
    ggtitle(paste0('Adherencia dels pacients tractats amb ',x,' per especialitat i PDC'),
            subtitle = '(PDC = Proporció de dies coberts)')+
    theme_minimal()+
    theme(
      axis.text.x = element_text(angle=90,vjust = 0.5,hjust = 1,face = 'bold'),
      axis.text.y = element_text(face = 'bold'),
      axis.title = element_text(face = 'bold',size = 12),
      plot.title = element_text(hjust = 0.5,face ='bold.italic',size = 14),
      plot.subtitle = element_text(hjust=0.5,face = 'italic',size = 12),
      legend.title = element_text(face = 'bold')
    )
  return(plot.especialitat.stack)
})
Taules.Especialitat<-lapply(llistat.medicament.agrupat,function(x){
  Taula<-Agrupacio.PDC.Med %>% filter(Medicament.AGRUPAT%in%x) %>% 
    spread(Classificacio.PDC,N) %>% 
    replace(is.na(.),0) %>% 
    dplyr::mutate(Total=rowSums(.[3:ncol(.)]),
                  across(.cols = 3:ncol(.),.fns = ~round(./Total*100,1),.names = '% ({.col})'),
                  across(.cols=starts_with('%'),.fns = ~paste0(.,'%'))
                  ) %>% 
    select(2,contains(levels.PDC[2:length(levels.PDC)]),Total)
})

#afegeixo titols pel print
names(Taules.Especialitat)<-paste0('Detall dels valors d\'adherència del medicament ',llistat.medicament.agrupat,
                                          ', per especialitat. Anys ',Any.inici,'-',Any.final)


# Print Resultats Das28 ---------------------------------------------------
header_st<- createStyle(
  fontName = 'Calibri',
  fontSize = '15',
  textDecoration = c("bold", "underline")
  )
wb<-createWorkbook()
addWorksheet(wb,sheetName = 'Resum_4_anys',gridLines = F,tabColour = '#EE2C2C')
curr_row <- 1
writeData(wb,1,
          paste0('Gràfic resum del resultat dels medicaments pel tractament d\'Artritis Reumatoide en termes de Éxit/Fracás pel valor de DAS28. Anys ',Any.inici,'-',Any.final),
          startCol = 1,startRow = curr_row,headerStyle = header_st)
curr_row <- curr_row+2
Plot.resultats.n
insertPlot(wb,1,width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row )
dev.off()
curr_row <- curr_row+30
writeData(wb,1,
          paste0('Gràfic resum del % de resultat dels medicaments pel tractament d\'Artritis Reumatoide en termes de Éxit/Fracás pel valor de DAS28. Anys ',Any.inici,'-',Any.final),
          startCol = 1,startRow = curr_row,headerStyle = header_st)
curr_row <- curr_row+2
Plot.resultats.Percentatge
insertPlot(wb,1,width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row )
dev.off()
curr_row <- curr_row+30
writeData(wb,1,
          paste0('Taula resum. Resultat dels medicaments pel tractament d\'Artritis Reumatoide en termes de Éxit/Fracás pel valor de DAS28. Detall anys ',Any.inici,'-',Any.final),
          startCol = 1,startRow = curr_row,headerStyle = header_st)
curr_row <- curr_row+2
writeDataTable(wb,1,select(table1.data,-RPT.Indicació.Codi),startCol = 1, startRow = curr_row,withFilter = F,
               tableStyle = 'tableStyleMedium9')

for(i in seq_along(medicaments$RPT.ATC.Descripció)){
  curr_row <-1
  Nom.pestanya<-paste0(medicaments$RPT.ATC.Descripció[i],'(',Any.min.Das28,'_',Any.max.Das28,')')
  addWorksheet(wb,sheetName = Nom.pestanya,gridLines = F)
  pageSetup(wb,Nom.pestanya,orientation = 'landscape',paperSize = 9,fitToWidth = T,fitToHeight = T)
  writeData(wb,i+1,
           names(llistats.grafics.DAS28[i]),
            startCol = 1,startRow = curr_row,headerStyle = header_st)
  curr_row <- curr_row+2
  print(llistats.grafics.DAS28[[i]])
  insertPlot(wb,sheet=i+1,width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row )
  dev.off()
}
addWorksheet(wb,'RawData',tabColour = '#FFD700')
writeDataTable(wb,'RawData',df.dif.pas,withFilter = T)
saveWorkbook(wb,'Resultats/AR_Resultats_DAS28.xlsx',overwrite = T)


# Print Resultats Adherències ---------------------------------------------
cells<-NULL
wb2<-createWorkbook()
addWorksheet(wb2,sheetName = 'Resum_Agregat',gridLines = F,tabColour = '#EE2C2C')
curr_row<-1
cells<-c(cells,curr_row)
writeData(wb2,'Resum_Agregat',
          paste0('Resum de les adherències dels pacients als tractaments agrupats per medicament (anys ',Any.inici,'-',Any.final,')'),
          startRow = curr_row,startCol = 1,headerStyle = header_st)
curr_row<-curr_row+2
print(plot.MA.stack)
insertPlot(wb2,'Resum_Agregat',width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row)
curr_row<-curr_row+30
print(plot.MA.stack.percent)
insertPlot(wb2,'Resum_Agregat',width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row)
dev.off()
curr_row<-curr_row+30
cells<-c(cells,curr_row)
writeData(wb2,'Resum_Agregat',
          paste0('Taula resum:Adherències dels pacients als tractaments agrupats per medicament (anys ',Any.inici,'-',Any.final,')'),
          startRow = curr_row,startCol = 1)
curr_row<-curr_row+2
writeDataTable(wb2,'Resum_Agregat',Taula.PDC.MA,startCol = 1, startRow = curr_row,withFilter = F,
               tableStyle = 'tableStyleMedium9')
writeDataTable(wb2,'Resum_Agregat',Taula.Pacients.Totals.Das28,startCol = ncol(Taula.PDC.MA)+2, startRow = curr_row,withFilter = F,
               tableStyle = 'tableStyleMedium9')
conditionalFormatting(wb2,'Resum_Agregat',cols = 1,rows = cells,rule= 'A',type ='contains' ,style =header_st)


for(i in seq_along(llistat.medicament.agrupat)){
  curr_row <-1
  Nom.pestanya<-paste0(llistat.medicament.agrupat[i],'_especialitats')
  addWorksheet(wb2,sheetName = Nom.pestanya,gridLines = F)
  pageSetup(wb2,Nom.pestanya,orientation = 'landscape',paperSize = 9,fitToWidth = T,fitToHeight = T)
  writeData(wb2,i+1,
            names(Grafics.Especialitat.stack[i]),
            startCol = 1,startRow = curr_row)
  curr_row <- curr_row+2
  print(Grafics.Especialitat.stack[[i]])
  insertPlot(wb2,sheet=i+1,width =25,height = 15,units = 'cm',fileType = 'jpeg',startCol = 1,startRow = curr_row )
  dev.off()
  curr_row <- curr_row+30
  writeData(wb2,i+1,
            names(Taules.Especialitat[i]),
            startCol = 1,startRow = curr_row)
  curr_row <- curr_row+2
  writeDataTable(wb2,Nom.pestanya,Taules.Especialitat[[i]],startCol = 1, startRow = curr_row,withFilter = F,
                 tableStyle = 'tableStyleMedium9')
}
addWorksheet(wb2,'Raw_Data',gridLines = F,tabColour = '#EEC900')
writeDataTable(wb2,'Raw_Data',Taula.dades.PDC,startCol = 1,startRow = 1,withFilter = T,tableStyle = 'tableStyleMedium9')
saveWorkbook(wb2,'Resultats/AR_Resultats_Adherències.xlsx',overwrite = T)
