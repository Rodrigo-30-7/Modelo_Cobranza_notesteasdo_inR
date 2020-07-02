#Clientes que aparecen en la base de comportamiento de pago entre 201710 y 201809
cl= group_by(clientes2,ID_CLIENTE)%>%summarise(n=n())%>%data.frame()
set.seed(666) #semilla
#vector de periodos de referencia 201710-201809
periodos=sort(unique(clientes2$periodo_ref)) 
cl= cl[,1]%>%data.frame()
names(cl)<-"ID_CLIENTE"
muestra_periodos<- sample(periodos,size=nrow(cl),replace=TRUE)%>%data.frame()
names(muestra_periodos)<-"LID_TIEMPO"
cl<-cbind(cl,muestra_periodos)
cl$marca_muestra<-1

#Cruce muestra periodos con comportam0iento de pago
muestra_comp=merge(clientes2,cl, by=c("ID_CLIENTE", "LID_TIEMPO")) #186083

# nos quedamos con aquellos casos en que la solicitud tiene entre 1 y 120 días de mora
muestra_comp=filter(muestra_comp,LDIASDEMORA >= 1 & LDIASDEMORA <= 120) #29819

#Se hace base de los padres con contratos hijo en la muestra
file <- paste0(INPUTDIR, "PadresRenegociados.csv")
padresRenegociados=read.csv(file=file,stringsAsFactors=F, header = T,sep = ";") %>% data.frame()


hijos_muestra=filter(padresRenegociados,LIDTPLCABEZACOTIZAP %in% muestra_comp$ID_SOLICITUD)%>%select(LIDTPLCABEZACOTIZAP,LIDTPLCABEZACOTIZA)

#Agregamos motivo de cancelación

file <- paste0(INPUTDIR, "base_motivos_cancelacion.csv")
mot_cancelacion=read.csv(file=file,stringsAsFactors=F, header = T,sep = ";") %>% data.frame()
#Para trabajar mejor colocamos la fecha de cancelación a principio de mes, lo que importa es el mes de la cancelación

mot_cancelacion$fecha_motivo=ymd(paste(substring(mot_cancelacion$periodo_motivo_am,1,4),substring(mot_cancelacion$periodo_motivo_am,5,6),"01",sep="-"))

muestra_comp=merge(muestra_comp,mot_cancelacion, by=c("ID_SOLICITUD"), all.x = TRUE)

#Se crea marca de mes motivo de cancelación para los filtros
#Perido_ref=LID_TIEMPO
muestra_comp$fecha_ref=ymd(paste(substring(muestra_comp$periodo_ref,1,4),substring(muestra_comp$periodo_ref,5,6),"01",sep="-"))

muestra_comp$mes_motivo=ifelse(0<=elapsed_months(muestra_comp$fecha_motivo,muestra_comp$fecha_ref) & elapsed_months(muestra_comp$fecha_motivo,muestra_comp$fecha_ref) <=6,elapsed_months(muestra_comp$fecha_motivo,muestra_comp$fecha_ref),NA)
#mes_motivo: diferencia entre el mes de referencia y de la cancelación === >> 0 significa que es el mismo mes, 1 el rezago sgte


#Clientes con comportamiento de pago en los 6 meses
#Mora_f1 - Mora_f6 son los días mora en los meses siguientes al mes de referencia
muestra_comp$marca_comp_pago6=apply(select(muestra_comp,mora_f1:mora_f6),1,function(x){ifelse(sum(x>=0,na.rm=TRUE)==6,1,0)}) 
# marca que indica si el cliente tuvo los 6 meses de comportamiento en la base comp de pago
muestra_comp$marca_comp_pago6=ifelse(is.na(muestra_comp$marca_comp_pago6),0,muestra_comp$marca_comp_pago6)

#Se crea marca de los contratos que tienen hijos
#Se crea marca de los contratos que tienen hijos

file <- paste0(INPUTDIR, "base_fprimervencimiento.csv")
primervenc=read.csv(file=file,stringsAsFactors=F, header = T,sep = ";") %>% data.frame()
names(primervenc)=c("ID_SOLICITUD","fvenc")
primervenc$fvenc_am=paste0(substring(primervenc$fvenc,7,10),substring(primervenc$fvenc,4,5))%>%as.numeric()

hijos_muestra=merge(hijos_muestra,primervenc,by.x="LIDTPLCABEZACOTIZA", by.y="ID_SOLICITUD",all.x=TRUE)

comp_pago_hijos<- comp_pago[,c(1,3,5)]
comp_pago_hijos<- filter(comp_pago_hijos,ID_SOLICITUD %in% hijos_muestra$LIDTPLCABEZACOTIZA)


#Se crean los rezagos de los hijos
cl_ref=comp_pago_hijos
cl_ref$periodo_ref=cl_ref$LID_TIEMPO

periodos=sort(unique(cl_ref$periodo_ref))


hijos3=list()
for(i in 1:length(periodos)){
  print(c(i,length(periodos)))
  
  hijos_per=unique(cl_ref[which(cl_ref$periodo_ref==periodos[i]),1])
  hijos_filtrados=filter(cl_ref,ID_SOLICITUD %in% hijos_per & periodo_ref==periodos[i]) 
  periodo_rez=f_rezagosF(periodos[i],12)[2:4]
  
  for(j in 1:length(periodo_rez)){
    print(paste0("fut_",j))
    
    rezago_mora=filter(comp_pago_hijos, (ID_SOLICITUD %in% hijos_per) & LID_TIEMPO==periodo_rez[[j]]) %>% select(ID_SOLICITUD,LDIASDEMORA)
    names(rezago_mora)=c("ID_SOLICITUD",paste0("mora_f",j))
    hijos_filtrados=merge(hijos_filtrados,rezago_mora, by="ID_SOLICITUD", all.x=TRUE)
    
  }
  hijos3[[i]]=hijos_filtrados
}

hijos2=do.call(rbind.data.frame, hijos3)
hijos_muestra=merge(hijos_muestra,hijos2,by.x=c("LIDTPLCABEZACOTIZA","fvenc_am"), by.y=c("ID_SOLICITUD","LID_TIEMPO"),all.x=TRUE)hijos_muestra$marca_fv=ifelse(is.na(hijos_muestra$LDIASDEMORA)& !is.na(hijos_muestra$fvenc_am),1,0)
#Hay contratos "hijo" que aparecen en la base de comportamiento de pagos un mes después de su
#primer vencimiento por lo que para pegar el comportamiento se le sumo un mes a su fvenc
hijos_muestra1=filter(hijos_muestra, hijos_muestra$marca_fv==0)
hijos_muestra2=filter(hijos_muestra, hijos_muestra$marca_fv==1)
hijos_muestra2$fvenc2=ymd(paste(substring(hijos_muestra2$fvenc_am,1,4),substring(hijos_muestra2$fvenc_am,5,6),"01",sep="-"))
hijos_muestra2$fvenc3=ymd(hijos_muestra2$fvenc2)+months(1)
hijos_muestra2$fvenc_am2=paste0(substr(hijos_muestra2$fvenc3,1,4),substr(hijos_muestra2$fvenc3,6,7))
hijos_muestra2<-hijos_muestra2[,-c(2,4:9,11)]
hijos_muestra2<-rename(hijos_muestra2, fvenc=fvenc3,fvenc_am=fvenc_am2)
hijos_muestra2=merge(hijos_muestra2,hijos2,by.x=c("LIDTPLCABEZACOTIZA","fvenc_am"), by.y=c("ID_SOLICITUD","LID_TIEMPO"),all.x=TRUE)

muestra_hijos<- rbind(hijos_muestra1,hijos_muestra2)

muestra_hijos$marca_ci<-ifelse(muestra_hijos$LIDTPLCABEZACOTIZA==muestra_hijos$LIDTPLCABEZACOTIZAP,1,0)
muestra_hijos$marca_hijo<-1
muestra_hijos<-rename(muestra_hijos, mora_f1_h=mora_f1, mora_f2_h=mora_f2, mora_f3_h=mora_f3, LDIASDEMORA_HIJO=LDIASDEMORA)
base_muestra<-merge(muestra_comp,muestra_hijos,by.x="ID_SOLICITUD", by.y="LIDTPLCABEZACOTIZAP",all.x=TRUE)


#Filtramos los casos donde el #de contrato padre es igual al #de contrato hijo 183 casos
base_ci<- filter(base_muestra,marca_ci==1) 
base_muestra<-filter(base_muestra,marca_ci!=1 | is.na(base_muestra$marca_ci)) 

# Se consideran como NO RECUPERADO los casos que se cancelan en los siguientes 6 meses por alguno de los siguientes motivos

base_muestra$motivo_malo6=ifelse(base_muestra$CMOTIVOCANCELACION %in% c("(K) CASTIGADO","(V) RECUPERADO", "CASTIGO MORA") & elapsed_months(base_muestra$fecha_motivo,base_muestra$fecha_ref)<=6 ,1,0)
base_muestra$motivo_malo6[is.na(base_muestra$motivo_malo6)]=0

# Se consideran como RECUPERADO los casos que se cancelan en los siguientes 6 meses por alguno de los siguientes motivos
#"(E) EXTINTO","PREPAGO WEB", "(P) PREPAGADO","(D) DEVOLUCION VOLUNTARIA","(N) NOVACION","(I) DEVOLUCION C. INTELIGENTE","PREPAGO PARCIAL",
#"(U) PREPAGO JUDICIAL", "PREPAGO WEB JUDICIAL"
# (R) RENEGOCIADO RENEGOCIACION SENCILLA sin hijos

base_muestra$motivo_bueno6<-ifelse(base_muestra$CMOTIVOCANCELACION %in% c("(E) EXTINTO","PREPAGO WEB", 
                                                                          "(P) PREPAGADO","(D) DEVOLUCION VOLUNTARIA","(N) NOVACION","(I) DEVOLUCION C. INTELIGENTE","PREPAGO PARCIAL",
                                                                          "(U) PREPAGO JUDICIAL", "PREPAGO WEB JUDICIAL") & elapsed_months(base_muestra$fecha_motivo,base_muestra$fecha_ref)<=6 ,1,0)

base_muestra$marca_rsh<-ifelse(base_muestra$CMOTIVOCANCELACION %in% c("(R) RENEGOCIADO","RENEGOCIACION SENCILLA",
                                                                      "(F) REFINANCIAMIENTO VFMG") & is.na(base_muestra$marca_hijo) & elapsed_months(base_muestra$fecha_motivo,base_muestra$fecha_ref)<=6,1,0)

base_muestra$marca_rch<-ifelse(base_muestra$CMOTIVOCANCELACION %in% c("(R) RENEGOCIADO","RENEGOCIACION SENCILLA",
                                                                      "(F) REFINANCIAMIENTO VFMG") & base_muestra$marca_hijo==1 & elapsed_months(base_muestra$fecha_motivo,base_muestra$fecha_ref)<=6,1,0)
base_muestra$marca_rch<- ifelse(is.na(base_muestra$marca_rch),0,base_muestra$marca_rch)


#Separamos la muestra de los refinanciados cancelados con hijos o sin hijos cancelados antes de los 6 meses observados
base_muestra1<- filter(base_muestra, marca_rch!=1 & marca_rsh!=1)

base_muestra2<- filter(base_muestra, marca_rch==1 ) #Muestra renegociados o refinanciados con hijos cancelados antes de los 6 meses observados


#Luego de esto se construye la marca de recupero según lo definido en el punto 3.2 para cada base y se filtran los casos donde la marca queda NA.

#######################################################
###### CAMBIO DE TRAMO
#######################################################

#Para definir si un cliente se recupera o no se debe calcular si mejora su situación de impago
#Y en qué mes mejora
# Si tenía 30 o menos días de mora que llegue a 0 
#si tenía de 30 a 60 q baje a menos 30, y así sucesivamente

#Dividimos la muestra para trabajar por separado los que tienen comportamiento de pago
#en los 6 meses observados y los que no, ya que dependiendo de cada caso se tienen que 
#evaluar condiciones diferentes
#Cuando tiene comportamiento de pago en los 6 meses

muestra_cp12<- filter(base_muestra1,marca_comp_pago6==1)

muestra_cp12$ct1<-ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f1 ==0,1,
                         ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f1 <30,1,
                                ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f1 <60,1,
                                       ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f1 <90,1,0))))

muestra_cp12$ct2<- ifelse(muestra_cp12$ct1==1,1,
                          ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f2 ==0,1,
                                 ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f2 <30,1,
                                        ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f2 <60,1,
                                               ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f2 <90,1,0)))))

muestra_cp12$ct3<- ifelse(muestra_cp12$ct2==1,1,
                          ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f3 ==0,1,
                                 ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f3 <30,1,
                                        ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f3 <60,1,
                                               ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f3 <90,1,0)))))

muestra_cp12$ct4<- ifelse(muestra_cp12$ct3==1,1,
                          ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f4 ==0,1,
                                 ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f4 <30,1,
                                        ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f4 <60,1,
                                               ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f4 <90,1,0)))))

muestra_cp12$ct5<- ifelse(muestra_cp12$ct4==1,1,
                          ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f5 ==0,1,
                                 ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f5 <30,1,
                                        ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f5 <60,1,
                                               ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f5 <90,1,0)))))

muestra_cp12$ct6<- ifelse(muestra_cp12$ct5==1,1,
                          ifelse(muestra_cp12$LDIASDEMORA<=30 & muestra_cp12$mora_f6 ==0,1,
                                 ifelse(muestra_cp12$LDIASDEMORA>30 & muestra_cp12$LDIASDEMORA<=60 & muestra_cp12$mora_f6 <30,1,
                                        ifelse(muestra_cp12$LDIASDEMORA>60 & muestra_cp12$LDIASDEMORA<=90 & muestra_cp12$mora_f6 <60,1,
                                               ifelse(muestra_cp12$LDIASDEMORA>90 & muestra_cp12$LDIASDEMORA<=120 & muestra_cp12$mora_f6 <90,1,0)))))



# Cuando no tiene comportamiento de pago en los 6 meses

muestra_scp12<- filter(base_muestra1,marca_comp_pago6==0)

muestra_scp12$ct1<-ifelse(muestra_scp12$mes_motivo<= 1 & muestra_scp12$motivo_malo6 == 1,0,
                          ifelse(muestra_scp12$mes_motivo<= 1 & muestra_scp12$motivo_bueno6 == 1,1,
                                 ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f1 ==0,1,
                                        ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f1 <30,1,
                                               ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f1 <60,1,
                                                      ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f1 <90,1,0))))))

muestra_scp12$ct2<- ifelse(muestra_scp12$ct1==1,1,
                           ifelse(muestra_scp12$mes_motivo<= 2 & muestra_scp12$motivo_malo6 == 1,0,
                                  ifelse(muestra_scp12$mes_motivo<= 2 & muestra_scp12$motivo_bueno6 == 1,1,
                                         ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f2 ==0,1,
                                                ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f2 <30,1,
                                                       ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f2 <60,1,
                                                              ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f2 <90,1,0)))))))

muestra_scp12$ct3<- ifelse(muestra_scp12$ct2==1,1,
                           ifelse(muestra_scp12$mes_motivo<= 3 & muestra_scp12$motivo_malo6 == 1,0,
                                  ifelse(muestra_scp12$mes_motivo<= 3 & muestra_scp12$motivo_bueno6 == 1,1,
                                         ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f3 ==0,1,
                                                ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f3 <30,1,
                                                       ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f3 <60,1,
                                                              ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f3 <90,1,0)))))))

muestra_scp12$ct4<- ifelse(muestra_scp12$ct3==1,1,
                           ifelse(muestra_scp12$mes_motivo<= 4 & muestra_scp12$motivo_malo6 == 1,0,
                                  ifelse(muestra_scp12$mes_motivo<= 4 & muestra_scp12$motivo_bueno6 == 1,1,
                                         ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f4 ==0,1,
                                                ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f4 <30,1,
                                                       ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f4 <60,1,
                                                              ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f4 <90,1,0)))))))

muestra_scp12$ct5<- ifelse(muestra_scp12$ct4==1,1,
                           ifelse(muestra_scp12$mes_motivo<= 5 & muestra_scp12$motivo_malo6 == 1,0,
                                  ifelse(muestra_scp12$mes_motivo<= 5 & muestra_scp12$motivo_bueno6 == 1,1,
                                         ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f5 ==0,1,
                                                ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f5 <30,1,
                                                       ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f5 <60,1,
                                                              ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f5 <90,1,0)))))))

muestra_scp12$ct6<- ifelse(muestra_scp12$ct5==1,1,
                           ifelse(muestra_scp12$mes_motivo<= 6 & muestra_scp12$motivo_malo6 == 1,0,
                                  ifelse(muestra_scp12$mes_motivo<= 6 & muestra_scp12$motivo_bueno6 == 1,1,
                                         ifelse(muestra_scp12$LDIASDEMORA<=30 & muestra_scp12$mora_f6 ==0,1,
                                                ifelse(muestra_scp12$LDIASDEMORA>30 & muestra_scp12$LDIASDEMORA<=60 & muestra_scp12$mora_f6 <30,1,
                                                       ifelse(muestra_scp12$LDIASDEMORA>60 & muestra_scp12$LDIASDEMORA<=90 & muestra_scp12$mora_f6 <60,1,
                                                              ifelse(muestra_scp12$LDIASDEMORA>90 & muestra_scp12$LDIASDEMORA<=120 & muestra_scp12$mora_f6 <90,1,0)))))))



muestra_cp12$mes_ct=apply(select(muestra_cp12,ct1:ct6),1,function(x){ifelse(sum(x==1)>0,7-sum(x==1),0)})
muestra_scp12$mes_ct=apply(select(muestra_scp12,ct1:ct6),1,function(x){ifelse(sum(x==1)>0,7-sum(x==1),0)})


#Unimos las dos muestras

muestra1<- rbind(muestra_cp12,muestra_scp12)

muestra1$marca_na=apply(select(muestra1,ct1:ct6),1,function(x){ifelse(sum(is.na(x))>0,1,0)})

#Se marcan los casos con NA en los cambios de tramo que tengan motivo de cancelación "(S) SINIESTRO" y "(x) NULO"

muestra1$marca_sn<-ifelse(muestra1$CMOTIVOCANCELACION %in% c("(X) NULO","(S) SINIESTRO") & muestra1$marca_na==1,1,0)

muestra1$marca_hrec<-NA


###Ahora trabajamos con los refinanciados y renegociados con hijos

#Si un contrato se cancela y es renegociado o refinanciado se analiza el comportamiento 
#de pago de los 3 primeros meses del contrato hijo
# Para considerarse como recuperado en los 3 meses los días de mora del contrato hijo no debe ser
#mayor a 5
#Si el contrato se marca como recuperado antes del mes de cancelación se considera recuperado

#Creamos una marca de hijo recuperado

base_muestra2$marca_hrec<-ifelse(base_muestra2$mora_f1_h<5 & base_muestra2$mora_f2_h<5 & base_muestra2$mora_f3_h<5,1,0)
base_muestra2$marca_hrec<-ifelse(is.na(base_muestra2$marca_hrec),0,base_muestra2$marca_hrec)

base_muestra2$ct1<-ifelse(base_muestra2$mes_motivo<= 1 & base_muestra2$marca_hrec== 1,1,
                          ifelse(base_muestra2$mes_motivo<= 1 & base_muestra2$marca_hrec== 0,0,
                                 ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f1 ==0,1,
                                        ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f1 <30,1,
                                               ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f1 <60,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f1 <90,1,0))))))

base_muestra2$ct2<-ifelse(base_muestra2$ct1==1,1,
                          ifelse(base_muestra2$mes_motivo<= 2 & base_muestra2$marca_hrec== 1,1,
                                 ifelse(base_muestra2$mes_motivo<= 2 & base_muestra2$marca_hrec== 0,0,
                                        ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f2 ==0,1,
                                               ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f2 <30,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f2 <60,1,
                                                             ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f2 <90,1,0)))))))

base_muestra2$ct3<-ifelse(base_muestra2$ct2==1,1,
                          ifelse(base_muestra2$mes_motivo<= 3 & base_muestra2$marca_hrec== 1,1,
                                 ifelse(base_muestra2$mes_motivo<= 3 & base_muestra2$marca_hrec== 0,0,
                                        ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f3 ==0,1,
                                               ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f3 <30,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f3 <60,1,
                                                             ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f3 <90,1,0)))))))

base_muestra2$ct4<-ifelse(base_muestra2$ct3==1,1,
                          ifelse(base_muestra2$mes_motivo<= 4 & base_muestra2$marca_hrec== 1,1,
                                 ifelse(base_muestra2$mes_motivo<= 4 & base_muestra2$marca_hrec== 0,0,
                                        ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f4 ==0,1,
                                               ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f4 <30,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f4 <60,1,
                                                             ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f4 <90,1,0)))))))

base_muestra2$ct5<-ifelse(base_muestra2$ct4==1,1,
                          ifelse(base_muestra2$mes_motivo<= 5 & base_muestra2$marca_hrec== 1,1,
                                 ifelse(base_muestra2$mes_motivo<= 5 & base_muestra2$marca_hrec== 0,0,
                                        ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f5 ==0,1,
                                               ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f5 <30,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f5 <60,1,
                                                             ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f5 <90,1,0)))))))

base_muestra2$ct6<-ifelse(base_muestra2$ct5==1,1,
                          ifelse(base_muestra2$mes_motivo<= 6 & base_muestra2$marca_hrec== 1,1,
                                 ifelse(base_muestra2$mes_motivo<= 6 & base_muestra2$marca_hrec== 0,0,
                                        ifelse(base_muestra2$LDIASDEMORA<=30 & base_muestra2$mora_f6 ==0,1,
                                               ifelse(base_muestra2$LDIASDEMORA>30 & base_muestra2$LDIASDEMORA<=60 & base_muestra2$mora_f6 <30,1,
                                                      ifelse(base_muestra2$LDIASDEMORA>60 & base_muestra2$LDIASDEMORA<=90 & base_muestra2$mora_f6 <60,1,
                                                             ifelse(base_muestra2$LDIASDEMORA>90 & base_muestra2$LDIASDEMORA<=120 & base_muestra2$mora_f6 <90,1,0)))))))


base_muestra2$mes_ct=apply(select(base_muestra2,ct1:ct6),1,function(x){ifelse(sum(x==1)>0,7-sum(x==1),0)})

base_muestra2$marca_na=apply(select(base_muestra2,ct1:ct6),1,function(x){ifelse(sum(is.na(x))>0,1,0)})

base_muestra2$marca_sn<-NA

#Unimos con muestra 1

muestra2<- rbind(muestra1,base_muestra2) #Muestra sin renegociados o refinanciados sin hijos


#Se filtran los casos con NA en los cambios de tramo que tengan motivo de cancelación "(S) SINIESTRO" y "(x) NULO"
muestra2<-filter(muestra2, marca_sn==0 | is.na(marca_sn) )


#######################################################
#             Marca recupero                          #
#######################################################

muestra2$marca_recupero<-ifelse(muestra2$mes_ct>=1 & muestra2$mes_ct<=6,1,0)

#Hay casos con NA en la marca de recupero, ya que desaparecen de la base de comportamiento, vuelven a aparecer
#o en el motivo de cancelación tienen números en vez del motivo

muestra_contratos<-filter(muestra2, marca_na==0)