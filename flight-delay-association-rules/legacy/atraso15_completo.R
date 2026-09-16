v$atrasop<-0
v$atrasoc<-0
v1<-subset(v,is.na(v$tempoatrasopartida)|
             v$tempoatrasopartida>15)
v1$atrasop<-1
v2<-subset(v,v$tempoatrasopartida<=15)
v<-rbind(v1,v2)
v1<-subset(v,is.na(v$tempoatrasochegada)|
             v$tempoatrasochegada>15)
v1$atrasoc<-1
v2<-subset(v,v$tempoatrasochegada<=15)
v<-rbind(v1,v2)

voo<-v

voo$contadoratr<-5
voo$datahoraref<-strptime('1900-01-01 00:00:00', "%Y-%m-%d %H:%M:%S")
voo$contador<-1000
voo$atraso<-1000
voo$atperc<-5
voo$atperc_c<-5

for (a in 1:nrow(aeroporto)){
  
  
  kaero1<-subset(voo, aeroportopartida==aeroporto[a,1])
  kaero2<-subset(voo, aeroportochegada==aeroporto[a,1])
  kaero1$datahoraref<-strptime(kaero1$partidaprevista, 
                               "%Y-%m-%d %H:%M:%S")
  kaero2$datahoraref<-strptime(kaero2$chegadaprevista, 
                               "%Y-%m-%d %H:%M:%S")
  
  
  kk<-subset(voo, aeroportopartida!=aeroporto[a,1] & 
               aeroportochegada!=aeroporto[a,1])
  kaero<-rbind(kaero1,kaero2)
  
  kaero_order<-kaero[order(kaero$datahoraref),]
  
  p<-subset(kaero_order,aeroportopartida==aeroporto[a,1])
  c<-subset(kaero_order,aeroportochegada==aeroporto[a,1])
  
  p1<-subset(p,tempoatrasopartida>15|is.na(tempoatrasopartida))
  p2<-subset(p,tempoatrasopartida<=15)
  
  c1<-subset(c,tempoatrasochegada>15|is.na(tempoatrasochegada))
  c2<-subset(c,tempoatrasochegada<=15)
  
  p1$contadoratr<-1
  p2$contadoratr<-0
  c1$contadoratr<-1
  c2$contadoratr<-0
  
  p<-rbind(p1,p2)
  c<-rbind(c1,c2)
  
  kaero_order<-rbind(p,c)
  kaero_order<-kaero_order[order(kaero_order$datahoraref),]
  
  
  for (j in 1:nrow(kaero_order)) {
    
    x1<-kaero_order$datahoraref[j]
    x2<-x1-3600
    
    w<-max(1,j-300)
    z<-max(1,j-1)
    
    kaero_order$contador[j]<-sum(kaero_order$datahoraref[c(w:z)]>=x2 &
                                   kaero_order$datahoraref[c(w:z)]<x1)
    
    kaero_order$atraso[j]<-sum(kaero_order$datahoraref[c(w:z)]>=x2 &
                                 kaero_order$datahoraref[c(w:z)]<x1 &
                                 kaero_order$contadoratr[c(w:z)]==1) 
    
    print (j)
    
  }
  
  kaero_order1<-subset(kaero_order,
            kaero_order$aeroportopartida==aeroporto[a,1])
  
  kaero_order11<-subset(kaero_order1,kaero_order1$contador==0)
  kaero_order11$atperc<-0
  
  kaero_order12<-subset(kaero_order1,kaero_order1$contador!=0)
  kaero_order12$atperc<-kaero_order12$atraso/kaero_order12$contador
  
  kaero_order1<-rbind(kaero_order11,kaero_order12)
  
  kaero_order2<-subset(kaero_order,
            kaero_order$aeroportochegada==aeroporto[a,1])
  
  kaero_order21<-subset(kaero_order2,kaero_order2$contador==0)
  kaero_order21$atperc_c<-0
  
  kaero_order22<-subset(kaero_order2,kaero_order2$contador!=0)
  kaero_order22$atperc_c<-kaero_order22$atraso/kaero_order22$contador
  
  kaero_order2<-rbind(kaero_order21,kaero_order22)
  
  kaero_order<-rbind(kaero_order1,kaero_order2)
  
  kaero_order<-kaero_order[order(kaero_order$datahoraref),]
  
  voo<-rbind(kk,kaero_order)
  
  #print(a)
  save(voo,file="/home/anac/voo_new/voo.RData")

}


#BINNING

rm(t)
rm(tt)
rm(ttt)

w <- data.frame(voo$atperc,0)
u<-data.frame(voo$atperc_c,0)
colnames(w)<-c("x")
colnames(u)<-c("x")
t<-rbind(w,u)

tt<-subset(t,!is.na(x))
rm(w)
rm(u)

ttt <- remove_outliers(tt)

rm(z)

z<-data.frame(0,0)


k <- quantile(ttt$x, c(0.0, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[1,1]<-mean((b-a)/2)        
z[1,2]<-2

k <- quantile(ttt$x, c(0.0,0.5, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[2,1]<-mean((b-a)/2)        
z[2,2]<-3

k <- quantile(ttt$x, c(0.0,0.33,0.67, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[3,1]<-mean((b-a)/2)        
z[3,2]<-4


k <- quantile(ttt$x, c(0.0, 0.25, 0.5, 0.75, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[4,1]<-mean((b-a)/2)        
z[4,2]<-5

k <- quantile(ttt$x, c(0.0, 0.20, 0.40, 0.60, 0.80,1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[5,1]<-mean((b-a)/2)        
z[5,2]<-6

k <- quantile(ttt$x, c(0.0, 0.17, 0.33, 0.5, 0.67, 0.84, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[6,1]<-mean((b-a)/2)        
z[6,2]<-7       

k <- quantile(ttt$x, c(0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[7,1]<-mean((b-a)/2)        
z[7,2]<-8

k <- quantile(ttt$x, c(0.0, 0.11, 0.22, 0.33, 0.44, 0.55, 0.66, 0.77, 0.88, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[8,1]<-mean((b-a)/2)        
z[8,2]<-9

k <- quantile(ttt$x, c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[9,1]<-mean((b-a)/2)        
z[9,2]<-10

colnames(z)<-c("media","num")


#save(z,file="G:/Alice/Documents/Mestrado/Avioes/TRE/z_vis2.RData")

curvature.max(z$num,z$media,df=3)


# TRANSFORMATION

voo$time_dep<-ifelse(strptime
(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=5
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=8, 
"early morning",
                     
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=9
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=10, 
"mid morning",
                            
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=11
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=12, 
"late morning",
                                   
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=13
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=16, 
"afternoon",
                                          
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=17
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=19, 
"early evening",
                                                 
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=20
& strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=22, 
"late evening",
                                                        
ifelse(strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour>=23
| strptime(voo$partidaprevista, "%Y-%m-%d %H:%M:%S")$hour<=4, 
"night", "NA")))))))



voo$time_arr<-ifelse(strptime
(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=5
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=8, 
"early morning",
                     
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=9
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=10, 
"mid morning",
                            
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=11
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=12, 
"late morning",
                                  
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=13
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=16, 
"afternoon",
                                          
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=17
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=19, 
"early evening",
                                                 
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=20
& strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=22, 
"late evening",
                                                        
ifelse(strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour>=23
| strptime(voo$chegadaprevista, "%Y-%m-%d %H:%M:%S")$hour<=4, 
"night", "NA")))))))

# Delay percentages


voo$delay_level_dep<-ifelse(voo$atperc>=0 & 
          voo$atperc<0.1176471, "low",
          ifelse(voo$atperc>=0.1176471 & 
          voo$atperc<0.3000000, "medium",      
          ifelse(voo$atperc>=0.3000000 & 
          voo$atperc<=1, "high","NA")))


voo$delay_level_arr<-ifelse(voo$atperc_c>=0 & 
            voo$atperc_c<0.1176471, "low",
            ifelse(voo$atperc_c>=0.1176471 & 
            voo$atperc_c<0.3000000, "medium",      
            ifelse(voo$atperc_c>=0.3000000 & 
            voo$atperc_c<=1, "high","NA")))


# Pressure

voo$pressure_dep<-ifelse(voo$sea_level_pressurehpa_dep>=996 & 
            voo$sea_level_pressurehpa_dep<1014, "low",
            ifelse(voo$sea_level_pressurehpa_dep>=1014 & 
            voo$sea_level_pressurehpa_dep<1018, "medium",      
            ifelse(voo$sea_level_pressurehpa_dep>=1018 & 
            voo$sea_level_pressurehpa_dep<=1036, "high","NA")))


voo$pressure_arr<-ifelse(voo$sea_level_pressurehpa_arr>=996 & 
            voo$sea_level_pressurehpa_arr<1014, "low",
            ifelse(voo$sea_level_pressurehpa_arr>=1014 & 
            voo$sea_level_pressurehpa_arr<1018, "medium",      
            ifelse(voo$sea_level_pressurehpa_arr>=1018 & 
            voo$sea_level_pressurehpa_arr<=1036, "high","NA")))

# Humidity

voo$humid_dep<-ifelse(voo$humidity_dep>=4 & 
              voo$humidity_dep<65, "low",
              ifelse(voo$humidity_dep>=65 & 
              voo$humidity_dep<82, "medium",      
              ifelse(voo$humidity_dep>=82 & 
              voo$humidity_dep<=100, "high","NA")))


voo$humid_arr<-ifelse(voo$humidity_arr>=4 & 
              voo$humidity_arr<65, "low",
              ifelse(voo$humidity_arr>=65 & 
              voo$humidity_arr<82, "medium",      
              ifelse(voo$humidity_arr>=82 & 
              voo$humidity_arr<=100, "high","NA")))

# Dry-bulb temperature

voo$temp_dep<-ifelse(voo$temperature_dep>=-3 & 
              voo$temperature_dep<21, "low",
              ifelse(voo$temperature_dep>=21 & 
              voo$temperature_dep<26, "medium",      
              ifelse(voo$temperature_dep>=26 & 
              voo$temperature_dep<=41, "high","NA")))


voo$temp_arr<-ifelse(voo$temperature_arr>=-3 & 
              voo$temperature_arr<21, "low",
              ifelse(voo$temperature_arr>=21 & 
              voo$temperature_arr<26, "medium",      
              ifelse(voo$temperature_arr>=26 & 
              voo$temperature_arr<=41, "high","NA")))

# Visibility
voo$visibility_dep<-ifelse(voo$visibilitykm_dep>=0 &
                    voo$visibilitykm_dep<4.82803,"IFR",
                    ifelse(voo$visibilitykm_dep>=4.82803,"VFR",
                    "NA"))

voo$visibility_arr<-ifelse(voo$visibilitykm_arr>=0 &
                    voo$visibilitykm_arr<4.82803,"IFR",
                    ifelse(voo$visibilitykm_arr>=4.82803,"VFR",
                    "NA"))

# Wind speed
voo$wspeed_dep<-ifelse(voo$wind_speedkm_h_dep>=0 &
  voo$wind_speedkm_h_dep<1.852, "Calm",
  ifelse(voo$wind_speedkm_h_dep<=5.556, "Light air",
  ifelse(voo$wind_speedkm_h_dep<=11.112, "Light breeze",
  ifelse(voo$wind_speedkm_h_dep<=18.52, "Gentle breeze",              
  ifelse(voo$wind_speedkm_h_dep<=29.632,"Moderate breeze",
  ifelse(voo$wind_speedkm_h_dep<=38.892, "Fresh breeze",       
  ifelse(voo$wind_speedkm_h_dep<=50.004, "Strong breeze",
  ifelse(voo$wind_speedkm_h_dep<=61.116, "Near gale",           
  ifelse(voo$wind_speedkm_h_dep<=74.08, "Gale",
  ifelse(voo$wind_speedkm_h_dep<=87.044, "Strong gale",
  ifelse(voo$wind_speedkm_h_dep<=101.86, "Storm",
  ifelse(voo$wind_speedkm_h_dep<=116.676, "Violent storm",  
  ifelse(voo$wind_speedkm_h_dep>116.676, "Hurricane", "NA")))))))))))))

voo$wspeed_arr<-ifelse(voo$wind_speedkm_h_arr>=0 &
voo$wind_speedkm_h_arr<1.852, "Calm",
ifelse(voo$wind_speedkm_h_arr<=5.556, "Light air",
ifelse(voo$wind_speedkm_h_arr<=11.112, "Light breeze",
ifelse(voo$wind_speedkm_h_arr<=18.52, "Gentle breeze",              
ifelse(voo$wind_speedkm_h_arr<=29.632,"Moderate breeze",
ifelse(voo$wind_speedkm_h_arr<=38.892, "Fresh breeze",       
ifelse(voo$wind_speedkm_h_arr<=50.004, "Strong breeze",
ifelse(voo$wind_speedkm_h_arr<=61.116, "Near gale",           
ifelse(voo$wind_speedkm_h_arr<=74.08, "Gale",
ifelse(voo$wind_speedkm_h_arr<=87.044, "Strong gale",
ifelse(voo$wind_speedkm_h_arr<=101.86, "Storm",
ifelse(voo$wind_speedkm_h_arr<=116.676, "Violent storm",  
ifelse(voo$wind_speedkm_h_arr>116.676, "Hurricane", "NA")))))))))))))

# Wind direction

voo$wdirection_dep<-ifelse(voo$winddirdegrees_dep<=11,"N",
ifelse(voo$winddirdegrees_dep>=349 & voo$winddirdegrees_dep<=360,"N",
ifelse(voo$winddirdegrees_dep>=12 & voo$winddirdegrees_dep<=33,"NNE",
ifelse(voo$winddirdegrees_dep>=34 & voo$winddirdegrees_dep<=56,"NE",
ifelse(voo$winddirdegrees_dep>=57 & voo$winddirdegrees_dep<=78,"ENE",
ifelse(voo$winddirdegrees_dep>=79 & voo$winddirdegrees_dep<=101,"E",
ifelse(voo$winddirdegrees_dep>=102 & voo$winddirdegrees_dep<=123,"ESE",
ifelse(voo$winddirdegrees_dep>=124 & voo$winddirdegrees_dep<=146,"SE",
ifelse(voo$winddirdegrees_dep>=147 & voo$winddirdegrees_dep<=168,"SSE",
ifelse(voo$winddirdegrees_dep>=169 & voo$winddirdegrees_dep<=191,"S",
ifelse(voo$winddirdegrees_dep>=192 & voo$winddirdegrees_dep<=213,"SSO",
ifelse(voo$winddirdegrees_dep>=214 & voo$winddirdegrees_dep<=236,"SO",
ifelse(voo$winddirdegrees_dep>=237 & voo$winddirdegrees_dep<=258,"OSO",
ifelse(voo$winddirdegrees_dep>=259 & voo$winddirdegrees_dep<=281,"O",
ifelse(voo$winddirdegrees_dep>=282 & voo$winddirdegrees_dep<=303,"ONO",
ifelse(voo$winddirdegrees_dep>=304 & voo$winddirdegrees_dep<=326,"NO",
ifelse(voo$winddirdegrees_dep>=327 & voo$winddirdegrees_dep<=348,"NNO",
       "NA")))))))))))))))))

voo$wdirection_arr<-ifelse(voo$winddirdegrees_arr<=11,"N",
ifelse(voo$winddirdegrees_arr>=349 & voo$winddirdegrees_arr<=360,"N",
ifelse(voo$winddirdegrees_arr>=12 & voo$winddirdegrees_arr<=33,"NNE",
ifelse(voo$winddirdegrees_arr>=34 & voo$winddirdegrees_arr<=56,"NE",
ifelse(voo$winddirdegrees_arr>=57 & voo$winddirdegrees_arr<=78,"ENE",
ifelse(voo$winddirdegrees_arr>=79 & voo$winddirdegrees_arr<=101,"E",
ifelse(voo$winddirdegrees_arr>=102 & voo$winddirdegrees_arr<=123,"ESE",
ifelse(voo$winddirdegrees_arr>=124 & voo$winddirdegrees_arr<=146,"SE",
ifelse(voo$winddirdegrees_arr>=147 & voo$winddirdegrees_arr<=168,"SSE",
ifelse(voo$winddirdegrees_arr>=169 & voo$winddirdegrees_arr<=191,"S",
ifelse(voo$winddirdegrees_arr>=192 & voo$winddirdegrees_arr<=213,"SSO",
ifelse(voo$winddirdegrees_arr>=214 & voo$winddirdegrees_arr<=236,"SO",
ifelse(voo$winddirdegrees_arr>=237 & voo$winddirdegrees_arr<=258,"OSO",
ifelse(voo$winddirdegrees_arr>=259 & voo$winddirdegrees_arr<=281,"O",
ifelse(voo$winddirdegrees_arr>=282 & voo$winddirdegrees_arr<=303,"ONO",
ifelse(voo$winddirdegrees_arr>=304 & voo$winddirdegrees_arr<=326,"NO",
ifelse(voo$winddirdegrees_arr>=327 & voo$winddirdegrees_arr<=348,"NNO",
"NA")))))))))))))))))

