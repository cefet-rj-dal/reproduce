#Classificando maternidades por característica da TMN
library(ggplot2)
library(dplyr)
library(reshape2)
library(stats)

load("~/LH/rmRJ.RData")

dado <- rmrj

df <- dado %>%  group_by(local) %>% mutate(count = n(), na.rm=TRUE)
df <- unique(subset(df, select = c(local, count)))
zr <- dado %>%  group_by(local) %>% filter(tmn==0) %>% mutate(zeros = n(), na.rm=TRUE)
zr <- unique(subset(zr, select = c(local, zeros)))

df <- merge(x = df, y = zr, by = "local", all.x=TRUE)
df$zeros[is.na(df$zeros)] <- 0

df$perc <- df$zeros/df$count
  
cat <- kmeans(df$perc, 3)
cat$cluster
df$cat <- cat$cluster
table(df$cat)

#Trocando para ficar a categoria 1 com mais zeros e 3 com menos
cnes_cat <- df
cnes_cat$cat <- gsub("2", "4", cnes_cat$cat)
cnes_cat$cat <- gsub("3", "2", cnes_cat$cat)
cnes_cat$cat <- gsub("4", "3", cnes_cat$cat)
table(cnes_cat$cat)

#save(cnes_cat, file="LH/cnes_cat.RData")

df <- merge(x = dado, y = cnes_cat, by = "local", all.x=TRUE)
#save(df, file="LH/df.RData")


#Gráfico com 1 exemplo de cada categoria

c3 <- subset(dado, local=="2290227", select=c(time, tmn))
names(c3) <- c("time", "HOSPITAL ESTADUAL ADAO PEREIRA NUNES (2290227)")
c2 <- subset(dado, local=="2268922", select=c(time, tmn))
names(c2) <- c("time", "HOSPITAL MUNICIPAL DESEMBARGADOR LEAL JUNIOR (2268922)")
c1 <- subset(dado, local=="5042488", select=c(time, tmn))
names(c1) <- c("time", "MATERNIDADE MUNICIPAL DRA ALZIRA REIS VIEIRA FERREIRA (5042488)")

df <- merge(merge(
  c1,
  c2, by = "time", all=TRUE),
  c3, by = "time", all=TRUE)
df_melt <- melt(df, id=c("time"))

d <- ggplot(df_melt, aes(x = time, y = value, group=variable)) + 
  geom_line() +
  #    ylim(0, 30) +
  scale_x_date(date_breaks = "year", date_labels ="%Y") +
  facet_wrap(~ variable, scales = 'free_y', ncol = 1) + 
  theme(legend.position = "none",
        strip.background = element_rect(colour="black", fill="#8C9EFF"), axis.text=element_text(size=12)) +
  xlab("") + ylab("NMR")
d

##########

load("~/LH/df.RData")
dado <- df

lista_1 <- subset(df, cat==1)
lista_1 <-unique(lista_1$local)

lista_2 <- subset(df, cat==2)
lista_2 <- unique(lista_2$local)

lista_3 <- subset(df, cat==3)
lista_3 <- unique(lista_3$local)

#Visualizando todas as séries para cada categoria
for (i in lista_1){
  sb <- subset(dado, local==i)
  
  a <- ggplot(sb, aes(x = time, y = tmn)) + 
    geom_line() + 
    geom_vline(xintercept=sb$time[sb$anom.p==TRUE], linetype="dashed", 
               color = "blue", size=.7) + 
    geom_vline(xintercept=sb$time[sb$anom.t==TRUE], linetype="dashed", 
               color = "red", size=.7) + 
    geom_vline(xintercept=sb$time[sb$anom.c==TRUE], linetype="dashed", 
               color = "green", size=.7) + 
    ggtitle(sb$local)
  print(a)
}


