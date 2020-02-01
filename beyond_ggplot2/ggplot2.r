#### BOXPLOT ####
dfPr <- read.csv(file.choose()) #metodo1: Para abrir archivo csv desde el disco duro
load(file.choose())             #metodo2: Para abrir archivo Rdata desde disco duro
load('C:/8_COURSES/R_Hidrologia/Erick Claros/CURSO R GGPLO/beyond_ggplot2/dfPr.Rdata') #metodo3: Para abrir archivo Rdata desde ruta indicada con subdirectorios "/", R no lee rutas con "\"
class(dfPr$dates)

p <- ggplot(dfPr,aes(dates,V8)) + geom_boxplot()

dfPr$mes <- format(dfPr$dates,"%b")
  
p <- ggplot(dfPr,aes(reorder(mes,dates),V8)) + geom_boxplot() +
  scale_y_continuous(trans='log2') + xlab('Precipitación') +
  ylab('Meses')

p + ggtitle('Estación V8')

  
#### HISTOGRAM ####
p <- ggplot(dfPr,aes(V8)) + 
  geom_histogram(col='blue',fill = 'white',binwidth = 5) +
  theme_bw()
p #Visualizar
#### SMOOTH DENSITY PLOT ####
p <- ggplot(dfPr,aes(V8,fill=mes)) + 
  geom_density(alpha=0.25 ,bw=2) + theme_bw() 
p #Visualizar

####  SCATTERPLOT ####
p <- ggplot(dfPr, aes(V8,V6)) +geom_point()
p #Visualizar

#opcion 1
## tranformacion a mensual ##
dfPr.xts <- xts(dfPr[,c(-1,-10)],order.by = dfPr$dates)
#### tranformar a mensual 
#usar apply.monthly
dfPrm.xts <- apply.monthly(dfPr.xts,FUN=apply,2,sum)
dfPrm <- as.data.frame(dfPrm.xts)

#opcion 2
dfPrm <- SeriesAggreg(dfPr[,-10],'daily','monthly',rep('sum',8))

p <- ggplot(dfPrm,aes(V7,V8)) + geom_point()

a <- lm(dfPrm$V8~dfPrm$V7)

p <- p + geom_abline(intercept = 36.215,slope = 1.466, col= 'red')

dfPrm$mes <- format(dfPrm$dates,'%b')

p <- ggplot(dfPrm,aes(V7,V8)) + 
  geom_point(aes(col=reorder(mes,dates))) +
  geom_text(aes(label=mes),nudge_x = 5)

#buscar   geom_text_reppel()  : para separar etiquetas