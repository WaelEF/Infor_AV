library(shiny)
library(devtools)
library(dashboardthemes)
library(shinythemes)
library(shinydashboard)

library("quantmod")
library("lubridate")
library("e1071")
library(xts)
library(highcharter) 
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

library(DT)
library(ggplot2)
library(readxl)
library(xts)
library(highcharter)
library(shinyBS)
library(tidyquant)

library(tseries)
library(forecast)


library(timetk)

Extraction <- function(db,names)
{
  data = as.data.frame(db)
  erreurs = c()
  data1 = data.frame(unique(data$SEANCE))
  n=nrow(data1)
  colnames(data1) = "SEANCE"
  names = names[!grepl("[[:digit:]]",names)]
  for (titre in names)
  {
    Cours_cloture = as.data.frame(data[data$LIB_VAL==titre,])[,c("SEANCE","CLOTURE")]
    
    data1 = merge(x = data1,y=Cours_cloture, by="SEANCE", all.x = TRUE)
    
    #Repérage et stockage des titres ayant des valeurs manquantes
    if( any(is.na(data1$CLOTURE)) || any(is.null(data1$CLOTURE))  )
    {
      print(paste("Problème :",titre))
      erreurs = c(erreurs, titre)
    }
    names(data1)[ncol(data1)] = titre
  }
  return(list("db" = data1, "erreurs"= erreurs))
}

ST_Plot <- function(db,titre)
{
  hc=NULL
  
  hc <- highchart(type = "stock") %>% 
    hc_title(text = paste("Cours de cloture ")) %>% 
    hc_subtitle(text = "Donnees extraites de Yahoo Finance")
  for(stock in titre)
  {
    data = db[[stock]]
    serie=as.xts(data,order.by = as.Date(rownames(db)))
    hc <- hc %>%  hc_add_series(serie, id = stock,name=stock)
  }

  return(hc)
}


charter2 <- function(name)
{
  start_Date = as.Date("2016-01-01")
  x = getSymbols(name,auto.assign=FALSE,from = start_Date)
  
  return(hchart(x))
}








plot_avance1 <- function(name)
{
  start_Date = as.Date("2016-01-01")
  x = getSymbols(name,auto.assign = FALSE,from = start_Date)
  x=data.frame(x)
  
  AAPL_ts <- log(ts(na.omit(x[[paste0(name,".Close")]]), start =c(2016,01), frequency = 365))
  
  AAPL_decomp_stl <- stl(AAPL_ts, s.window = "periodic", robust = 'TRUE')
  return(ggtsdisplay(AAPL_ts))
  
}
plot_avance2 <- function(name)
{
  start_Date = as.Date("2016-01-01")
  x = getSymbols(name,auto.assign = FALSE,from = start_Date)
  x=data.frame(x)
  
  AAPL_ts <- log(ts(na.omit(x[[paste0(name,".Close")]]), start =c(2016,01), frequency = 365))
  
  AAPL_decomp_stl <- stl(AAPL_ts, s.window = "periodic", robust = 'TRUE')
  return(plot(AAPL_decomp_stl))
  
}












PT_Plot <- function(db,portefeuille)
{
  hc = NULL
  hc <- highchart(type = "stock") %>% 
    hc_title(text = paste("Variation Valeur du Portefeuille")) %>% 
    hc_subtitle(text = "Donnees extraites de Yahoo Finance")
  
  date = db$SEANCE
  
  names = as.character(portefeuille$Titres)
  prices= portefeuille$Valeurs
  DBT = db[names(DB) %in% names]
  N = nrow(DBT)
  Variation <- data.frame(matrix(NA, nrow = N-1))
  Variation[1] = NULL
  Variation$V_Portefeuille = 0;
  S = sum(prices)
  k=1
  for(titre in names(DBT))
  {
    Variation[[paste0("R_",titre)]] = log( DBT[[titre]][2:(N)]) - log(DBT[[titre]][1:(N-1)])
    Proportion = prices[k]/S
    Variation$V_Portefeuille = Variation$V_Portefeuille + Proportion * Variation[[paste0("R_",titre)]]
    k = k +1
  }
  
  data = c(0,Variation$V_Portefeuille)
  serie=as.xts(data,order.by = date)
}

###Création Dataframe composition du portefeuille
Contenu <- function(input)
{
  DB1 <- DB2 <- data.frame(0)
  names(DB1) = "Valeurs"
  names(DB2) = "Titres"
  j=0
  for(i in names(input))
  {
    if(grepl("Value",i))
    {
      j=j+1
      DB2 = rbind(DB2,input[[paste0("input",j)]])
      DB1 = rbind(DB1,input[[i]])
    }
  }
  DB = cbind(DB2,DB1)
  DB = DB[-1,]
  row.names(DB)= NULL
  
  if (NROW(DB) == 0)
    return(NULL)
  
  return(DB)
}








###################################################
####Méthodes de calcul de la VaR Titre#####
###################################################
Monte_var <- function(DB,name,alpha)
{
  dat1=DB[[paste0("R_",name)]]
  N = length(dat1)
  volatilite = sd(dat1)
  esperance = mean(dat1)
  c=c()
  for(i in 1:100000)
  {
    c[i] = exp((esperance - 0.5 * volatilite^ 2) + volatilite * pnorm(runif(1), mean=0,sd= 1)) - 1
  }
  alpha = alpha/100
  VaR = quantile(c,alpha)
  exceptions = sum(VaR < dat1)
  return(list(var = -VaR,exceptions = exceptions))
}

Para_var<- function(DB,name,alpha)
{
  dat1=DB[[paste0("R_",name)]]
  N = length(dat1)
  
  volatilite = sd(dat1)
  esperance = mean(dat1)
  VaR_percent = esperance -qnorm(1-(alpha/100),esperance,volatilite)
  exceptions = sum(dat1 > VaR_percent)
  
  return(var=list(var=-VaR_percent,exceptions =exceptions))
}

Histo_var<- function(DB,name,alpha)
{
  dat1=DB[[paste0("R_",name)]]
  N = length(dat1)
  alpha_pire = round((alpha/100)*(N-1))
  dat1 = sort(dat1)
  VaR_Alpha = dat1[alpha_pire]
  exceptions = sum(dat1 > VaR_Alpha)
  return(list(var = -VaR_Alpha,exceptions = exceptions))

}

###################################################
####Méthodes de calcul de la VaR : Portefeuille#####
###################################################

Histo_portefeuille <- function(DB,contenu,alpha,nombre_jours)
{
  names = contenu[["Titres"]]
  N = nrow(DB)
  Variation <- data.frame(matrix(NA, nrow = N))
  Variation[1] = NULL
  Variation$V_Portefeuille = 0;
  prices = as.numeric(as.character(contenu[["Valeurs"]]))
  S = sum(prices)
  k=1
  for(titre in names)
  {
    Proportion = prices[k]/S
    Variation$V_Portefeuille = Variation$V_Portefeuille + Proportion * DB[[paste0("R_",titre)]]
    k = k +1
  }
  alpha_pire = round((alpha/100)*(N-1))
  Variation$V_Portefeuille = sort(Variation$V_Portefeuille)
  VaR_Alpha =  sqrt(nombre_jours) * S * Variation$V_Portefeuille[alpha_pire]
  exceptions = sum(Variation$V_Portefeuille > Variation$V_Portefeuille[alpha_pire])
  return(list(var = -VaR_Alpha,exceptions = exceptions))
}


Para_portefeuille <- function(DB,contenu,alpha,nombre_jours)
{
  names = contenu[["Titres"]]
  N = nrow(DB)
  Variation <- data.frame(matrix(NA, nrow = N))
  Variation[1] = NULL
  Variation$V_Portefeuille = 0;
  prices = as.numeric(as.character(contenu[["Valeurs"]]))
  S = sum(prices)
  k=1
  for(titre in names)
  {
    Proportion = prices[k]/S
    Variation$V_Portefeuille = Variation$V_Portefeuille + Proportion * DB[[paste0("R_",titre)]]
    k = k +1
  }
  alpha_pire = round((alpha/100)*(N-1))
  variation_portefeuille = Variation$V_Portefeuille
  volatilite = sd(variation_portefeuille)
  esperance = mean(variation_portefeuille)
  VaR_percent = esperance -qnorm(1-(alpha/100),esperance,volatilite)
  exceptions = sum(VaR_percent < variation_portefeuille)
  return(list(var = -VaR_percent*sqrt(nombre_jours)*S ,exceptions =exceptions))
}

Monte_portefeuille <- function(DB,contenu,alpha,nombre_jours)
{
  names = contenu[["Titres"]]
  N = nrow(DB)
  Variation <- data.frame(matrix(NA, nrow = N))
  Variation[1] = NULL
  Variation$V_Portefeuille = 0;
  prices = as.numeric(as.character(contenu[["Valeurs"]]))
  S = sum(prices)
  k=1
  for(titre in names)
  {
    Proportion = prices[k]/S
    Variation$V_Portefeuille = Variation$V_Portefeuille + Proportion * DB[[paste0("R_",titre)]]
    k = k +1
  }
  alpha_pire = round((alpha/100)*(N-1))
  variation_portefeuille = Variation$V_Portefeuille
  volatilite = sd(variation_portefeuille)
  esperance = mean(variation_portefeuille)
  c=c()
  for(i in 1:100000)
  {
    c[i] = exp((esperance - 0.5 * volatilite^ 2) + volatilite * pnorm(runif(1), mean=0,sd= 1)) - 1
  }
  alpha = alpha/100
  VaR = quantile(c,alpha)
  exceptions = sum(VaR < variation_portefeuille)
  return(list(var = -VaR* S * sqrt(nombre_jours),exceptions=exceptions))
}




####Graphique Histogramme Portefeuille

Graphique <- function(DB,contenu,alpha,nombre_jours)
{
names = contenu[["Titres"]]
N = nrow(DB)
Variation <- data.frame(matrix(NA, nrow = N))
Variation[1] = NULL
Variation$V_Portefeuille = 0;
prices = as.numeric(as.character(contenu[["Valeurs"]]))
S = sum(prices)
k=1
for(titre in names)
{
  Proportion = prices[k]/S
  Variation$V_Portefeuille = Variation$V_Portefeuille + Proportion * DB[[paste0("R_",titre)]]
  k = k +1
}
alpha_pire = round((alpha/100)*(N-1))
variation_portefeuille = Variation$V_Portefeuille

dat1=data.frame(x=variation_portefeuille)

ggplot(data=dat1) + geom_histogram(aes(x=x), fill="blue", alpha=.5) +
  xlab("Variations journalieres du Portefeuille") + ylab("Frequence") +
  ggtitle("Histogramme des variations journalieres") + theme_bw()

}
                                                    #####################################
                                                    ####Modifications Theme utilisé######
                                                    #####################################



theme_onenote_modifie <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(95,155,213)"
  
  ,headerButtonBackColor = "rgb(95,155,213)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(55,116,176)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(95,155,213)"
  ,headerBoxShadowColor = "rgb(220,220,220)"
  ,headerBoxShadowSize = "2px 3px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(241,241,241)"
    ,colorMiddle = "rgb(237,237,237)"
    ,colorEnd = "rgb(210,210,210)"
    ,colorStartPos = 0
    ,colorMiddlePos = 97
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = ""
  
  ,sidebarUserTextColor = "rgb(0,0,0)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(133,47,180)"
  ,sidebarSearchBorderColor = "rgb(210,210,210)"
  
  ,sidebarTabTextColor = "rgb(0,0,0)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = ""
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(193,193,193)"
    ,colorMiddle = "rgb(216,216,216)"
    ,colorEnd = "rgb(218,218,218)"
    ,colorStartPos = 0
    ,colorMiddlePos = 5
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(133,47,180)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(230,230,230)"
    ,colorMiddle = "rgb(225,225,225)"
    ,colorEnd = "rgb(210,210,210)"
    ,colorStartPos = 0
    ,colorMiddlePos = 97
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = ""
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(225,225,225)"
  ,boxPrimaryColor = "rgb(95,155,213)"
  ,boxInfoColor = "rgb(235,235,235)"
  ,boxSuccessColor = "rgb(112,173,71)"
  ,boxWarningColor = "rgb(237,125,49)"
  ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(133,47,180)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(210,210,210)"
  ,tabBoxBorderRadius = 0
  
  ### inputs
  ,buttonBackColor = "rgb(240,240,240)"
  ,buttonTextColor = "rgb(80,80,80)"
  ,buttonBorderColor = "rgb(185,185,185)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(227,227,227)"
  ,buttonTextColorHover = "rgb(80,80,80)"
  ,buttonBorderColorHover = "rgb(210,210,210)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(210,210,210)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(210,210,210)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)