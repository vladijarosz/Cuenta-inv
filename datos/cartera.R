# Instalar y cargar el paquete
install.packages("quantmod")
library(quantmod)
library(openxlsx)
library(dplyr)
library(highcharter)
library(dygraphs)



setwd("C:/Users/Admin/Documents/vladimir/finanzas/2024")
tenencia <- read.xlsx("tenencias.xlsx")
mep_tot <- read.csv("MEP.csv")
tenencia$Ticker <- paste0(tenencia$Ticker, ".BA")

#
mep_T <- mep_tot %>%
  filter(fecha >= "2024-01-01" & fecha <= "2025-01-01")
mep <- mep_T[c("ultimo","fecha")]
mep$fecha <- as.Date(mep$fecha)
mep <- as.xts(mep)
colnames(mep)[colnames(mep)=="ultimo"] <- "mep"

C_mep <- data.frame(mep_T$fecha, mep_T$ultimo)
colnames(C_mep)[colnames(C_mep)=="mep_T.fecha"] <- "fecha"


#
tickers <- c(
  "ALUA.BA",  # Aluar Aluminio Argentino S.A.
  "BBAR.BA",  # BBVA Banco Francés S.A.
  "BMA.BA",   # Banco Macro S.A.
  "BYMA.BA",  # Bolsas y Mercados Argentinos S.A.
  "CEPU.BA",  # Central Puerto S.A.
  "COME.BA",  # Sociedad Comercial del Plata S.A.
  "CRES.BA",  # Cresud S.A.C.I.F. y A.
  "CVH.BA",   # Cablevisión Holding S.A.
  "EDN.BA",   # Empresa Distribuidora y Comercializadora Norte S.A. (Edenor)
  "GGAL.BA",  # Grupo Financiero Galicia S.A.
  "HARG.BA",  # Holcim Argentina S.A.
  "LOMA.BA",  # Loma Negra Compañía Industrial Argentina S.A.
  "MIRG.BA",  # Mirgor S.A.
  "PAMP.BA",  # Pampa Energía S.A.
  "SUPV.BA",  # Grupo Supervielle S.A.
  "TECO2.BA", # Telecom Argentina S.A.
  "TGNO4.BA", # Transportadora de Gas del Norte S.A.
  "TGSU2.BA", # Transportadora de Gas del Sur S.A.
  "TRAN.BA",  # Compañía de Transporte de Energía Eléctrica en Alta Tensión Transener S.A.
  "TXAR.BA",  # Ternium Argentina S.A.
  "VALO.BA",  # Banco de Valores S.A.
  "YPFD.BA",  # YPF S.A.
  "AGRO.BA",  # Agrometal S.A.
  "AUSO.BA",  # Autopistas del Sol S.A.
  "BHIP.BA",  # Banco Hipotecario S.A.
  "BOLT.BA",  # Boldt S.A.
  "BPAT.BA",  # Banco Patagonia S.A.
  "CADO.BA",  # Carlos Casado S.A.
  "CAPX.BA",  # Capex S.A.
  "CARC.BA",  # Carboclor S.A.
  "CECO2.BA", # Endesa Costanera S.A.
  "CELU.BA",  # Celulosa Argentina S.A.
  "CEPU.BA",  # Central Puerto S.A.
  "CGPA2.BA", # Camuzzi Gas Pampeana S.A.
  "CTIO.BA",  # Consultatio S.A.
  "DGCU2.BA", # Distribuidora de Gas Cuyana S.A.
  "DOME.BA",  # Domec S.A.
  "FERR.BA",  # Ferrum S.A.
  "FIPL.BA",  # Fiplasto S.A.
  "GAMI.BA",  # B-Gaming S.A.
  "GBAN.BA",  # Gas Natural BAN S.A.
  "GCDI.BA",  # GCDI S.A.
  "GCLA.BA",  # Grupo Clarín S.A.
  "GRIM.BA",  # Grimoldi S.A.
  "HAVA.BA",  # Havanna Holding S.A.
  "IEB.BA",   # Dycasa S.A.
  "INTR.BA",  # Compañía Introductora de Buenos Aires S.A.
  "INVJ.BA",  # Inversora Juramento S.A.
  "IRSA.BA",  # IRSA Inversiones y Representaciones S.A.
  "LEDE.BA",  # Ledesma S.A.A.I.
  "LONG.BA",  # Longvie S.A.
  "METR.BA",  # Metrogas S.A.
  "MOLA.BA",  # Molinos Agro S.A.
  "MOLI.BA",  # Molinos Río de la Plata S.A.
  "MORI.BA",  # Morixe Hermanos S.A.
  "MTR.BA",   # Matba Rofex S.A.
  "OEST.BA",  # Grupo Concesionario del Oeste S.A.
  "PATA.BA",  # Importadora y Exportadora de la Patagonia S.A.
  "POLL.BA",  # Polledo S.A.
  "RICH.BA",  # Laboratorios Richmond S.A.C.I.F.
  "RIGO.BA",  # Rigolleau S.A.
  "SAMI.BA",  # San Miguel S.A.
  "SEMI.BA"   # Molinos Juan Semino S.A. 
  )

getSymbols(tenencia$Ticker, src = "yahoo", from = "2024-01-02", to = "2025-01-14")



# Descargar datos para cada ticker
for (ticker in tickers) {
  getSymbols(ticker, src = "yahoo", from = "2024-01-02", to = "2025-01-14", auto.assign = TRUE)
}


#
AL29.BA <- read.csv("AL29.csv")
AL30.BA <- read.csv("AL30.csv")
META.BA <- read.csv("META.csv")
VIST.BA <- read.csv("VIST.csv")
GD35.BA <- read.csv("GD35.csv")
GD30.BA <- read.csv("GD30.csv")

AL29.BA$cierre <- AL29.BA$cierre/100
AL30.BA$cierre <- AL30.BA$cierre/100
GD30.BA$cierre <- GD30.BA$cierre/100
GD35.BA$cierre <- GD35.BA$cierre/100

colnames(GD30.BA)[colnames(GD30.BA)== "cierre"] <- "GD30.BA.Close"
colnames(GD35.BA)[colnames(GD35.BA)== "cierre"] <- "GD35.BA.Close"
colnames(VIST.BA)[colnames(VIST.BA)== "cierre"] <- "VIST.BA.Close"
colnames(AL29.BA)[colnames(AL29.BA)== "cierre"] <- "AL29.BA.Close"
colnames(AL30.BA)[colnames(AL30.BA)== "cierre"] <- "AL30.BA.Close"
colnames(META.BA)[colnames(META.BA)== "cierre"] <- "META.BA.Close"

AL29.BA$fecha <- as.Date(AL29.BA$fecha)
AL29.BA <- as.xts(AL29.BA)
AL30.BA$fecha <- as.Date(AL30.BA$fecha)
AL30.BA <- as.xts(AL30.BA)
META.BA$fecha <- as.Date(META.BA$fecha)
META.BA <- as.xts(META.BA)
VIST.BA$fecha <- as.Date(VIST.BA$fecha)
VIST.BA <- as.xts(VIST.BA)
GD30.BA$fecha <- as.Date(GD30.BA$fecha)
GD30.BA <- as.xts(GD30.BA)
GD35.BA$fecha <- as.Date(GD35.BA$fecha)
GD35.BA <- as.xts(GD35.BA)


#
tenencia$fecha <- as.Date(tenencia$fecha, origin = "1899-12-30")

# Obtener datos de una acción argentina (por ejemplo, YPF)
getSymbols("^MERV", src = "yahoo", from = "2024-01-02", to = "2025-01-14")
getSymbols("^GSPC", src = "yahoo", from = "2024-01-02", to = "2025-01-14")
SYP <- GSPC
#
cotizaciones <- merge(
  AAPL.BA = Cl(AAPL.BA),
  AL30.BA = Cl(AL30.BA),
  AL29.BA = Cl(AL29.BA),
  ALUA.BA = Cl(ALUA.BA),
  BBAR.BA = Cl(BBAR.BA),
  BHIP.BA = Cl(BHIP.BA),
  GGAL.BA = Cl(GGAL.BA),
  HARG.BA = Cl(HARG.BA),
  MELI.BA = Cl(MELI.BA),
  MOLA.BA = Cl(MOLA.BA),
  MOLI.BA = Cl(MOLI.BA),
  NU.BA = Cl(NU.BA),
  PAMP.BA = Cl(PAMP.BA),
  SPY.BA = Cl(SPY.BA),
  TGNO4.BA = Cl(TGNO4.BA),
  TXAR.BA = Cl(TXAR.BA),
  YPFD.BA = Cl(YPFD.BA),
  EDN.BA = Cl(EDN.BA),
  GD35.BA = Cl(GD35.BA),
  GD30.BA = Cl(GD30.BA),
  EWZ.BA = Cl(EWZ.BA),
  META.BA = Cl(META.BA),
  VIST.BA = Cl(VIST.BA),
  COCOAUSD.BA = (mep),
  SYP = Cl(SYP),
  MERV= Cl(MERV),
  fill = NA
)

acciones_tenemcia <- unique(tenencia$Ticker) 
acciones_tenemcia
tickers
acciones <- union(acciones_tenemcia,tickers)
acciones <- unique(acciones)
acciones

cierres_list <- list()

# Extraer las cotizaciones de cierre
for (ticker in acciones) {
  if (exists(ticker)) {  # Verifica que el objeto existe en el entorno
    datos <- get(ticker)  # Obtener el objeto xts
    cierres_list[[ticker]] <- Cl(datos)  # Extraer la columna de cierre
  }
}

# Combinar todas las columnas de cierre en un único objeto xts
cierres_xts <- do.call(merge, cierres_list)


cierres_xts <- merge.xts(cierres_xts, mep ,join = "inner")

rm(cotizacion_usd)
nrow(cierres_xts)
nrow(mep_alineado)
nrow(cotizacion_usd)
cotizacion_usd <- cierres_xts

cotizacion_usd$mep <- NULL
mep_alineado <- cierres_xts$mep

cotizacion_usd <- cotizacion_usd / as.numeric(mep_alineado)

cotizacion_usd$SYP.BA <- cotizacion_usd$GSPC.Close

cierres_xts$SYP.BA <- (GSPC$GSPC.Close * mep_alineado)/200


# Renombrar columnas eliminando el sufijo ".Close"
colnames(cierres_xts) <- sub("\\.Close$", "", colnames(cierres_xts))
cotizaciones <- cierres_xts


# Ejemplo: Encontrar la cotización de AAPL.BA el 2024-04-30
# cotizacion_especifica <- cotizaciones["2024-04-30", "AAPL.BA.Close"]


# Asignar las cotizaciones a tenencia
tenencia$cotizacion <- sapply(1:nrow(tenencia), function(i) {
  # Extraer fecha y ticker para cada fila
  fecha <- tenencia$fecha[i]
  ticker <- tenencia$Ticker[i]
  
  # Intentar extraer la cotización, devolver NA si no existe
  tryCatch({
    cotizaciones[as.character(fecha), ticker]
  }, error = function(e) NA)
})

tickers_mep <- c("COCOAUSD.BA", "EXT.BA", "YMCQO.BA")

# Asignar las cotizaciones a tenencia
tenencia$cotizacion <- sapply(1:nrow(tenencia), function(i) {
  # Extraer fecha y ticker para cada fila
  fecha <- tenencia$fecha[i]
  ticker <- tenencia$Ticker[i]
  
  # Determinar qué cotización usar
  tryCatch({
    if (ticker %in% tickers_mep) {
      # Usar cotización del "mep" para estas acciones
      cotizaciones[as.character(fecha), "mep"]
    } else {
      # Usar cotización normal para el resto
      cotizaciones[as.character(fecha), ticker]
    }
  }, error = function(e) NA)  # Devuelve NA si hay algún error
})

tenencia$cotizacion[is.na(tenencia$cotizacion)] <- 1


tenencia$valor <- tenencia$Tenencia*tenencia$cotizacion

tenencia$Codigo <- NULL

#tenencia a meses
tenen_04 <- tenencia %>%
  filter(fecha >= "2024-04-01" & fecha <= "2024-05-01")
tenen_05 <- tenencia %>%
  filter(fecha >= "2024-05-01" & fecha <= "2024-06-01")
tenen_06 <- tenencia %>%
  filter(fecha >= "2024-06-01" & fecha <= "2024-07-01")
tenen_07 <- tenencia %>%
  filter(fecha >= "2024-07-01" & fecha <= "2024-08-01")
tenen_08 <- tenencia %>%
  filter(fecha >= "2024-08-01" & fecha <= "2024-09-01")
tenen_09 <- tenencia %>%
  filter(fecha >= "2024-09-01" & fecha <= "2024-10-01")
tenen_11 <- tenencia %>%
  filter(fecha >= "2024-11-01" & fecha <= "2024-12-01")
tenen_12 <- tenencia %>%
  filter(fecha>="2024-12-10" & fecha <="2025-01-01")
tenen_10 <- tenencia %>%
  filter(fecha>="2024-10-10" & fecha <="2024-11-01")


#####
S_Pesos <- c(sum(tenen_04$valor),sum(tenen_05$valor),
             sum(tenen_06$valor),sum(tenen_07$valor),
             sum(tenen_08$valor),sum(tenen_09$valor),
             sum(tenen_10$valor),sum(tenen_11$valor),
             sum(tenen_12$valor))
S_pesos <- as.data.frame(S_Pesos)

colnames(S_pesos)[colnames(S_pesos)=="S_Pesos"] <- "valor_peso"


S_pesos$fecha <- unique(tenencia$fecha)

S_pesos <- S_pesos[, c("fecha","valor_peso")]
S_pesos <- merge(S_pesos, C_mep, by = "fecha", all.x = TRUE)

colnames(S_pesos)[colnames(S_pesos)=="mep_T.ultimo"] <- "mep"

#
cartera <- S_pesos
cartera$valor_usd <- cartera$valor_peso/cartera$mep
cartera$var <- c(100, diff(cartera$valor_usd)/head(cartera$valor_usd,-1))
cartera$indice <- cumsum(cartera$var)
cartera$var[1] <- NA

cotiz <- cartera
cotiz$fecha <- as.Date(cotiz$fecha)
cotiz <- as.data.frame(cotiz)
cotizacion <- as.data.frame(cotizacion)
cotiz <- cotiz[,-2]
cotiz <- cotiz[,-2]
cotiz <- cotiz[,-3]
cotiz <- cotiz[,-3]

cotiz <- merge(cotiz,cotizacion_usd,by="fecha", all.x = TRUE)
cotiz$mep <- NULL
var <- as.data.frame(
  lapply(cotiz, function(x) {
    if (is.numeric(x)) {
      c(NA, diff(x) / head(x, -1)*100)  # Calcular las variaciones porcentuales
    } else {
      x  # Dejar columnas no numéricas sin cambios
    }
  })
)
var[1,] <- 100
acum <- as.data.frame(lapply(var,function(x){
  if(is.numeric(x)){cumsum(x)}
  else{x}
}))
acum[1,1] <- "2024-04-30"
acum$fecha <- as.Date(acum$varAcum.fecha)
acum$varAcum.fecha <- NULL
# Asegúrate de que "fecha" sea la primera columna
acum <- acum[, c("fecha", setdiff(names(acum), "fecha"))]


colnames(var) <- paste0("var.",colnames(var))
colnames(acum) <- paste0("varAcum.",colnames(acum))
cotiza <- cbind(cotiz,acum)


#

plot( acum$varAcum.AL30.BA.Close, , type = "l" )
lines(acum$varAcum.AAPL.BA.Close)


# Asegúrate de que el objeto acumulados sea de tipo xts
acum$fecha <- as.Date(acum$fecha)
acumulados <- as.xts(acum)  # Esto asume que acum ya tiene una columna de tipo Date como índice
index(acumulados) 

# Generar el gráfico interactivo
library(dygraphs)

# Graficar varAcum.valor_usd con dygraphs

dygraph(per_Merv.xts, main = "kkk") %>%
  dyAxis("y",
         label = "Variaciones Acumuladas (%)",
         valueFormatter = "function(v) { return v.toFixed(2) + '%'; }",  # Formato en porcentaje
         axisLabelFormatter = "function(v) { return v.toFixed(0) + '%'; }"  # Etiqueta en ejes
  ) %>%
  dyAxis("x", label = "Fecha") %>%
  dyOptions(
    drawGrid = TRUE,        # Mostrar cuadrículas
    colors = "blue"         # Color de la serie
  ) %>%
  dyRangeSelector()  # Selector de rango interactivo


## ver ganadores y perdedores vs MERval

# Determinar el valor límite
Mer_24 <- tail(acum$MERV.Close,1)

# Filtrar columnas donde la última fila es mayor al valor límite
gan_Merv <- acum[, sapply(acum, function(col) tail(col, 1) >= Mer_24)]
gan_Merv$valor_usd <- acum$valor_usd

gan_Merv.xts <- as.xts(gan_Merv)

# perdedoras merval

per_Merv <- acum[,sapply(acum, function(col)tail(col,1)<= Mer_24)]
per_Merv$fecha <- acum$fecha
per_Merv.xts <- as.xts(per_Merv)






