library(shiny)
library(readxl)
library(data.table)
library(ggplot2)
library(lubridate)
library(shinydashboard)

trm = read_excel("1.1.1.TCM_Serie historica IQY.xlsx")
trm = setDT(trm)
names(trm)[1] = "Fecha"
names(trm)[2] = "TRM"
trm_1 = trm[,Fecha := as.Date(Fecha, format = c("%Y-%m-%d"))]
trm_1 = trm_1[Fecha >= "2008-01-01" & Fecha <= "2020-10-16",]

trm_1[, Ano := year(trm_1$Fecha)]
trm_1[, Mes := month(trm_1$Fecha)]
trm_1[, Ano_Mes := paste(Ano, Mes, 1, sep = "-")]
trm_1 = trm_1[,Ano_Mes := as.Date(Ano_Mes, format = c("%Y-%m-%d"))]
trm_1 = trm_1[order(Fecha)]

petroleo = read.csv("Datos historicos Futuros petroleo Brent.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
colnames(petroleo)
names(petroleo)[1] = "Fecha"
names(petroleo)[2] = "Cierre"
petroleo_1 = data.table(petroleo[1:2], stringsAsFactors = FALSE)
petroleo_1 = petroleo_1[,Fecha := as.Date(Fecha, format = c("%d.%m.%Y"))]
petroleo_1 = petroleo_1[,Cierre := as.numeric(gsub(",", ".", gsub("\\.", "", Cierre)))]
petroleo_1 = petroleo_1[Fecha >= "2008-01-01" & Fecha <= "2020-10-16",] 

petroleo_1[, Ano := year(petroleo_1$Fecha)]
petroleo_1[, Mes := month(petroleo_1$Fecha)]
petroleo_1[, Ano_Mes := paste(Ano, Mes, 1, sep = "-")]
petroleo_1 = petroleo_1[,Ano_Mes := as.Date(Ano_Mes, format = c("%Y-%m-%d"))]
petroleo_1 = petroleo_1[order(Fecha)]

ggplot(data=trm_1, aes(x=trm_1$Fecha, 
                       y=trm_1$TRM)) +
  geom_line(aes(color = "TRM")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Diaria (2008-2020)")

###' TRM - Mensual

trm_mensual = setDT(trm_1[, .(TRM = last(TRM)), by = Ano_Mes])
setnames(trm_mensual, "Ano_Mes", "Fecha")

ggplot(data=trm_mensual, 
       aes(x=trm_mensual$Fecha, 
           y=trm_mensual$TRM)) +
  geom_line(aes(color = "TRM")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Mensual (2008-2020)")

###' TRM - Anual

trm_anual = trm_1[, .(TRM = last(TRM)), by = Ano]
setnames(trm_anual, "Ano", "Fecha")

ggplot(data=trm_anual, 
       aes(x=trm_anual$Fecha, 
           y=trm_anual$TRM)) +
  geom_line(aes(color = "TRM")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Anual (2008-2020)")

###' PETROLEO - Diario

ggplot(data=petroleo_1, aes(x=petroleo_1$Fecha, 
                            y=petroleo_1$Cierre)) +
  geom_line(aes(color = "Brent")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Diario (2008-2020)")

###' PETROLEO - Mensual

pet_mensual = petroleo_1[, .(Cierre = last(Cierre)), by = Ano_Mes]
setnames(pet_mensual, "Ano_Mes", "Fecha")

ggplot(data=pet_mensual, 
       aes(x=pet_mensual$Fecha, 
           y=pet_mensual$Cierre)) +
  geom_line(aes(color = "Brent")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Mensual (2008-2020)")

###' PETROLEO - Anual

pet_anual = petroleo_1[, .(Cierre = last(Cierre)), by = Ano]
setnames(pet_anual, "Ano", "Fecha")

ggplot(data=pet_anual, 
       aes(x=pet_anual$Fecha, 
           y=pet_anual$Cierre)) +
  geom_line(aes(color = "Brent")) +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Anual (2008-2020)")

###' TRM - Mensual

ggplot(data=trm_mensual, 
       aes(x=trm_mensual[,Mes := lubridate::month(trm_mensual$Fecha, label = TRUE)]$Mes, 
           y=trm_mensual$TRM,
           group= trm_mensual[,Mes := month(trm_mensual$Fecha)]$Mes, label = TRUE)) +
  geom_boxplot(aes(color = trm_mensual[,Mes := month(trm_mensual$Fecha)]$Mes, label = TRUE)) +
  theme(legend.position="none") + theme(legend.title = element_blank()) +
  labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Mensual")

###' Petroleo - Mensual

ggplot(data=pet_mensual, 
       aes(x=pet_mensual[,Mes := lubridate::month(pet_mensual$Fecha, label = TRUE)]$Mes, 
           y=pet_mensual$Cierre,
           group= pet_mensual[,Mes := month(pet_mensual$Fecha)]$Mes, label = TRUE)) +
  geom_boxplot(aes(color = pet_mensual[,Mes := month(pet_mensual$Fecha)]$Mes, label = TRUE)) +
  theme(legend.position="none") + theme(legend.title = element_blank()) +
  labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Mensual")

trm_pet = setDT(merge(trm_1, petroleo_1, by = 'Fecha'))
trm_pet = trm_pet[Fecha >= "2010-01-01"]

cor.test(trm_pet$TRM, trm_pet$Cierre, method = "pearson")

ggplot(data=trm_pet, 
       aes(x = trm_pet$TRM, y = trm_pet$Cierre)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(y = "Petroleo", x = "TRM") + labs(title = "Grafico de Dispersion")

trm_pet = setDT(trm_pet)

shapiro.test(trm_pet[, ln_TRM := log(trm_pet$TRM)]$ln_TRM )
shapiro.test(trm_pet$Cierre) 

min_TRM = trm_1[, lapply(.SD,min),, by = Ano][,.(Ano, TRM)]
setnames(min_TRM, "TRM", "min_TRM")

max_TRM = trm_1[, lapply(.SD,max),, by = Ano][,.(Ano, TRM)]
setnames(max_TRM, "TRM", "max_TRM")

mean_TRM = trm_1[, lapply(.SD,mean), by = Ano][,.(Ano, TRM)]
setnames(mean_TRM, "TRM", "mean_TRM")

min_max_trm = merge(min_TRM, max_TRM)

resumen_TRM = merge(min_max_trm, mean_TRM)
resumen_TRM

AAPL = read.csv("Datos historicos AAPL.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
AMZN = read.csv("Datos historicos AMZN.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
FB = read.csv("Datos historicos FB.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
GOOG = read.csv("Datos historicos GOOG.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
MSFT = read.csv("Datos historicos MSFT.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

###' Se crea funcion con el mismo preprocesamiento utilizado para cargar la informacion del petroleo en la Parte 1.

load_info = function(accion){
  df = data.table(accion[1:2], stringsAsFactors = FALSE)
  names(df)[1] = "Fecha"
  names(df)[2] = "Cierre"
  df = df[,Fecha := as.Date(Fecha, format = c("%d.%m.%Y"))]
  df = df[,Cierre := as.numeric(gsub(",", ".", gsub("\\.", "", Cierre)))]
  df = df[Fecha >= "2008-01-01" & Fecha <= "2020-10-16",] 
  
  df[, Ano := year(df$Fecha)]
  df[, Mes := month(df$Fecha)]
  df[, Ano_Mes := paste(Ano, Mes, 1, sep = "-")]
  df = df[,Ano_Mes := as.Date(Ano_Mes, format = c("%Y-%m-%d"))]
}

AAPL = load_info(AAPL)
AMZN = load_info(AMZN)
FB = load_info(FB)
GOOG = load_info(GOOG)
MSFT = load_info(MSFT)

###' Se crea funcion para crear los graficos historicos de precios

line_precios = function(accion){
  ggplot(data=accion, aes(x=accion$Fecha, 
                          y=accion$Cierre)) +
    geom_line(aes(color = "accion")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor Diario de la Accion", x = "Fecha") + labs(title = "Diario (2008-2020)")
}

line_precios(AAPL)
line_precios(AMZN)
line_precios(FB)
line_precios(GOOG)
line_precios(MSFT)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Analisis de Precios"),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: 
      tabsetPanel(type = "tabs",
                  
                  tabPanel("TRM vs. Petroleo", 
                           
                           tags$div(class="header", checked=NA,
                                    tags$h3("Historicos de Precios")
                           ),
                           
                           column(6, plotOutput("TRMD"),
                                  plotOutput("TRMM"),
                                  plotOutput("TRMA")),
                           column(6, plotOutput("PETD"),
                                  plotOutput("PETM"),
                                  plotOutput("PETA")),
                           
                           tags$div(class="header", checked=NA,
                                    tags$h3("Analisis de Correlacion")
                           ),
                      
                           tags$div(class="header", checked=NA,
                                    tags$h4("Grafico de Dispersion")
                           ),
                           
                           plotOutput("Corr"),
                           
                           valueBoxOutput("cor_test")
                           
                           ),
                  
                  tabPanel("Summary", 
                           
                           tags$div(class="header", checked=NA,
                                    tags$h4("Tabla Resumen TRM")
                           ),
                           
                           tableOutput("summary")
                           
                           ),
                  
                  tabPanel("S&P500", 
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             
                             radioButtons("Accion", "Accion a Analizar:",
                                          c("Apple" = "AAPL",
                                            "Amazon" = "AMZN",
                                            "Facebook" = "FB",
                                            "Google" = "GOOG",
                                            "Microsoft" = "MSFT"))
                             
                           ),
                             
                           plotOutput("acciones")
                      
                           )
                  
      )
      
    )
 # )
)

# Define server logic
server <- function(input, output) {
  
  output$TRMD <- renderPlot({
    ggplot(data=trm_1, aes(x=trm_1$Fecha, 
                           y=trm_1$TRM)) +
    geom_line(aes(color = "TRM")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Diaria (2008-2020)")
  })
  
  output$TRMM <- renderPlot({
    ggplot(data=trm_mensual, 
           aes(x=trm_mensual$Fecha, 
               y=trm_mensual$TRM)) +
    geom_line(aes(color = "TRM")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Mensual (2008-2020)")
  })
  
  output$TRMA <- renderPlot({
    ggplot(data=trm_anual, 
           aes(x=trm_anual$Fecha, 
               y=trm_anual$TRM)) +
    geom_line(aes(color = "TRM")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor TRM", x = "Fecha") + labs(title = "TRM Anual (2008-2020)")
  })
  
  output$PETD <- renderPlot({
    ggplot(data=petroleo_1, aes(x=petroleo_1$Fecha, 
                                y=petroleo_1$Cierre)) +
    geom_line(aes(color = "Brent")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Diario (2008-2020)")
  })
  
  output$PETM <- renderPlot({
    ggplot(data=pet_mensual, 
           aes(x=pet_mensual$Fecha, 
               y=pet_mensual$Cierre)) +
    geom_line(aes(color = "Brent")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Mensual (2008-2020)")
  })
  
  output$PETA <- renderPlot({
    ggplot(data=pet_anual, 
           aes(x=pet_anual$Fecha, 
               y=pet_anual$Cierre)) +
    geom_line(aes(color = "Brent")) +
    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
    labs(y = "Valor Petroleo", x = "Fecha") + labs(title = "Petroleo Anual (2008-2020)")
  })
  
  output$cor_test <- renderValueBox({
    valueBox(cor(trm_pet$TRM, trm_pet$Cierre, method = "spearman"), 
             "Coeficiente de Correlacion de Spearman")
  })
  
  output$Corr <- renderPlot({
    ggplot(data=trm_pet, 
           aes(x = trm_pet$TRM, y = trm_pet$Cierre)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE) +
    labs(y = "Petroleo", x = "TRM") + labs(title = "Grafico de Dispersion")
  })
  
  # Generate a summary of the data
  output$summary <- renderTable({
    resumen_TRM
  })
  
  # Generate an Sidebar Panel information
  output$acciones <- renderPlot({
    if (input$Accion == "AAPL") {
      line_precios(AAPL)
      }else if (input$Accion == "AMZN") {
        line_precios(AMZN)  
      }else if (input$Accion == "FB") {
        line_precios(FB)  
      }else if (input$Accion == "GOOG") {
        line_precios(GOOG)  
      }else if (input$Accion == "MSFT") {
        line_precios(MSFT)  
      }
  })

}  

# Create Shiny app ----
shinyApp(ui, server)
