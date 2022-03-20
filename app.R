#Primer shiny de El estimado ¡Vamos con todo!
#Instalamos nuestras paqueterías
rm(list = ls())
if (!require(pacman)) install.packages("pacman") 
library(pacman)
p_load("foreign","car","doBy", "reshape", "data.table", "stats", "dplyr","haven",
       "ggplot2","tidyr","extrafont","magick","grid", "ineq","reldist","writexl","reshape2",
       "openxlsx","readxl","shiny","DT","shiny", "tidyverse", "shinydashboard", "lubridate","scales")

setwd("C:/APP_ENIGH/Deciles_ENIGH")
deciles<-read_xlsx("DECILES_final.xlsx",col_names = T)
tema<-theme(plot.title = element_text(hjust=0.6,face = "bold",size=20),
            legend.title = element_text(size=18),
            legend.text=element_text(size=16),
            axis.text=element_text(size=15),
            axis.title=element_text(size=16))
######################################################################
#Shiny APP. El estimado. Primer Proyecto. ¡Venga!

ui<-fluidPage(
    tags$h1("Primer Proyecto de",tags$em("El Estimado")),
    tags$h2("Este es el inicio de todo",tags$strong("¡VENGA!")),
    tabPanel(
        title = "Deciles",
        sidebarPanel(
            h4("Por entidad federativa"),
            selectInput("entidad",label = "Seleccione la entidad federativa:",
                        choices = unique(deciles$nombre_ent),selected = "Nacional")
        ),
        mainPanel(plotOutput("graf_deciles")),
        DT::dataTableOutput("cuadro_deciles")
    ))

server<-function(input,output){
    selections=reactive({
        req(input$entidad)
        filter(deciles,nombre_ent==input$entidad)
    })
    output$graf_deciles<-renderPlot({
        ggplot(data = selections(),aes(decil,ingreso_mensual,fill=sexo_jefe,size=6))+
            geom_bar(stat = "identity",position = position_dodge(),size=1.2)+
            xlab("Decil")+ylab("Ingresos Promedio")+
            labs(title="Ingresos mensuales promedio por deciles",
                 caption = "Fuente: Elaboración de M. Valdés con datos de INEGI,ENIGH (2020)")+
            scale_fill_manual(values = c("Mujer"="#40A93E","Hombre"="#114C7D"),
                              name="Sexo del jefe de hogar")+
            scale_y_continuous(labels=scales::comma,breaks = seq(0,125000,10000),limits = c(0,125000))+
            geom_text(aes(decil,ingreso_mensual,label=paste("$",scales::comma(ingreso_mensual))),
                      angle=90,position = position_dodge(width=0.8),size=5,hjust=-0.1)+
            scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                               labels=c("I","II","III","IV","V","VI","VII","VIII","IX","X"))+
            tema
    })
    output$cuadro_deciles= 
        DT::renderDataTable({
            DT::datatable(selections()[,c("cve_ent", "sexo_jefe","nombre_ent","roma","ing_mensual","porcen_ing",
                                          "tot_hogares","ing_total")],
                          colnames = c("Clave de la Entidad", "Sexo del jefe de hogar", "Nombre de la Entidad", 
                                       "Decil","Ingreso Mensual Promedio","Porcentaje del Y Nal","Total de Hogares",
                                       "Ingresos Totales Corrientes"),
                          options = list(order = list(1, 'des')),
                          rownames = FALSE
            )
        })
}

shinyApp(ui = ui, server = server)