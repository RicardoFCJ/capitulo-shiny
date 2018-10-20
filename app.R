#Capitulo R Shiny

#Carregando os pacotes necessários
library(shiny)
library(lubridate)
library(ggplot2)

#Variáveis globais
vars=reactiveValues(start=F,
                    stTime=0,
                    data=as.data.frame(
                      matrix(ncol=4,
                             nrow=0,
                             dimnames=list(NULL,c("Experimentador","Participante","Comportamento","Ocorrencia")))
                      ,stringsAsFactors = F))

format.timediff <- function(start_time) {
  diff = as.numeric(difftime(Sys.time(), start_time, units="mins"))
  hr <- diff%/%60
  min <- floor(diff - hr * 60);min=ifelse(nchar(min)<2,paste0(0,min),min)
  sec <- round(diff%%1 * 60);sec=ifelse(nchar(sec)<2,paste0(0,sec),sec)
  
  return(paste(min,sec,sep=':'))
}

#Interface de usuário
ui <- fluidPage(
  fluidRow(
    column(12,column(4,textInput("participante","",value="Participante")),
           column(4,align="center",h2(textOutput("cronometro"))),
           column(4,textInput("experimentador","",value="Experimentador"))),
    column(12,column(4),
           column(4,align="center",actionButton("start","Iniciar"),
                  actionButton("pause","Pausar")),
           column(4)),
    column(12), 
    column(12,align="center",actionButton("comp1","Comportamento 1", style='padding:15px 35px; font-size:100%'),
           actionButton("comp2","Comportamento 2", style='padding:15px 35px; font-size:100%')),
    column(12),
    column(12,align="center",actionButton("comp3","Comportamento 3", style='padding:15px 35px; font-size:100%'),
           actionButton("comp4","Comportamento 4", style='padding:15px 35px; font-size:100%')),
    column(12,align="center",textOutput("aviso")),
    column(12,actionButton("expdados","Exportar dados"),
           actionButton("expgraph","Exportar gráfico"))
  )
)

#Servidor
server <- function(input, output, session) {
  observeEvent(input$start,{
    vars$stTime=Sys.time()
    vars$start=T
  })
  observeEvent(input$pause,{
    vars$start=F
    fim=format.timediff(vars$stTime)
    vars$data[nrow(vars$data)+1,]=c(input$experimentador,input$participante,0,fim)
  })

  output$cronometro=renderText({
    invalidateLater(1000,session)
    if(vars$start){
      paste(format.timediff(vars$stTime))
    }else{
      paste("00:00")
    }
  })
  
  observeEvent(input$comp1,{
    ocorrencia=format.timediff(vars$stTime)
    output$aviso=renderText({paste("Comportamento 1 registrado em",ocorrencia)})
    vars$data[nrow(vars$data)+1,]=c(input$experimentador,input$participante,1,ocorrencia)
  })
  
  observeEvent(input$comp2,{
    ocorrencia=format.timediff(vars$stTime)
    output$aviso=renderText({paste("Comportamento 2 registrado em",ocorrencia)})
    vars$data[nrow(vars$data)+1,]=c(input$experimentador,input$participante,2,ocorrencia)
  })
  
  observeEvent(input$comp3,{
    ocorrencia=format.timediff(vars$stTime)
    output$aviso=renderText({paste("Comportamento 3 registrado em",ocorrencia)})
    vars$data[nrow(vars$data)+1,]=c(input$experimentador,input$participante,3,ocorrencia)
  })
  
  observeEvent(input$comp4,{
    ocorrencia=format.timediff(vars$stTime)
    output$aviso=renderText({paste("Comportamento 4 registrado em",ocorrencia)})
    vars$data[nrow(vars$data)+1,]=c(input$experimentador,input$participante,4,ocorrencia)
  })
  
  observeEvent(input$expdados,{
    write.csv(vars$data,paste0(paste(input$experimentador,input$participante,sep="-"),".csv"))
    output$aviso=renderText({paste("Os dados foram salvos na pasta.")})
  })
  
  observeEvent(input$expgraph,{
    dados=vars$data
    fim=dados$Ocorrencia[dados$Comportamento==0]
    dados=dados[dados$Comportamento!=0,]
    for(i in 1:unique(dados$Comportamento)){
      dados=rbind(c(input$experimentador,input$participante,i,"00:00"),dados)
      ggplot(dados[dados$Comportamento==i,], aes(x=ms(Ocorrencia), y=cumsum(grepl(i,Comportamento))-1)) +
        geom_line() + geom_point()+xlim(0,ms(fim))+ylab("Frequência de resposta")+xlab("Ocorrência por segundo")
      ggsave(paste0("Comportamento-",i,".png"))
    }
    output$aviso=renderText({paste("Os gráficos foram salvos na pasta.")})
  })
  
  session$onSessionEnded(function(){
    experimentador=isolate(input$experimentador)
    participante=isolate(input$participante)
    nome.do.arquivo=paste0(paste(experimentador,participante,sep="-"),".csv")
    nome.do.autoSave=paste0(paste(experimentador,participante,sep="-"),"AutoSave.csv")
    if(dim(isolate(vars$data))[1]>0){
      if(!file.exists(nome.do.arquivo)){
        write.csv(isolate(vars$data),nome.do.autoSave)
      } 
    }
  })
}

#Rodando o aplicativo
shinyApp(ui, server)