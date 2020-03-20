library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyWidgets)
require(maps)  
require(mapdata) 
library(ggplot2) 
library(readxl) 
library(ggthemes) 
library(ggrepel) 
library(plotly)
library(stringi)
library(stringr)
library(shinythemes)
library(htmltools)
library(htmlwidgets)
library(dplyr)
dplyr::recode
ui <- fluidPage(
  theme=shinytheme('readable'),
  tags$style(type='text/css','#action{font-size:50px}'),
  tags$style(type='text/css','#action2{font-size:50px}'),
  tags$style(type='text/css','#action3{font-size:50px}'),
  tags$head(
    tags$style(HTML(" h4{
                    font-family: Cambria ,cursive;color: #000000;font-size:20px;}"))),
  titlePanel(tags$img(src='covidgraph.png',height=120,width=350,style='padding:0px',h4(
    "19/3/2020" ))),
  
  sidebarLayout(position = "right",
                sidebarPanel(
                  conditionalPanel(condition = "input.tabs1==1",tags$hr(),
                                   actionButton("action3", "Go!",class="btn-warning"),
                                   h4("Data source:"),
                                   h5("1. https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data"),
                                   h5("2. https://covid19.kemkes.go.id"),
                                   h5("(data will be updated every day)"),
                                 #  h4("Author(s):"),
                                #   h5("1. Hafizh Iman Naufal (ig:@hafizh_iman/twitter:@HAFIZHIMAN)"),
                                 #  h5("2. Thoriq Nashrullah  (ig:@bangtooy/twitter:@bangto0y)"),
                                 #  h6("Masukan dan saran terkait App ini dapat menghubungi kami melalui SosMed"),
                                 #  h6("Semoga App ini dapat bermanfaat, Terima kasih..."),
                                   h6("--------------------------------------------------------------------------")
                                   #  "
                  ),
                  conditionalPanel(condition = "input.tabs1==2",
                                   uiOutput("independents"),
                                   #setSliderColor("BlanchedAlmond",1),
                                   chooseSliderSkin("HTML5",color = 'black'),
                                   uiOutput("sleding"),
                                   actionButton("action2", "Go!",class="btn-primary"),
                                   tags$hr(),
                                   tags$hr(),
                                   uiOutput("independents2"),
                                   actionButton("action", "Go!",class="btn-danger")
                  ),
                ),
                mainPanel(
                  tabsetPanel(id="tabs1",
                              
                              tabPanel("      Global COVID-19      ",
                                       fluidRow(
                                         plotlyOutput("contents1"),
                                         plotlyOutput("contents2"),
                                         tableOutput("tabel")),
                                       plotlyOutput("contents5"),
                                       value=2),
                              tabPanel("      COVID-19 in Indonesia      ",
                                       fluidRow(
                                         plotlyOutput("contents4"),
                                         plotOutput("contents3",width = "100%", height = "800px") 
                                       )
                                       ,value=1)
                  )
                )
  )) 

server <- function(input, output, session){
  #batas0
  filedata = reactive({
    df = read.csv("covid19.csv",header = TRUE)
  })
  datapeta = reactive({
    df = read.csv("provinsi_corona.csv",header = TRUE)
  })
  #batas1
  output$independents = renderUI({
    df = filedata()
    items<-levels(df[,2])
    selectInput("independents",h3("Select Country:"),selected ="China",items,
                multiple=F,size =15,selectize = F ,width = '200%')
    
  })
  output$independents2 = renderUI({
    df = filedata()
    items<-levels(df[,2])
    selectInput("independents2",h3("Compare Countries:"),selected =c("China","Italy","Iran","Spain","South_Korea","France","Germany"),items,
                multiple=T)
    
  })
  #sleding
  output$sleding = renderUI({
    df = filedata()
    items<-subset(df,CountryExp==input$independents)
    n<-length(items[,1])
    sliderInput("sleding", label =h3("Slide to change number of days:"), min = 2,max = n, value = 13,
                round = T,post = " days",pre = "in last ",step = 1)
  })
  #datafix
  datafix=reactive({
    df = filedata()
    df =subset(df,CountryExp==input$independents)
    df =df[1:input$sleding,]
  })
  datafix2=reactive({
    df = filedata()
    laso=as.factor(input$independents2)
    df=subset(df, subset = CountryExp %in% laso)
    #df=filter(df,CountryExp==laso)
    df$CountryExp <- factor(df$CountryExp, levels =laso)
    df<-as.data.frame(df)
  })
  #data...................................................................................................
  #batas2
  output$contents1 = renderPlotly({
    validate(
      need(input$action2!=0, ' ')
    )
    input$action2
    isolate({datas = datafix()
    string<-as.character.Date(datas$DateRep)
    string<-str_replace_all(string," ","")
    string_split = strsplit(as.character(string), split = "/")
    vec<-array(NA,nrow(datas))
    for ( i in 1:nrow(datas)){
      vec[i]<-paste(string_split[[i]][3],string_split[[i]][1],string_split[[i]][2],sep = "-")
    }
    vec<-as.Date(vec)
    datas$DateRep<-vec
    angka<-c(sum(datas$NewConfCases),sum(datas$NewDeaths))
    kelas<-c("Kasus Konfirmasi","Kematian")
    datap<-data.frame(kelas,angka)
    #### ploting !
    fig <- plot_ly(datas, x = ~DateRep, y = ~NewDeaths, type = 'bar', name = 'Daily deaths',
                   marker = list(color = 'rgb(255, 51, 51)'))
    fig <- fig %>% add_trace(y = ~NewConfCases, name = 'Daily cases', marker = list(color = 'rgb(26, 118, 255)'))
    contents1 <- fig %>% layout(title = list(text=paste("Daily new cases and deaths in",input$independents),y=0.95),
                                tickfont = list(
                                  size = 25,
                                  color = 'rgb(0, 0, 0)'),font=list(family = "cambria",size = 18),
                                xaxis = list(
                                  title = "",
                                  tickfont = list(
                                    size = 16,
                                    color = 'rgb(0, 0, 0)')),
                                yaxis = list(
                                  title = 'COVID-19 daily cases and deaths',
                                  titlefont = list(
                                    size = 18,
                                    color = 'rgb(0, 0, 0)'),
                                  tickfont = list(
                                    size = 16,
                                    color = 'rgb(107, 107, 107)')),
                                legend = list(title=list(text='<b>  </b>'),x = 200,y = 1, bgcolor = 'rgba(0, 0, 0, 0)', 
                                              bordercolor = 'rgba(255, 255, 255, 0)'),barmode = 'group',margin = list(b = 0.1),bargroupgap = 0.05)})
    
    
  })
  #batas3
  output$contents2 = renderPlotly({
    validate(
      need(input$action2!=0, ' ')
    )
    input$action2
    isolate({    datas = datafix()
    string<-as.character.Date(datas$DateRep)
    string<-str_replace_all(string," ","")
    string_split = strsplit(as.character(string), split = "/")
    vec<-array(NA,nrow(datas))
    for ( i in 1:nrow(datas)){
      vec[i]<-paste(string_split[[i]][3],string_split[[i]][1],string_split[[i]][2],sep = "-")
    }
    vec<-as.Date(vec)
    datas$DateRep<-vec
    angka<-c(sum(datas$NewConfCases),sum(datas$NewDeaths))
    kelas<-c("Total cases","Total deaths")
    datap<-data.frame(kelas,angka)
    #### ploting!
    df <- datap
    fig2 <- df %>% plot_ly(labels = ~kelas, values = ~angka,marker = list(colors = c('#6a1051', '#a05f96')),
                           textinfo = 'value')
    fig2 <- fig2 %>% add_pie(hole = 0.6)
    fig2 <- fig2 %>% layout(title = list(text=paste("Total confirmed COVID-19 cases and deaths in",input$independents,
                                                    "for the last",input$sleding,"days"),y=1),  
                            showlegend = T,
                            font=list(family = "cambria",size = 18,color='black'))
    #  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig2 <- fig2 %>% layout(legend = list(title=list(text='<b>  </b>'),x = 200,y = 1.2, bgcolor = 'rgba(0, 0, 0, 0)'))
    })
  })
  #batas4
  output$contents3 = renderPlot({
    validate(
      need(input$action3!=0, ' ')
    )
    input$action3
    isolate({    mapel = datapeta()
    global <- map_data("world")
    gg1 <- ggplot() + 
      geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                   fill = "gray85", color = "gray80") + 
      coord_fixed(1.3) 
    gg1
    Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
      geom_point(data = mapel, aes(x = long, y = lat), 
                 color = "red", size=10 ,alpha = 0.5, show.legend = F) +
      geom_text_repel(data = mapel, aes(x = long, y = lat, 
                                        label= admin), color = "grey30",show.legend=F, size=5) + 
      ggtitle ("Areas in Indonesia with reported confirmed cases of COVID-19") + theme_map()+theme(plot.title = element_text(hjust=0.5))+
      theme(plot.title = element_text(size=28))+theme(plot.title = element_text(family ="cambria" ))
    Indo})
  })
  #batas5
  output$contents4 = renderPlotly({
    validate(
      need(input$action3!=0, ' ')
    )
    input$action3
    isolate({    fig <- plot_ly(
      type = 'table',
      columnorder = c(1,2),
      columnwidth = c(80,20),
      header = list(
        values = c('<b>COVID-19 situations in Indonesia </b>', '<b>#</b>'),
        line = list(color = '#506784'),
        fill = list(color = '#119DFF'),
        align = c('center','center'),
        font = list(color = 'white', size = 25,family="cambria"),height=40
      ),
      cells = list(
        values = rbind(
          c('Number of people inspected*', 'Positive COVID-19', 'Recovered (Positive COVID-19)', 'Dead (Positive COVID-19)'
            ,'Negative COVID-19','Inspection process'),
          c(1651,309,15,25,1342,"-")),
        line = list(color = '#506784'),
        fill = list(color = c('white', 'white')),
        align = c('center', 'center'),
        font = list(color = c('black'), size = 20,family="arial"),height=30
      ))
    
    fig<- fig %>% layout(title = list(text=paste("*This data was obtained from covid19.kemkes.go.id"),y=0.35),tickfont = list(
      size = 18,
      color = 'rgb(0, 0, 0)'),font=list(family = "cambria",size = 13))})
  })
  #batasssssssssss
  output$tabel = renderTable({
    validate(
      need(input$action2!=0, ' ')
    )
    input$action2
    isolate({ ds<-filedata()
    ds<-ds[,2:4]
    ns<-levels(ds$CountryExp)
    n<-length(ns)
    dash<-matrix(0,nrow = n,ncol = 3)
    dash<-as.data.frame(dash)
    dash[,1]<-ns
    for (i in 1:n){
      a<-subset(ds,CountryExp==ns[i])
      dash[i,2:3]<-c(sum(a[,2]),sum(a[,3]))
    }
    dash<-as.data.frame(dash)
    dash<-arrange(dash, desc(V2))
    dash$V2<-as.integer(dash$V2)
    dash$V3<-as.integer(dash$V3)
    Nomor<-seq(1:n)
    dash<-data.frame(Nomor,dash)
    colnames(dash)<-c("No.","Reporting Country","Total confirmed cases","Total deaths")
    head(dash,15)})
    
  },caption= "*15 countries with the largest confirmed COVID-19 cases")  
  
  #batas8
  output$contents5 = renderPlotly({
    validate(
      need(input$action!=0, ' ')
    )
    input$action
    isolate({ ds<-datafix2()
    ds<-ds[,2:4]
    ns<-levels(ds$CountryExp)
    n<-length(ns)
    dash<-matrix(0,nrow = n,ncol = 3)
    dash<-as.data.frame(dash)
    dash[,1]<-ns
    for (i in 1:n){
      a<-subset(ds,CountryExp==ns[i])
      dash[i,2:3]<-c(sum(a[,2]),sum(a[,3]))
    }
    dash<-as.data.frame(dash)
    # dash<-arrange(dash, desc(V2))
    dash$V2<-as.integer(dash$V2)
    dash$V3<-as.integer(dash$V3)
    colnames(dash)<-c("Nama_Negara","Kasus_Konfirmasi","Kematian")
    fig <- plot_ly(dash, x = ~Nama_Negara, y = ~Kasus_Konfirmasi, type = 'bar', name = 'Total cases',
                   marker = list(color = '#381460'))
    fig <- fig %>% add_trace(y = ~Kematian, name = 'Total deaths',marker = list(color = '#feb72b'))
    fig <- fig %>% layout(title = list(text=paste("COVID-19 confirmed cases and deaths by country"),y=0.95),
                          tickfont = list(size = 25,color = 'rgb(0, 0, 0)'),font=list(family = "cambria",size = 15),
                          yaxis = list(title = "Total confirmed COVID-19 cases and deaths",tickfont = list(size = 10,color = 'rgb(0, 0, 0)')),
                          xaxis = list(title = "Countries",tickfont = list(size = 16,color = 'rgb(0, 0, 0)'))
                          ,barmode = 'stack',
                          legend = list(title=list(text='<b>  </b>'),x = 200,y = 1, bgcolor = 'rgba(0, 0, 0, 0)', 
                                        bordercolor = 'rgba(255, 255, 255, 0)'))
    })
    
  }) 
  observeEvent(1==1, {
    showModal(modalDialog(
      title = h3("Hello"),
      "Covidgraph is ShinyApps which informs the current situations 
      related to COVID-19 in Indonesia / World (in interactive graphs).",h2(""),
      "If you find this application helpful, please share / spread this application link.",
      h2(""),
      "Thanks!"
    ))
  })
}

shinyApp(ui, server)