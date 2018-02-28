#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);require(ggplot2);require(cowplot)
data(list=c("mtcars","iris","ToothGrowth"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("One Dimensional Data Visuals"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("df","Choose Dataset",c("Motor Trend Car Road Tests"="mtcars","Iris (Flower) Measurements"="iris","Vitamin C Effects on Tooth Growth"="ToothGrowth"),selected="mtcars"),
        selectInput("vrbl","Choose Variable","",multiple=F),
        checkboxGroupInput("plottype","Choose Plot Type",choices=c("Dot Plot"="dot","Box Plot"="box","Histogram"="hist" )), #,"Bar Plot"="bar"
        tableOutput("stats")
      ),#end sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidPage( fluidRow(
        plotOutput("G")),
        fluidRow(
          #div(id='mydiv',class='simpleDiv',tags$br(),
          #tags$h4("First 6 lines of the dataset") ),#end div,
        tableOutput("view")#,
        )
      )#end fluidPage
      )#end mainPanel
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  DFinput<-reactive({
    switch(input$df,"mtcars"=mtcars,"iris"=iris,"ToothGrowth"=ToothGrowth)
  })
  
    observe(updateSelectInput(session,"vrbl",choices=names(DFinput())) )
  
    output$view<-renderTable(head(DFinput(),6))
    
    output$stats<-renderTable({
      values<-eval(parse(text=paste0("DFinput()$",input$vrbl)))
      print(input$vrbl)
      meanx<-mean(values,na.rm=T)
      medianx<-median(values,na.rm=T)

        Mode <- function(x) {
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
      modex<-Mode(values)
      madx<-mean(abs(values-meanx),na.rm=T)
      stats<-data.frame(Stat=c("Mean","Median","Mode","Mean Abs Dev"),Value=c(meanx,medianx,modex,madx))
      return(stats)

    })
  
### Define main plot
   output$G <- renderPlot({
      # generate bins based on input$bins from ui.R
     DF<-DFinput()
     #basic graph template
     g0<-ggplot()+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(face="bold",size=18))
     
     grafs<-list()
     
     #Dot Plot
     if("dot"%in%input$plottype)
        {
        tmp<-g0+geom_dotplot(data=DF,aes_string(x=input$vrbl),dotsize=.8)+theme(axis.text.y=element_blank())
        yrange<-ggplot_build(tmp)$layout$panel_params[[1]]$y.range
        grafs$dot<-tmp+ylim(yrange)
        }
     
     # if(input$plottype=="bar")
     #    {
     #    G<-g0+geom_bar(data=DF,aes_string(x=input$vrbl))
     #    }
     
     #Boxplot
     if("box"%in% input$plottype)
        {
        grafs$box<-g0+geom_boxplot(data=DF,aes_string(x=1,y=input$vrbl))+coord_flip()+theme(axis.text.y=element_blank(),axis.title.y=element_blank())
        }
     
     #Histogram
     if("hist" %in% input$plottype)
        {
        grafs$hist<-g0+geom_histogram(data=DF,aes_string(x=input$vrbl))
        }
     
     if(is.null(input$plottype)){
       ggplot()+annotate("text",x=-1,y=1,label="Choose a plot type",size=12)+theme_nothing()
     }else{ plot_grid(plotlist=grafs,align="v",ncol=1)#+coord_fixed(ratio=4/3) 
       }
     
   },height=200+100*length(grafs),width="auto")#end renderPlot
   
   # #make outputplot scale the height based on # of graphs
   # output$dynamicheight<-renderUI({
   #   plotOutput("G",height=200+200*output$plotheight)})
   
   
}#end serverside

# Run the application 
shinyApp(ui = ui, server = server)

