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
         plotOutput("G"),
        tags$div(
          tags$br(),
          tags$h4("First 6 lines of the dataset")),
        tableOutput("view")
      )
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
      meanx<-mean(values)
      medianx<-median(values)

        Mode <- function(x) {
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
      modex<-Mode(values)
      madx<-mean(abs(values-meanx))
      stats<-data.frame(Stat=c("Mean","Median","Mode","Mean Abs Dev"),Value=c(meanx,medianx,modex,madx))
      return(stats)

    })
  
   output$G <- renderPlot({
      # generate bins based on input$bins from ui.R
     DF<-DFinput()
     #basic graph template
     g0<-ggplot()+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(face="bold",size=18))
     
     grafs<-list()
     
     if("dot"%in%input$plottype)
        {
        grafs$dot<-g0+geom_dotplot(data=DF,aes_string(x=input$vrbl),dotsize=.8)+theme(axis.text.y=element_blank())
        }
     
     # if(input$plottype=="bar")
     #    {
     #    G<-g0+geom_bar(data=DF,aes_string(x=input$vrbl))
     #    }
     
     if("box"%in% input$plottype)
        {
        grafs$box<-g0+geom_boxplot(data=DF,aes_string(x=1,y=input$vrbl))+coord_flip()+theme(axis.text.y=element_blank(),axis.title.y=element_blank())
        }
     
     if("hist" %in% input$plottype)
        {
        grafs$hist<-g0+geom_histogram(data=DF,aes_string(x=input$vrbl))
        }
     
     if(is.null(input$plottype)){
       ggplot()+annotate("text",x=-1,y=1,label="Choose a plot type",size=12)+theme_nothing()
     }else{ plot_grid(plotlist=grafs,align="v",cols=1) }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

