library(shiny)
library(ggplot2)
library(ggrepel)
library(dichromat)


binmodf <- function(Tournament_Round, First_Serve_Pct , 
                    Second_Serve_Pct ,  Aces_Won , 
                    Double_Faults_Commited , Winners_Earned , Unforced_Errors , 
                    Break_Points_Created , Break_Points_Won , Net_Points_Attempted ,
                    Net_Points_Won , Male_Female_Tournament , Tournament_Name){
  
  
  
  
  if (Male_Female_Tournament == 'Male'){ Male_Female_TournamentMale = 1}else{Male_Female_TournamentMale=0}
  if (Tournament_Name == 'FrenchOpen'){
    Tournament_NameFrenchOpen = 1
    Tournament_NameUSOpen = 0
    Tournament_NameWimbledon = 0
  }
  else if (Tournament_Name == 'USOpen'){
    Tournament_NameFrenchOpen = 0
    Tournament_NameUSOpen = 1
    Tournament_NameWimbledon = 0
  }
  else if (Tournament_Name == 'Wimbledon'){
    Tournament_NameFrenchOpen = 0
    Tournament_NameUSOpen = 0
    Tournament_NameWimbledon = 1
  }
  else{
    Tournament_NameFrenchOpen = 0
    Tournament_NameUSOpen = 0
    Tournament_NameWimbledon = 0
  }
  

  
  
  projection = -3.28713201088239  +  
    Tournament_Round * -0.0596944332867595  +  First_Serve_Pct * 0.0227410995598204  +  
    Second_Serve_Pct * 0.0177076885105162  +  Aces_Won * 0.00709987099430909  +  
    Double_Faults_Commited * -0.106163422604834  +  Winners_Earned * 0.109856404798583  +  
    Unforced_Errors * -0.105275957162989  +  Break_Points_Created * 0.323160224095126  +  
    Break_Points_Won * 0.205370183640738  +  Net_Points_Attempted * -0.0378229507606594  +  
    Net_Points_Won * -0.0240683567879807  +  Male_Female_TournamentMale * -0.392968636196737  +  
    Tournament_NameFrenchOpen * -0.461655740083599  +  Tournament_NameUSOpen * -0.120514349309649  +  
    Tournament_NameWimbledon * -1.28741607921663
  
  sigmoid = 1 / (1 + exp(-projection))
  print (projection)
  return (sigmoid)
}

Interpret_Dist = function(IN, Distribution, param){
  
  if(Distribution == 'norm'){
    Mean_ = param[1]; SD_ = param[2];center = qnorm(0.5,Mean_, SD_); point = pnorm(IN,Mean_, SD_);}
  else if(Distribution == 'exp'){Rate = param[1];point  = pexp(IN,Rate);}
  else if(Distribution == 'gamma'){Rate = param[1];Shape = param[2];point  = pgamma(IN,rate = Rate,shape = Shape);}
  return (paste(as.character(round(point*100)),"% of population did as much or less than chosen value"))
  }


Plot_Dense <- function(IN, Distribution, param){
  if(Distribution == 'norm'){
    Mean_ = param[1]; SD_ = param[2];lim1 = param[3]; lim2 = param[4];
    dist <- qnorm(1:1000/1000,Mean_, SD_);
  }
  else if(Distribution == 'exp'){
    Rate = param[1]; lim1 = param[2]; lim2 = param[3];
    dist <- qexp(1:1000/1000,Rate);
  }
  else if(Distribution == 'gamma'){
    Rate = param[1]; Shape = param[2];lim1 = param[3]; lim2 = param[4];
    dist <- qgamma(1:1000/1000,rate = Rate,shape = Shape);
  }
  Y = density(dist, n = 1000)$y
  X = density(dist, n = 1000)$x
  ref = data.frame(X,Y)

  N = IN
  R = as.numeric(rownames(ref[(ref$X > N - 0.1) & (ref$X < N + 0.1),]))[1]
  value = ref$Y[R]
  if (is.na(value)){value = 0}
  ggplot()+
    geom_density(aes(dist), fill = 'blue',color = 'purple', alpha = 0.2, size = 1) + 
    geom_histogram(
      aes(x = dist,y=..density..),
      bins = 50, fill = 'blue',color = 'purple', alpha = 0.2) + 
    geom_vline(xintercept = IN, color = "deeppink4" , size = 1.5, linetype = "dashed") + 
    xlim(lim1,lim2) + 
    geom_text(
      aes(
        x = IN, y = value, label = IN
        ), size = 10, color = 'black',family="Times", fontface="bold.italic", lineheight=.8) + 
    theme_minimal(base_size = 16)
  
}



# Define server logic required to generate and plot a random distribution
server <- shinyServer(function(input, output) {
  #experimental
  output$rPlot <- renderPlot({
    prob = binmodf(as.numeric(input$Tournament_Round), as.numeric(input$First_Serve_Pct) , 
                   as.numeric(input$Second_Serve_Pct) ,  as.numeric(input$Aces_Won) , 
                   as.numeric(input$Double_Faults_Commited) , as.numeric(input$Winners_Earned) , 
                   as.numeric(input$Unforced_Errors) , as.numeric(input$Break_Points_Created) , 
                   as.numeric(input$Break_Points_Won) , as.numeric(input$Net_Points_Attempted) ,
                   as.numeric(input$Net_Points_Won) , input$Male_Female_Tournament, input$Tournament_Name)
    #print (prob)
    
    output <- data.frame(group = c("Winning Probability", "Loosing Probability"),value = c(prob , 1 - prob))
    ggplot(output, aes(x="", y=value, fill=group))+geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      geom_text_repel(aes(label = round(value,2)), color = "white", size=6) + theme_void()
  })
  
  

  output$distPlot_1 <- renderPlot({
    Plot_Dense(
      IN = input$First_Serve_Pct,Distribution = 'norm',param = c(62,8.19,0,100)
      )
  })
  output$p1_text <- renderText(Interpret_Dist(input$First_Serve_Pct,Distribution = 'norm',param = c(62,8.19)))
  
  
  
  output$distPlot_2 <- renderPlot({
    Plot_Dense(
      IN =input$Second_Serve_Pct,Distribution = 'norm',param = c(38,8.19,0,100)
      )
  })
  output$p2_text <- renderText(Interpret_Dist(input$Second_Serve_Pct,Distribution = 'norm',param = c(38,8.19)))
  
  #For Aces
  output$distPlot_3 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Aces_Won),Distribution = 'exp',param = c(0.18,0,40)
    )
  })
  output$p3_text <- renderText(Interpret_Dist(input$Aces_Won,Distribution = 'exp',param = c(0.18)))
  
  #For doubleFault
  output$distPlot_4 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Double_Faults_Commited),Distribution = 'gamma',param = c(0.45,1.8,0,25)
    )
  })
  output$p4_text <- renderText(Interpret_Dist(input$Double_Faults_Commited,Distribution = 'gamma',param = c(0.45,1.8)))
  
  #Winners Earned
  output$distPlot_5 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Winners_Earned),Distribution = 'gamma',param = c(0.09,2.6,0,100)
    )
  })
  output$p5_text <- renderText(Interpret_Dist(input$Winners_Earned,Distribution = 'gamma',param = c(0.09,2.6)))
  
  #Unforced Error
  output$distPlot_6 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Unforced_Errors),Distribution = 'gamma',param = c(0.12,3.7,0,100)
    )
  })
  output$p6_text <- renderText(Interpret_Dist(input$Unforced_Errors,Distribution = 'gamma',param = c(0.12,3.7)))
  
  #Break_Points_Created
  output$distPlot_7 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Break_Points_Created),Distribution = 'gamma',param = c(0.35,2,0,30)
    )
  })
  output$p7_text <- renderText(Interpret_Dist(input$Break_Points_Created,Distribution = 'gamma',param = c(0.35,2)))
  
  #Break_Points_Won
  output$distPlot_8 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Break_Points_Won),Distribution = 'gamma',param = c(0.2,1.7,0,40)
    )
  })
  output$p8_text <- renderText(Interpret_Dist(input$Break_Points_Won,Distribution = 'gamma',param = c(0.2,1.7)))
  
  #Net_Points_Attempted
  output$distPlot_9 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Net_Points_Attempted),Distribution = 'gamma',param = c(0.155,2.25,0,100)
    )
  })
  output$p9_text <- renderText(Interpret_Dist(input$Net_Points_Attempted,Distribution = 'gamma',param = c(0.155,2.25)))
  
  #Net_Points_Won
  output$distPlot_10 <- renderPlot({
    Plot_Dense(
      IN =as.numeric(input$Net_Points_Won),Distribution = 'gamma',param = c(0.123,2.18,0,100)
    )
  })
  output$p10_text <- renderText(Interpret_Dist(input$Net_Points_Won,Distribution = 'gamma',param = c(0.123,2.18)))
  
  output$Info_ <- renderUI({
    line0 = "<br/><br/>"
    line1 = " - Data Source https://archive.ics.uci.edu"
    line2 = " - Data Name: Tennis Major Tournament Match Statistics"
    line3 = " - Author: https://github.com/siddharthapachhai"
    line4 = " - Probability of victory in a match given the factors"
    
    HTML(paste(line1,line2,line3,line4,sep = "<br/><br/>"))
    })
  
})

# Define UI for application that plots random distributions 
ui <- shinyUI(fluidPage(
  titlePanel("Tennis Outcome Prediction"),
  sidebarLayout(
    sidebarPanel(width = 4,
      selectInput("Tournament_Round","Tournament Round",1:7),
      sliderInput("First_Serve_Pct","First Serve Percent",min = 1, max = 100, value = 1),
      sliderInput("Second_Serve_Pct","Second Serve Percent",min = 1, max = 100, value = 1),
      numericInput("Aces_Won", "Aces Won", value = 1, min =0),
      numericInput("Double_Faults_Commited", "Double Faults Commited", value = 1, min =0),
      numericInput("Winners_Earned", "Winners Earned", value = 1, min =0),
      numericInput("Unforced_Errors", "Unforced Errors", value = 1, min =0),
      numericInput("Break_Points_Created", "Break Points Created", value = 1, min =0),
      numericInput("Break_Points_Won", "Break Points Won", value = 1, min =0),
      numericInput("Net_Points_Attempted", "Net Points Attempted", value = 1, min =0),
      numericInput("Net_Points_Won", "Net Points Won", value = 1, min =0),
      selectInput("Male_Female_Tournament","Male or Female",c("Male","Female")),
      selectInput("Tournament_Name","Tournament Name",c("USOpen","Wimbledon","AusOpen","FrenchOpen"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("First Serve Percent", fluidRow(
                                      column(8, plotOutput("distPlot_1"), offset = 1),
                                      column(8, h4(textOutput("p1_text")), offset = 1)
                                    )),
                  tabPanel("Second Serve Percent", fluidRow(
                    column(8, plotOutput("distPlot_2"), offset = 1),
                    column(8, h4(textOutput("p2_text"), offset = 1))
                  )),
                  tabPanel("Aces Won", fluidRow(
                    column(8, plotOutput("distPlot_3"), offset = 1),
                    column(8, h4(textOutput("p3_text"), offset = 1))
                  )),
                  tabPanel("Double Faults Commited", fluidRow(
                    column(8, plotOutput("distPlot_4"), offset = 1),
                    column(8, h4(textOutput("p4_text"), offset = 1))
                  )),
                  tabPanel("Winners Earned", fluidRow(
                    column(8, plotOutput("distPlot_5"), offset = 1),
                    column(8, h4(textOutput("p5_text"), offset = 1))
                  )),
                  tabPanel("Unforced Errors", fluidRow(
                    column(8, plotOutput("distPlot_6"), offset = 1),
                    column(8, h4(textOutput("p6_text"), offset = 1))
                  )),
                  tabPanel("Break Points Created", fluidRow(
                    column(8, plotOutput("distPlot_7"), offset = 1),
                    column(8, h4(textOutput("p7_text"), offset = 1))
                  )),
                  tabPanel("Break Points Won", fluidRow(
                    column(8, plotOutput("distPlot_8"), offset = 1),
                    column(8, h4(textOutput("p8_text"), offset = 1))
                  )),
                  tabPanel("Net Points Attempted", fluidRow(
                    column(8, plotOutput("distPlot_9"), offset = 1),
                    column(8, h4(textOutput("p9_text"), offset = 1))
                  )),
                  tabPanel("Net Points Won", fluidRow(
                    column(8, plotOutput("distPlot_10"), offset = 1),
                    column(8, h4(textOutput("p10_text"), offset = 1))
                  )),
                  tabPanel("Info", fluidRow(
                    column(12,h3(htmlOutput("Info_")),offset = 1))),
                  column(8, plotOutput("rPlot"),offset = 1)
                  
        )
      )
  )
)) 

#binmodf(1,65,35,6,5,30,17,4,7,9,12,"Female","AusOpen")

shinyApp(ui = ui, server = server)