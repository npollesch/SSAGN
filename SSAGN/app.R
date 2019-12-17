#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(googledrive)
library(plyr)
library(shinythemes)

# Define UI for application that draws network
ui <- fluidPage( theme = shinytheme("cyborg"),
    
    titlePanel("Superior Science Advocacy Group/Network"),
    #setBackgroundColor(color="Black"),
      sidebarLayout(
      
      sidebarPanel(
        uiOutput("choose_node"),
        actionButton("focus_now", "Zoom in!"),
        uiOutput("choose_node_inst"),
        actionButton("focus_now_inst", "Zoom in!"),
        actionButton("zoom_out","Zoom out!")
        ),
      
      mainPanel(
        h4(textOutput("shiny_return")),
        visNetworkOutput("network_proxy_nodes",height = "800px")
        #h4(textOutput("nodes_data_from_shiny"))
      )
    )
#visNetworkOutput("network_hello",height = "800px")
    
)

# Define server logic required to draw network
server <- function(input, output) {

  network_raw<-read.csv("cstpnetwork.csv")
  nodes <- data.frame(id = network_raw$Name, Affiliation1 = network_raw$Affiliation.1, Affiliation2 = network_raw$Affiliation.2, CSTPEventTitle=network_raw$Presentation.Title, person=T)
  nodes<-rbind.fill(nodes,data.frame(id = unique(network_raw$Affiliation.1), person=F))
  nodes<-rbind.fill(nodes,data.frame(id = unique(network_raw$Affiliation.2), person=F))
  nodes<-rbind.fill(nodes,data.frame(id = "Cafe Scientifique Twin Ports", person=F,image="iconcstp.jpg",shape="circularImage"))
  nodes$Affiliation2[which(nodes$Affiliation2=="")]<-NA
  nodes<-nodes[!duplicated(nodes$id),]
  nodes<-nodes[-which(nodes$id==""),]
  nodes<-cbind(nodes,font.color="White",font.size=20)
  
  CSTPtoPeeps<-unique(data.frame(from="Cafe Scientifique Twin Ports",to=nodes$id[nodes$person]))

  Af1toAf2<-unique(data.frame(from=nodes[which(!is.na(nodes$Affiliation2)),2],to=nodes[which(!is.na(nodes$Affiliation2)),3]))
  PeepstoAf2<-unique(data.frame(from=nodes$id[nodes$person],to=nodes$Affiliation2[nodes$person]))
  PeepstoAf2<-PeepstoAf2[!is.na(PeepstoAf2[,2]),]
  PeepstoAf1<-unique(data.frame(from=nodes$id[nodes$person],to=nodes$Affiliation1[nodes$person]))
  
  edges<-rbind(CSTPtoPeeps,Af1toAf2,PeepstoAf2,PeepstoAf1)
  
  output$network_proxy_nodes <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visInteraction(hover = T) %>%
      visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}") %>%
visEvents(select = "function(nodes){Shiny.onInputChange('current_node_id',nodes.nodes);
                ;}")
  })
  
  
  output$shiny_return <-
  renderText({
    if(is.null(input$current_node_id)){"Hover for info"}
    else(input$current_node_id[[1]])
  })
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  output$table <- renderText({
    nodes[which(myNode$selected == nodes$id),]
  })
  
  
  
  
  
  observeEvent(input$focus_now, {
    visNetworkProxy("network_proxy_nodes") %>%
      visFocus(id = input$Focus, scale = 2)
      
  })
  
  observeEvent(input$focus_now_inst, {
    visNetworkProxy("network_proxy_nodes") %>%
      visFocus(id = input$Focus_inst, scale = 2)
    
  })
  
  observeEvent(input$zoom_out, {
    visNetworkProxy("network_proxy_nodes") %>%
      visFit()
    
  })
  
  output$choose_node <- renderUI({
    selectInput("Focus", "Find a person:",
                nodes$id[which(nodes$person==T)])
  })
  
  output$choose_node_inst <- renderUI({
    selectInput("Focus_inst", "Find an institution:",
                nodes$id[which(nodes$person==F)])
  })
    
  }

# Run the application 
shinyApp(ui = ui, server = server)
