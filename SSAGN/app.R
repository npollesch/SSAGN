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
#library(googledrive)
library(plyr)
library(shinythemes)

# Define UI for application that draws network
ui <- fluidPage( theme = shinytheme("cyborg"),
    
    titlePanel("Cafe Scientifique Twin Ports - SSAG/N"),
    #setBackgroundColor(color="Black"),
      sidebarLayout(
      
      sidebarPanel(
        uiOutput("choose_node"),
        actionButton("focus_now", "Zoom in!"),
        uiOutput("choose_node_inst"),
        actionButton("focus_now_inst", "Zoom in!"),
        actionButton("zoom_out","Reset view!"),
        h5(textOutput("nodeid")),
        h5(textOutput("nodeaffl")),
        h5(textOutput("hovernode")),
        tags$head(tags$style("#nodeid{color: orange;
                                 font-size: 20px;
                                 }"
        ),tags$style("#nodeaffl{color: orange;
                                 font-size: 14px;
                                 }"
        ),tags$style("#hovernode{color: gray;
                                 font-size: 14px;
                                 }"
        )
        ),
        ),
      
      mainPanel(
       
        
       
        visNetworkOutput("network_proxy_nodes",height = "800px")
       
      )
    )
#visNetworkOutput("network_hello",height = "800px")
    
)

# Define server logic required to draw network
server <- function(input, output) {

  network_raw<-read.csv("cstpnetwork.csv")
  nodes <- data.frame(id = network_raw$Name, 
                      Affiliation1 = network_raw$Affiliation.1, 
                      Affiliation2 = network_raw$Affiliation.2, 
                      CSTPEventTitle=network_raw$Presentation.Title, 
                      group="Person",person=T)
  nodes<-rbind.fill(nodes,data.frame(id = unique(network_raw$Affiliation.1), person=F, group="Institution"))
  nodes<-rbind.fill(nodes,data.frame(id = unique(network_raw$Affiliation.2), person=F, group="Institution"))
  nodes<-rbind.fill(nodes,data.frame(id = "Cafe Scientifique Twin Ports", person=F, group="CSTP"))
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
    visNetwork(nodes, edges,main="Superior Science Advocacy Group/Network",submain="This network shows the connections between CSTP event leaders and the instutions they represent or areas they are experts in. We have big plans for using this network, so stay tuned.") %>%
      visInteraction(hover = T) %>%
      visGroups(groupname="CSTP",size=75,shape="image",image=list(selected="https://cafescitwinports.files.wordpress.com/2019/12/ssn-icon-tsp-selectedbold.png",unselected="https://cafescitwinports.files.wordpress.com/2019/12/ssn-icon-tsp.png")) %>%
     # visNodes(color=list(hover="purple",highlight="red"))%>%
      visGroups(groupname="Institution",shape="square",color=list(hover="gray",highlight='orange'))%>%
      visGroups(groupname="Person",color=list(hover="gray",highlight="orange"))%>%
      visLegend(position="right",width=.1) %>%
      visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}", blurNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', null);
                ;}") %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id_select', nodes.nodes);
                ;}")# %>%
      #visEvents(select = "function(nodes) {
      #         Shiny.onInputChange('current_node_id_select', null);
       #         ;}")
  })
  
  ###### Select events
  output$nodeid = reactive({
    
    n <- input$current_node_id_select
    mynode <- nodes[(nodes$id == n),1]
    myaffl1 <- nodes$Affiliation1[(nodes$id==n)]
    myaffl2 <- nodes$Affiliation2[(nodes$id==n)]
    if(is.null(n)){out<-""}
    else(out<-paste(mynode))
    return(out)
  })
  
  
  output$nodeaffl = reactive({
    n <- input$current_node_id_select
    myaffl1 <- nodes$Affiliation1[(nodes$id==n)]
    if(is.null(n)){out<-""}
    else(
      if(is.na(myaffl1)){out<-""}
         else(out<-paste("Affiliation: ",myaffl1,sep="")))
    return(out)
  })
  
  ##### Hover events
  
  output$hovernode <-
  renderText({
    if(is.null(input$current_node_id)){""}
    else(paste("",input$current_node_id[[1]]))
  })
  

#### Zoom and focus events
  
  observeEvent(input$focus_now, {
    visNetworkProxy("network_proxy_nodes") %>%
      visFocus(id = input$Focus, scale = 1) %>%
      visSelectNodes(id = input$Focus)
  })
  
  observeEvent(input$focus_now_inst, {
    visNetworkProxy("network_proxy_nodes") %>%
      visFocus(id = input$Focus_inst, scale = 1) %>%
      visSelectNodes(id = input$Focus_inst)
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



#### IDEAS
# Each person can have 3 affiliations and 1 discipline
# Each person can be open to mentorship or not
