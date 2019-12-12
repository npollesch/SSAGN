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

# Define UI for application that draws a histogram
ui <- fluidPage(

visNetworkOutput("network_hello",height = "800px")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$network_hello<-renderVisNetwork({
    
    nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
    edges <- data.frame(from = c(1,2), to = c(2,3))
    
    visNetwork(nodes, edges, main="Nate is learning Shiny", submain="It is going ok") %>%
        visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
        visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
        addFontAwesome() %>%
        visLegend(addNodes = data.frame(label = c("A", "B"), shape = "icon", 
                                        icon.code = c("f0c0", "f007"), 
                                        icon.size = c(25,50), 
                                        icon.color = c(NA, "red")),
                  addEdges = data.frame(label = "link"), useGroups = FALSE)
})
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
