library(visNetwork)
library(shiny)
library(dplyr)

#data frame needed for code to work
#Nodes
# Name	Family	size	NameID	FamilyID	Path	Definition

#Edges
#id	from	to	source	target	family	size	group	value	Path	PathID

# Load Nodes and Links
nodes <- data.frame(read.csv( "Lifecycle_nodesupdate.csv"), 
                    header = TRUE, stringsAsFactors = FALSE)
edges <- data.frame(read.csv( "Lifecycle_linkswithharmonizedpaths.csv"), 
                    header = TRUE, stringsAsFactors = FALSE)
# Print the structure of the data to debug
print("Nodes data structure:")
print(str(nodes))

print("Edges data structure:")
print(str(edges))

ui <- fluidPage(
  title = "Data and Information Lifecycle",
  fillPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "filteredges",
          "Select path option",
          choices = c("All", sort(as.character(unique(edges$Path)))),  # Use unique path names from path column in data,
          selected = "All"
        ),
        width = 3
      ),
      mainPanel(
        visNetworkOutput("network_proxy_update", width = "100%", height = "90vh"),
        width = 9
      )
    )
  )
)

server <- function(input, output, session) {
  output$network_proxy_update <- renderVisNetwork({
    req(input$filteredges)
    
    # Validate edges data
    validate(
      need(nrow(edges) > 0, "Edges data is empty or invalid."),
      need(nrow(nodes) > 0, "Nodes data is empty or invalid."),
      need("source" %in% colnames(edges), "Edges data is missing 'source' column."),
      need("target" %in% colnames(edges), "Edges data is missing 'target' column."),
      need("Path" %in% colnames(edges), "Edges data is missing 'Path' column.")
    )
    
    # Check if the input$filteredges is a valid selection
    if (input$filteredges == "All") {
      selected_edges <- data.frame(from = edges$source, to = edges$target, arrows = "to", dashes=edges$dashes,width=edges$value,type = edges$Path, smooth = FALSE)
    } else {
      selected_edges <- subset(data.frame(from = edges$source, to = edges$target, arrows = "to", width=edges$value,type = edges$Path, smooth = FALSE), type == input$filteredges)
    }
    
    # Debugging: Check if selected_edges is empty
    if (nrow(selected_edges) == 0) {
      print("Selected edges is empty after filtering:")
      print(selected_edges)
    } else {
      print("Selected edges:")
      print(selected_edges)
    }
    
    visNetwork(
      data.frame(id = nodes$NameID, title = nodes$Definition, label = nodes$Name, group = nodes$Family, value = nodes$size),
      selected_edges,
      main = "Data and Information Lifecycle"
    ) %>%
      visNodes(font=list(size=20))%>%
      visLayout(randomSeed = 143) %>%
      visOptions(selectedBy = "group", highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "hierarchicalRepulsion", hierarchicalRepulsion = list(springLength = 400), stabilization = TRUE) %>%
      visLegend(main=list(text="Legend"),useGroups = TRUE, addEdges=data.frame(label=c("Validated","Potential"),dashes=c(FALSE,TRUE)),width=.15,stepX=10) %>%
      visInteraction(
        tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;font-family: verdana;font-size:18px;font-color:#000000;
                        background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                        border: 1px solid #808074;box-shadow: 3px 3px 15px rgba(0, 0, 0, 0.2);max-width:300px;word-break: none',
        multiselect = TRUE,
        navigationButtons = TRUE
      )
  })
  
  myVisNetworkProxy <- visNetworkProxy("network_proxy_update")
  
  observe({
    req(input$filteredges)
    
    if (input$filteredges == "All") {
      filteredEdges <- edges
      hiddenEdges <- data.frame(source=character(0), target=character(0), Path=character(0), stringsAsFactors=FALSE)  # Empty data frame
    } else {
      filteredEdges <- edges[edges$Path == input$filteredges, , drop = FALSE]
      hiddenEdges <- anti_join(edges, filteredEdges, by = c("source", "target", "Path"))
    }
    
    # Debugging: Check if filteredEdges is empty
    if (nrow(filteredEdges) == 0) {
      print("Filtered edges is empty after filtering:")
      print(filteredEdges)
    } else {
      print("Filtered edges:")
      print(filteredEdges)
    }
    
    # Debugging: Check if hiddenEdges is empty
    if (nrow(hiddenEdges) == 0) {
      print("Hidden edges is empty after anti_join:")
      print(hiddenEdges)
    } else {
      print("Hidden edges:")
      print(hiddenEdges)
    }
    
    if (nrow(hiddenEdges) > 0) {
      visRemoveEdges(myVisNetworkProxy, id = hiddenEdges$id)
    }
    
    if (nrow(filteredEdges) > 0) {
      visUpdateEdges(myVisNetworkProxy, edges = filteredEdges)
    }
  })
}

shinyApp(ui = ui, server = server)