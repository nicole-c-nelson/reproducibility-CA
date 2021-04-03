#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggrepel)
# load data
df_bootstrap_partial_points <- readRDS("df_bootstrap_partial_points.RDS")
df_original_bootstrap <- readRDS("df_original_bootstrap.RDS")
df_bootstrap_hull <- readRDS("df_original_bootstrap_hull.RDS")
df_bootstrap_nodes <- readRDS("df_bootstrap_nodes.RDS")


fig_5_plot <- function(x, y){
    p <- ggplot(data = df_bootstrap_partial_points %>% filter(Node %in% x), aes(x=Dim.1, y=Dim.2, fill=Node))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
        geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
        #geom_point(aes(color=Node), size=0.5, show.legend = F)+
        geom_polygon(data = df_bootstrap_hull %>% filter(Node %in% x), alpha = 0.4, show.legend = F)+
        geom_point(data = df_original_bootstrap %>% filter(Node %in% x), aes(shape = sample), size = 3)+
        guides(fill = F) +
        scale_shape_manual(name = element_blank(), values = c(15,22)) +
        geom_line(data = rbind(df_bootstrap_partial_points %>% filter(Group == 1001 & Node %in% x), df_bootstrap_nodes),  aes(group=Node), linetype=2, show.legend = F)+
        geom_text_repel(data=filter(df_bootstrap_partial_points, Group == 1001 & Node %in% x), aes(label=Node), show.legend = F)+
        labs(x= "Dimension 1: ‘Discipline’ (8.38%)", y = "Dimension 2: ‘Audience’ (7.65%)")+
        theme(legend.position="bottom")
    if (y == FALSE) {
        p
    }
    else if (y == TRUE) {
        p +
        coord_cartesian(xlim = c(-120, 160), ylim = c(-100,125))}
}

NodeList <- df_bootstrap_partial_points %>% 
    distinct(Node) %>% 
    arrange(Node) %>% 
    pull()

NodeListInit <- df_bootstrap_partial_points %>% 
    distinct(Node) %>% 
    filter(Node %in% c("Bayesian statistics",
                        "Brian Nosek/Center for Open Science",
                        "Fraud",
                        "Heterogeneity",
                        "Impact on policy or habits",
                        "Incentives",
                        "Legitimacy of science",
                        "P values",
                        "Pre-registration",
                        "Publishing culture",
                        "Reagents",
                        "Retractions",
                        "Sample size and power",
                        "Transparency")) %>% 
    pull()

# Define UI for application that draws a histogram
ui <- navbarPage("Nelson et al",
                 tabPanel("Figure 5",
                          fluidPage(
                              
                              # Application title
                              titlePanel("Old Faithful Geyser Data"),
                              
                              # Sidebar with a slider input for number of bins 
                              sidebarLayout(
                                  
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      plotOutput("distPlot"),
                                  ),
                                  sidebarPanel(
                                      checkboxGroupInput("nodeSelect",
                                                         "Choose which nodes you want to display",
                                                         choices = NodeList,
                                                         selected = NodeListInit),
                                      checkboxInput("axesToggle",
                                                    "Keep plot axes fixed?",
                                                    FALSE)
                                  )
                              )
                          )),
                 tabPanel("Figure 2",
                          (fluidPage(
                              titlePanel("Correspondence analysis"),
                              sidebarLayout(
                                  mainPanel(
                                      plotOutput("plot1", height = 350,
                                                 click = "plot_click"
                                      ),
                                      p("Click on any nodes or article points in the plot to get more information about them.")
                                  ),
                                  sidebarPanel(
                                      h3("Node Information"),
                                      verbatimTextOutput("node_info"),
                                      h3("Article information"),
                                      verbatimTextOutput("article_info")
                                  )
                              )
                            )
                           )
                     
                 )
            )


server <- function(input, output) {

    output$distPlot <- renderPlot({
        fig_5_plot(input$nodeSelect, input$axesToggle)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
