library(ggplot2)
library(viridis)
library(ggrepel)
library(shiny)


load("fig2a_data.RData")

# functions
# take coordinates from plot_click and return text for output

create_output_articles <- function(x) {
    art_clicked <- nearPoints(df_CA_results_articles_2, x, threshold = 5, maxpoints = 1, addDist = T)
    paste0(art_clicked$Name, "\n",
           "Dimension 1: ", art_clicked$Dim_1, "\n",
           "Dimension 2: ", art_clicked$Dim_2, "\n",
           "Audience: ", art_clicked$audience, "\n",
           "Author: ", art_clicked$author)
}

create_output <- function(x){
    node_clicked <- nearPoints(df_CA_results_nodes, x, threshold = 15, maxpoints = 1,
                            addDist = TRUE)
    art_clicked <- nearPoints(df_CA_results_articles_2, x, threshold = 15, maxpoints = 1,
                              addDist = TRUE)
    toggle <- 
    if (!is.null(node_clicked$Node)){
        paste0(node_clicked$Node, "\n",
                   "Dimension 1: ", node_clicked$Dim_1, "\n",
                   "Dimension 2: ", node_clicked$Dim_2, "\n")
    }
    else if (!is.null(art_clicked$Name)){
        paste0(art_clicked$Name, "\n",
                  "Dimension 1: ", art_clicked$Dim_1, "\n",
                "Dimension 2: ", art_clicked$Dim_2, "\n",
                "Audience: ", art_clicked$audience, "\n",
               "Author: ", art_clicked$author)
    }
    else {
        return("")
    }
    }
    


ui <- fluidPage(
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


server <- function(input, output
) {
    output$plot1 <- renderPlot({
        ggplot(df_CA_results_articles_2, aes(Dim_1,Dim_2)) +
            theme_bw()+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
            geom_vline(xintercept = 0, linetype=2, color="darkgrey") +
            scale_color_viridis(discrete = TRUE, option = "D") +
            geom_point(aes(color = term)) +
            geom_point(data=df_CA_results_sup_var, 
                       shape = 25, 
                       color="white", 
                       fill = "red",
                       size = 3,
                       aes(x=Dim_1, y=Dim_2))+
            geom_point(data = df_CA_results_nodes, 
                       aes(Dim_1, Dim_2, size=Contrib_1_2), 
                       shape = 22, 
                       fill = "lightgrey") +
            geom_text_repel(data=df_CA_results_sup_var, color="red",
                            aes(label = Name), point.padding = 0.25, box.padding = 0.5)+
            geom_text_repel(data = subset(df_CA_results_nodes, Contrib_1_2 > 4), 
                            aes(label = Node), point.padding = 0.25, box.padding = 0.75)+
            labs(size="Contribution",color="Terms",
                 x="Dimension 1: ‘Discipline’ (8.50%)", y="Dimension 2: ‘Audience’ (7.73%)")+
            theme(legend.position = "bottom")
        
        
    })
    
    output$node_info <- renderText(create_output(input$plot_click))
    
    output$article_info <- renderText(create_output_articles(input$plot_click))
    
    output$hover_info <- renderPrint({
        cat("input$plot_hover:\n")
        str(input$plot_hover)
    })
    output$dblclick_info <- renderPrint({
        cat("input$plot_dblclick:\n")
        str(input$plot_dblclick)
    })
    output$brush_info <- renderPrint({
        cat("input$plot_brush:\n")
        str(input$plot_brush)
    })
    
}


shinyApp(ui, server)

