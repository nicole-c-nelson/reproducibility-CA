
library(shiny)
library(tidyverse)
library(ggrepel)
library(janitor)
library(viridis)


# Figure 5
## load data
df_bootstrap_partial_points <-
    readRDS("df_bootstrap_partial_points.RDS")
df_original_bootstrap <- readRDS("df_original_bootstrap.RDS")
df_bootstrap_hull <- readRDS("df_original_bootstrap_hull.RDS")
df_bootstrap_nodes <- readRDS("df_bootstrap_nodes.RDS")

## generate plot for figure 5 with two inputs:
## x filters for nodes selected in the input panels
## y is a toggle for setting the coordinate system to fixed or not fixed
fig_5_plot <- function(x, y) {
    p <-
        ggplot(data = df_bootstrap_partial_points %>% filter(Node %in% x),
               aes(x = Dim.1, y = Dim.2, fill = Node)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        geom_hline(yintercept = 0,
                   linetype = 2,
                   color = "darkgrey") +
        geom_vline(xintercept = 0,
                   linetype = 2,
                   color = "darkgrey") +
        #geom_point(aes(color=Node), size=0.5, show.legend = F)+
        geom_polygon(
            data = df_bootstrap_hull %>% filter(Node %in% x),
            alpha = 0.4,
            show.legend = F
        ) +
        geom_point(data = df_original_bootstrap %>% filter(Node %in% x),
                   aes(shape = sample),
                   size = 3) +
        guides(fill = F) +
        scale_shape_manual(name = element_blank(), values = c(15, 22)) +
        geom_line(
            data = rbind(
                df_bootstrap_partial_points %>% filter(Group == 1001 &
                                                           Node %in% x),
                df_bootstrap_nodes
            ),
            aes(group = Node),
            linetype = 2,
            show.legend = F
        ) +
        geom_text_repel(
            data = filter(df_bootstrap_partial_points, Group == 1001 &
                              Node %in% x),
            aes(label = Node),
            show.legend = F
        ) +
        labs(x = "Dimension 1: ‘Discipline’ (8.38%)", y = "Dimension 2: ‘Audience’ (7.65%)") +
        theme(legend.position = "bottom")
    if (y == FALSE) {
        p
    }
    else if (y == TRUE) {
        p +
            coord_cartesian(xlim = c(-120, 160),
                            ylim = c(-100, 125))
    }
}

## list of all nodes for display in UI
NodeList <- df_bootstrap_partial_points %>%
    distinct(Node) %>%
    arrange(Node) %>%
    pull()

## set initial list of nodes to select in UI
NodeListInit <- df_bootstrap_partial_points %>%
    distinct(Node) %>%
    filter(
        Node %in% c(
            "Bayesian statistics",
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
            "Transparency"
        )
    ) %>%
    pull()


# Figure 2

load("fig2a_data.RData")

# merge in bibliographic data
references <- readxl::read_xlsx("Classification Sheet - Reference.xlsx") %>% 
    clean_names() %>% 
    mutate(reference = str_replace(reference, 
                                   "Files\\\\{2}", "")) %>% 
    mutate(reference = str_replace_all(reference, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause problems in some people's systems
    mutate(across(, ~na_if(., "Unassigned"))) %>% 
    mutate(short_title = if_else(is.na(short_title), title, short_title)) %>% 
    mutate(doi_html = paste0("<a href=\"https://doi.org/", doi, '/">', doi, '</a>')) %>% 
    mutate(url_html = paste0("<a href=\"", url, '/">', url, '</a>')) %>% 
    select(reference, author, doi, doi_html, date, reference_type, short_title, url, url_html, aa_audience, aa_clarification, aa_journalist, aa_skeptical, aa_terms)

#join with articles, create text for popup/infobox
df_CA_results_articles_3 <- df_CA_results_articles_2 %>% 
    mutate(audience = str_replace(audience, "[:punct:]", " ")) %>% 
    left_join(references, by = c("Name" = "reference"))  %>% 
    mutate(popup = paste0("<b>", short_title, "</b><br>",
                          author.y, "<br>",
                          ifelse(is.na(url), "", url_html), "<br>",
                          ifelse(is.na(doi), "", doi_html),"<br>",
                          "Dim. 1: ", round(Dim_1, 1), "<br>",
                          "Dim. 2: ", round(Dim_2, 1), "<br>",
                          "Audience: ", audience))


# functions
# take coordinates from plot_click and return text for output

create_output_articles <- function(x) {
    art_clicked <-
        nearPoints(
            df_CA_results_articles_3,
            x,
            threshold = 5,
            maxpoints = 1,
            addDist = T
        )
    art_clicked$popup
}

create_output <- function(x) {
    node_clicked <-
        nearPoints(
            df_CA_results_nodes,
            x,
            threshold = 15,
            maxpoints = 1,
            addDist = TRUE
        )
    art_clicked <-
        nearPoints(
            df_CA_results_articles_3,
            x,
            threshold = 15,
            maxpoints = 1,
            addDist = TRUE
        )
    toggle <-
        if (!is.null(node_clicked$Node)) {
            paste0(
                node_clicked$Node,
                "\n",
                "Dimension 1: ",
                node_clicked$Dim_1,
                "\n",
                "Dimension 2: ",
                node_clicked$Dim_2,
                "\n"
            )
        }
    else if (!is.null(art_clicked$Name)) {
        paste0(
            art_clicked$Name,
            "\n",
            "Dimension 1: ",
            art_clicked$Dim_1,
            "\n",
            "Dimension 2: ",
            art_clicked$Dim_2,
            "\n",
            "Audience: ",
            art_clicked$audience,
            "\n",
            "Author: ",
            art_clicked$author,
            "\n",
            art_clicked$popup
        )
    }
    else {
        return("")
    }
}


# Define UI for application that draws a histogram
ui <- navbarPage("Nelson et al",
                 tabPanel("Figure 5",
                          fluidPage(
                              # Application title
                              titlePanel("Figure 5"),
                              
                              # Sidebar with a slider input for number of bins
                              sidebarLayout(
                                  # Show a plot of the generated distribution
                                  mainPanel(plotOutput("fig5_Plot"),),
                                  sidebarPanel(
                                      checkboxGroupInput(
                                          "nodeSelect",
                                          "Choose which nodes you want to display",
                                          choices = NodeList,
                                          selected = NodeListInit
                                      ),
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
                                      plotOutput("fig2_plot", height = 1000,
                                                 click = "plot_click"),
                                      p(
                                          "Click on any nodes or article points in the plot to get more information about them."
                                      )
                                  ),
                                  sidebarPanel(
                                      h3("Node Information"),
                                      textOutput("node_info"),
                                      h3("Article information"),
                                      uiOutput("article_info")
                                  )
                              )
                          ))))


server <- function(input, output) {
    output$fig5_Plot <- renderPlot({
        fig_5_plot(input$nodeSelect, input$axesToggle)
    })
    
    output$fig2_plot <- renderPlot({
        ggplot(df_CA_results_articles_3, aes(Dim_1, Dim_2)) +
            theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            geom_hline(yintercept = 0,
                       linetype = 2,
                       color = "darkgrey") +
            geom_vline(xintercept = 0,
                       linetype = 2,
                       color = "darkgrey") +
            scale_color_viridis(discrete = TRUE, option = "D") +
            geom_point(aes(color = term)) +
            geom_point(
                data = df_CA_results_sup_var,
                shape = 25,
                color = "white",
                fill = "red",
                size = 3,
                aes(x = Dim_1, y = Dim_2)
            ) +
            geom_point(
                data = df_CA_results_nodes,
                aes(Dim_1, Dim_2, size = Contrib_1_2),
                shape = 22,
                fill = "lightgrey"
            ) +
            geom_text_repel(
                data = df_CA_results_sup_var,
                color = "red",
                aes(label = Name),
                point.padding = 0.25,
                box.padding = 0.5
            ) +
            geom_text_repel(
                data = subset(df_CA_results_nodes, Contrib_1_2 > 4),
                aes(label = Node),
                point.padding = 0.25,
                box.padding = 0.75
            ) +
            labs(
                size = "Contribution",
                color = "Terms",
                x = "Dimension 1: ‘Discipline’ (8.50%)",
                y = "Dimension 2: ‘Audience’ (7.73%)"
            ) +
            theme(legend.position = "bottom")
        
        
    })
    
    output$node_info <-
        renderText(create_output(input$plot_click))
    
    output$article_info <-
        renderText(create_output_articles(input$plot_click))
    
    output$hover_info <- renderText({
        cat("input$plot_hover:\n")
        str(input$plot_hover)
    })
    output$dblclick_info <- renderText({
        cat("input$plot_dblclick:\n")
        str(input$plot_dblclick)
    })
    output$brush_info <- renderText({
        cat("input$plot_brush:\n")
        str(input$plot_brush)
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
