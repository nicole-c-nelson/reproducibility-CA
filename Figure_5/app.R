
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


# Figure 3
## used to be figure 2a

load("fig2a_data.RData")

## merge in bibliographic data
### need to fetch exported Zotero database for publication name
zotero_ref <- read_csv("zotero_export.csv") %>% 
    clean_names() %>% 
    select(title, publication_title)

references <- readxl::read_xlsx("Classification Sheet - Reference.xlsx") %>% 
    clean_names() %>% 
    inner_join(zotero_ref, references, by = "title") %>%
    mutate(reference = str_replace(reference, 
                                   "Files\\\\{2}", "")) %>% 
    mutate(reference = str_replace_all(reference, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause problems in some people's systems
    mutate(across(, ~na_if(., "Unassigned"))) %>% #Nvivo has NA cells as "unassigned"
    mutate(short_title = if_else(is.na(short_title), title, short_title)) %>% 
    mutate(doi_html = paste0("doi:<a href=\"https://doi.org/", doi, '/">', doi, '</a>')) %>% 
    mutate(url_html = paste0("<a href=\"", url, '/">', url, '</a>')) %>% 
    select(reference, author, doi, doi_html, date, reference_type, publication_title, short_title, url, url_html, aa_audience, aa_clarification, aa_journalist, aa_skeptical, aa_terms)

#join with articles, create text for popup/infobox
df_CA_results_articles_3 <- df_CA_results_articles_2 %>% 
    mutate(audience = str_replace(audience, "[:punct:]", " ")) %>% 
    left_join(references, by = c("Name" = "reference"))  %>% 
    mutate(url = ifelse(!is.na(doi), NA, url)) %>% # if doi is present, set URL to NA
    mutate(popup_f3 = paste0("<b>", short_title, "</b><br>",
                          "<em>", publication_title, "</em><br>",
                          author.y, "<br>",
                          ifelse(is.na(url), "", url_html), "<br>",
                          ifelse(is.na(doi), "", doi_html),"<br>",
                          "Dimension 1: ", round(Dim_1, 2), "<br>",
                          "Dimension 2: ", round(Dim_2, 2), "<br>",
                          "Audience: ", audience),
           popup_f4 = paste0("<b>", short_title, "</b><br>",
                            "<em>", publication_title, "</em><br>",
                            author.y, "<br>",
                            ifelse(is.na(url), "", url_html), "<br>",
                            ifelse(is.na(doi), "", doi_html),"<br>",
                            "Dimension 1: ", round(Dim_1, 2), "<br>",
                            "Dimension 3: ", round(Dim_3, 2), "<br>",
                            "Audience: ", audience))

#create infobox variable for nodes:
df_CA_results_nodes <- df_CA_results_nodes %>% 
    mutate(popup_f3 = paste0("<b>",Node,"</b>",
                          "<br>",
                          "Dimension 1: ", 
                          round(Dim_1, 2),
                          "<br>",
                          "Dimension 2: ",
                          round(Dim_2, 2)
    ),
    popup_f4 = paste0("<b>",Node,"</b>",
                      "<br>",
                      "Dimension 1: ", 
                      round(Dim_1, 2),
                      "<br>",
                      "Dimension 3: ",
                      round(Dim_3, 2)
    ))

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
    art_clicked$popup_f3
}

create_output_articles_f4 <- function(x) {
    art_clicked <-
        nearPoints(
            df_CA_results_articles_3,
            x,
            threshold = 5,
            maxpoints = 1,
            addDist = T
        )
    art_clicked$popup_f4
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
        node_clicked$popup_f3
}

create_output_f4 <- function(x) {
    node_clicked <-
        nearPoints(
            df_CA_results_nodes,
            x,
            threshold = 15,
            maxpoints = 1,
            addDist = TRUE
        )
    node_clicked$popup
}


# Define UI with panels
ui <- navbarPage("Mapping the reproducibility crisis",
                 # tabPanel("Introduction",
                 #          fluidPage(
                 #              titlePanel("Mapping the discursive dimensions of the reproducibility crisis: A mixed methods analysis"),
                 #              sidebarLayout(
                 #                  mainPanel(p("Nicole C. Nelson, Kelsey Ichikawa, Julie Chung, and Momin M. Malik"),
                 #                            h3("Abstract"),
                 #                            p("To those involved in discussions about rigor, reproducibility, and replication in science, conversation about the “reproducibility crisis” appear ill-structured. Seemingly very different issues concerning the purity of reagents, accessibility of computational code, or misaligned incentives in academic research writ large are all collected up under this label. Prior work has attempted to address this problem by creating analytical definitions of reproducibility. We take a novel empirical, mixed methods approach to understanding variation in reproducibility discussions, using a combination of grounded theory and correspondence analysis to examine how a variety of authors narrate the story of the reproducibility crisis. Contrary to expectations, this analysis demonstrates that there is a clear thematic core to reproducibility discussions, centered on the incentive structure of science, the transparency of methods and data, and the need to reform academic publishing. However, we also identify three clusters of discussion that are distinct from the main body of articles: one focused on reagents, another on statistical methods, and a final cluster focused on the heterogeneity of the natural world. Although there are discursive differences between scientific and popular articles, we find no strong differences in how scientists and journalists write about the reproducibility crisis. Our findings demonstrate the value of using qualitative methods to identify the bounds and features of reproducibility discourse, and identify distinct vocabularies and constituencies that reformers should engage with to promote change."),
                 #                            p("The full text of the article is available", a("test", href = "test"))),
                 #                  sidebarPanel(p("Programming: Harald Kliems"))
                 #              )
                 #          )
                 #     
                 # ),
                 tabPanel("Figure 3",
                          (fluidPage(
                              # titlePanel("Correspondence analysis"),
                              sidebarLayout(
                                  mainPanel(
                                      p(
                                          "Click on any nodes or article points in the plot to get more information about them. Draw a rectangle and double-click in the plot area to zoom in."
                                      ),
                                      plotOutput("fig2_plot", height = 400,
                                                 click = "plot_click",
                                                 dblclick = "plot_dblclick",
                                                 brush = brushOpts(
                                                     id = "plot_brush",
                                                     resetOnNew = TRUE)),
                                      p(strong("Correspondence analysis biplot of 353 articles discussing reproducibility, analyzed for 29 themes."), "Articles that are close together have similar narrative profiles. The closer an article appears to the center of the plot, the more closely it resembles the mean profile for all articles. The further away a theme is from the origin, the more variation there is in how authors discuss that theme. The color of an article’s plotted point (a circle) indicates the main term used in the article, and the size of a theme’s plotted point (a square) represents the contribution of that theme to constructing the dimensions. The eight most contributing themes are labeled. Supplementary variables (not used to construct the dimensions) are labeled in red")
                                      
                                  ),
                                  sidebarPanel(
                                      h3("Node Information"),
                                      uiOutput("node_info"),
                                      h3("Article information"),
                                      uiOutput("article_info")
                                  )
                              )
                          ))),
                 tabPanel("Figure 4",
                          (fluidPage(
                              # titlePanel("Correspondence analysis"),
                              sidebarLayout(
                                  mainPanel(
                                      p(
                                          "Click on any nodes or article points in the plot to get more information about them. Draw a rectangle and double-click in the plot area to zoom in."
                                      ),
                                      plotOutput("fig4_plot", height = 400,
                                                 click = "plot_click",
                                                 dblclick = "plot_dblclick",
                                                 brush = brushOpts(
                                                     id = "plot_brush",
                                                     resetOnNew = TRUE)),
                                      p(strong("Correspondence analysis biplot depicting dimensions 1 and 3 of the analysis."), "Articles that are close together have similar narrative profiles. The closer an article appears to the center of the plot, the more closely it resembles the mean profile for all articles. The further away a theme is from the origin, the more variation there is in how authors discuss that theme. The color of an article’s plotted point (a circle) indicates the main term used in the article, and the size of a theme’s plotted point (a square) represents the contribution of that theme to constructing the dimensions. The eight most contributing themes are labeled. Supplementary variables (not used to construct the dimensions) are labeled in red")

                                  ),
                                  sidebarPanel(
                                      h3("Node Information"),
                                      uiOutput("node_info_f4"),
                                      h3("Article information"),
                                      uiOutput("article_info_f4")
                                  )
                              )
                          ))),
                 
                 tabPanel("Figure 5",
                          fluidPage(
                              # Application title
                              titlePanel("Figure 5"),
                              
                              # Sidebar with a main panel for the plot and a sidebar for selecting nodes
                              sidebarLayout(
                                  # Show a plot of the generated distribution
                                  mainPanel(plotOutput("fig5_Plot"),
                                            p(strong("Multiple factor analysis of bootstrap samples drawn a set of 353 articles on reproducibility."), "Shaded areas indicate the peeled convex hulls of points based on 1000 bootstrap replicates plus the original sample, showing an approximate 95% confidence region for each theme. Twenty-nine codes were included in the analysis. Colored squares indicate the position of each theme based on the original sample, and black squares indicate the position of each theme based on the overall bootstrap analysis.")),
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
                          ))

                 )


server <- function(input, output) {
    output$fig5_Plot <- renderPlot({
        fig_5_plot(input$nodeSelect, input$axesToggle)
    })
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
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
            theme(legend.position = "bottom") +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        
        
    })
    
    output$node_info <-
        renderText(create_output(input$plot_click))
    
    output$article_info <-
        renderText(create_output_articles(input$plot_click))

# figure 4 plot

output$fig4_plot <- renderPlot({
    ggplot(df_CA_results_articles_2, aes(Dim_1,Dim_3))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
    geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
    geom_point(aes(color = clust))+
    scale_color_viridis(discrete = TRUE, option = "D", direction = -1)+
    geom_point(data = df_CA_results_nodes, aes(Dim_1, Dim_3, size=Contrib_1_3), 
               shape = 22, 
               fill = "lightgrey",
               alpha = 0.8)+
    geom_text_repel(data = subset(df_CA_results_nodes, Contrib_1_3 > 1.9), 
                    aes(label = Node), point.padding = 0.25, box.padding = 0.5)+
    labs(size="Contribution", color="Cluster",
         x="Dim 1: Bench vs. statistical methods (8.50%)", 
         y="Dim 3: Variation vs. standardization (6.95%)")+
    theme(legend.position = "bottom") +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
}
)

output$node_info_f4 <-
    renderText(create_output_f4(input$plot_click))

output$article_info_f4 <-
    renderText(create_output_articles_f4(input$plot_click))
}

# Run the application
shinyApp(ui = ui, server = server)
