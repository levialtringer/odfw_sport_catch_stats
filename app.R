
# load necessary libraries:
library(shiny)
library(bslib)
library(plotly)
library(doBy)
library(hrbrthemes)
library(ggplot2)
library(scales)
library(stringr)

# pull in data (.RData used for efficient loading)
load("data/odfw.RData")

# trends plot:
time_series <- function(wb,species) {
  
  plot_df_1 <- subset(df, MONTH > 0)
  
  if (wb!="All Waterbodies") {
    plot_df_1 <- subset(plot_df_1, WB_NAME==wb)
  }
  
  if (!(species %in% c("All Species"))) {
    plot_df_1 <- subset(plot_df_1, SPECIES==species)
  } 
  
  plot_df_1 <- summaryBy(CATCH ~ YEAR, data = plot_df_1, keep.names = T, FUN = sum, na.rm = T)
  
  plot_df_1 <- plot_df_1 %>% arrange(YEAR)
  
  g1 <- ggplot() + 
    geom_line(data = plot_df_1, aes(x=YEAR, y=CATCH), color = "#257379", alpha = 0.9, linewidth = 1) + 
    theme_ipsum(base_family = "Arial", base_size = 12, axis_title_size = 12) + 
    ylab("Reported Number of Fish Caught\n") +
    xlab("\nYear") +
    scale_y_continuous(limits = c(0,max(plot_df_1$CATCH)), breaks = pretty_breaks(5), expand = c(0,0), labels = comma) +
    scale_x_continuous(breaks = seq(min(plot_df_1$YEAR), max(plot_df_1$YEAR), 4), expand = c(0.05,0.05)) + 
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(size = 1, colour = "black")) 
  
  g1 <- ggplotly(g1, height = 450) %>% layout(yaxis = list(title = list(text="Reported Catch",standoff=15)),
                                xaxis = list(title = list(text="Year", standoff=30)))
  
  g1[["x"]][["data"]][[1]][["text"]] <- paste0(g1[["x"]][["data"]][[1]][["x"]], ": ", 
                                               prettyNum(round(g1[["x"]][["data"]][[1]][["y"]], digits = 0), big.mark = ','))
  g1[["x"]][["layout"]][["yaxis"]][["tickvals"]] <- g1[["x"]][["layout"]][["yaxis"]][["tickvals"]][-1]
  g1[["x"]][["layout"]][["yaxis"]][["ticktext"]] <- g1[["x"]][["layout"]][["yaxis"]][["ticktext"]][-1]
  
  g1
  
}

# seasonality plot:
month_plot <- function(wb,species) {
  
  plot_df_2 <- subset(df, MONTH > 0)
  
  if (wb!="All Waterbodies") {
    plot_df_2 <- subset(plot_df_2, WB_NAME==wb)
  }
  
  if (!(species %in% c("All Species"))) {
    plot_df_2 <- subset(plot_df_2, SPECIES==species)
  } 
  
  plot_df_2 <- summaryBy(CATCH ~ MONTH + YEAR, data = plot_df_2, keep.names = T, FUN = sum, na.rm = T)
  plot_df_3 <- summaryBy(CATCH ~ MONTH, data = plot_df_2, keep.names = T, FUN = mean)
  plot_df_2$MONTH <- factor(month.abb[plot_df_2$MONTH], levels=month.abb)
  plot_df_3$MONTH <- factor(month.abb[plot_df_3$MONTH], levels=month.abb)
  
  g2 <- ggplot() +
    geom_line(data = plot_df_2, aes(x=MONTH, y=CATCH, group = YEAR), color = "grey20", alpha = 0.2, linewidth = 0.5) + 
    geom_col(data = plot_df_3, aes(x=MONTH, y=CATCH), fill = "#257379", alpha = 0.9) +
    theme_ipsum(base_family = "Arial", base_size = 12, axis_title_size = 12) + 
    scale_y_continuous(limits = c(0,max(plot_df_2$CATCH)), breaks = pretty_breaks(5), expand = c(0,0), labels = comma) +
    scale_x_discrete(expand = c(0,0)) + 
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(size = 1, colour = "black"))
  
  g2 <- ggplotly(g2, height = 450) %>% layout(yaxis = list(title = list(text="Reported Catch",standoff=15)),
                                xaxis = list(title = list(text="Month",standoff=30)))
  
  g2[["x"]][["data"]][[1]][["text"]] <- paste0(month.name[g2[["x"]][["data"]][[1]][["x"]]], 
                                               str_sub(g2[["x"]][["data"]][[1]][["text"]], -5,-1), ": ", 
                                               prettyNum(round(g2[["x"]][["data"]][[1]][["y"]], digits = 0), big.mark = ','))
  g2[["x"]][["data"]][[2]][["text"]] <- paste0(month.name[g2[["x"]][["data"]][[1]][["x"]]], " (Average): ",
                                               prettyNum(round(g2[["x"]][["data"]][[2]][["y"]], digits = 0), big.mark = ','))
  g2[["x"]][["layout"]][["yaxis"]][["tickvals"]] <- g2[["x"]][["layout"]][["yaxis"]][["tickvals"]][-1]
  g2[["x"]][["layout"]][["yaxis"]][["ticktext"]] <- g2[["x"]][["layout"]][["yaxis"]][["ticktext"]][-1]
  
  g2
  
}

# define UI ----
ui <- page_sidebar(
  
  # App title ----
  title = "Oregon DFW Salmon and Steelhead Catch Statistics",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    open = "desktop",
    bg = "#cecece",
    width = "360px",
    
    selectInput("wb", "Waterbody:", c("All Waterbodies", names(table(df$WB_NAME)))),
    selectInput("species", "Species:", c("All Species", names(table(df$SPECIES)))),
    
    tags$head(
      tags$style(HTML('
                                .selectize-input {
                                    white-space: nowrap;
                                }
                                .selectize-dropdown {
                                    width: 310px !important;
                                }'
      )
      )
    )
      
  ),
  
  # Main panel for displaying outputs ----
  navset_card_tab(
    title = textOutput("caption"),
    nav_panel("Trends", plotlyOutput("TSplot")),
    nav_panel("Seasonality", plotlyOutput("Mplot")),
    nav_panel("About", 
              HTML("<p>As an avid fly fisherman with Pacific Northwest roots, I found the 
                    <a href='https://www.dfw.state.or.us/resources/fishing/sportcatch.asp'>Oregon Department of Fish & 
                    Wildlife (ODFW) sport catch statistics</a> data, stored in cumbersome PDFs and CSVs, difficult to navigate.
                    To address this, I’ve developed this simple Shiny application that allows users to easily explore 
                    trends and seasonal patterns in reported salmon and steelhead catch statistics by waterbody. 
                    While this harvest data is unverified by ODFW, it offers 
                    valuable heuristics for new anglers seeking information on when and where to target these species.</p>"))
  )
  
)


# define server logic to select and plot data ----
server <- function(input, output) {
  
  wb_title <- reactive({
    paste(input$wb, " - ", input$species)
  })
  
  output$caption <- renderText({
    wb_title()
  })
  
  output$TSplot <- renderPlotly({
    time_series(wb=input$wb, input$species)
  })
  
  output$Mplot <- renderPlotly({
    month_plot(wb=input$wb, input$species)
  })
  
}

# run app
shinyApp(ui, server)
