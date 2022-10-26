# app.R

library(shiny)
library(ggplot2)

usage <- read.table("merge.txt", header=T, stringsAsFactors = F)
usage$Date <- paste(substr(usage$Date,1,4),
                    substr(usage$Date,5,6),
                    substr(usage$Date,7,8),
                    sep = "-")
usage$Date <- as.Date(usage$Date, "%Y-%m-%d")
colors <- c("#66C2A5", "#FFFFB3", "#8DA0CB", "#FDB462", "#A6D854", "#FFD92F", "#8DD3C7", "#BEBADA",
            "#E5C494", "#FC8D62", "#66C2A5", "#FB8072", "#E78AC3", "#80B1D3",  "#B3DE69", "#FCCDE5",
            "#D9D9D9", "#B3B3B3")[1:length(unique(usage$User))]
names(colors) <- unique(usage$User)

Total <- 658

# Define UI for app that draws a histogram
ui <- fluidPage(
  
  # App title
  titlePanel("Tang Lab quota usage on PKUHPC"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Include clarifying text ----
      helpText("Disk storage usage for Tang Lab on PKUHPC, only gpfs1 included."),
      helpText("Note: while the time series plot shows only the specified",
               "range of observations, the pie chart will always be based",
               "on lateset calculations."),
      
      # Input: Select account
      checkboxGroupInput("account", "Account:",
                   c("tangfuchou_cls" = "tangfuchou_cls",
                     "tangfuchou_coe" = "tangfuchou_coe",
                     "tangfuchou_pkuhpc" = "tangfuchou_pkuhpc",
                     "tangfuchou_test" = "tangfuchou_test"),
                   selected = c("tangfuchou_cls","tangfuchou_coe",
                                "tangfuchou_pkuhpc","tangfuchou_test")
                   ),
      
      # Input: Slider for range of date
      sliderInput("range",
                  "Dates:",
                  min = min(usage$Date),
                  max = max(usage$Date),
                  value=c(min(usage$Date),max(usage$Date))),
      
      # Button
      downloadButton("downloadData", "Download selecetd data (.csv)")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: time series
      plotOutput(outputId = "Lines"),
      
      plotOutput(outputId = "Pie")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  output$Lines <- renderPlot({
    ts_selected <<- usage[usage$Account %in% input$account,]
    ts_selected <<- ts_selected[ts_selected$Date >= input$range[1] &
                             ts_selected$Date <= input$range[2],]
    ggplot(ts_selected,aes(x=Date, y=Storage))+
      geom_point(aes(color=User))+
      geom_line(aes(color=User,group=User))+
      theme_bw()+theme(panel.grid = element_blank())+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_color_manual(values = colors[unique(ts_selected$User)])+
      ylab("Storage / TB")
    
  })
  
  output$Pie <- renderPlot({
    pie.data <- usage[usage$Date == max(usage$Date),]
    rownames(pie.data) <- pie.data$User
    
    users <- unique(pie.data$User)
    users.selected <- pie.data[pie.data$Account %in% input$account, ]$User
    users.other <- setdiff(users,users.selected)
    
    plot.data <- c(pie.data[users.selected,]$Storage,
                   sum(pie.data[users.other,]$Storage),
                   Total - sum(pie.data$Storage))
    plot.data <- data.frame("User" =  c(users.selected, "others", "free"),
                            "Storage" = plot.data)
    plot.data$User <- factor(plot.data$User, levels = plot.data$User)
    
    pie.color <- c(colors[users.selected], "gray80", "gray95")
    names(pie.color) <- c(users.selected, "others", "free")
    
    ggplot(plot.data,aes(x = 1, y = Storage, fill = User))+
      geom_bar(stat = "identity")+
      geom_text(aes(x = 1.5, label = round(Storage, 1)),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y",direction = -1)+
      scale_fill_manual(values = pie.color)+
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"))
    
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" |:","-",Sys.time()), "_selected.download.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(ts_selected, file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)

