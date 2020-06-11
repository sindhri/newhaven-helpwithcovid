#newhavenhelpwithcovid
#questions asked:
#number of users, total, by month, by date
#number of volunteers, total, by month, by date
#number of projects, total, by month, by date
#projects groups: by location, n_of_volunteers_needed, exposure level
#how many projects got volunteers and still need how many, a bar with n_signedup

library(shiny)
library(ggplot2)

source("helpers2.R")

report_list <- create_reports()
summary_table <- report_list[[1]]
monthly_report <- report_list[[2]]
projects_volunteers <- report_list[[3]]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("New Haven Help With Covid Database Analytics"),
    helpText("Data source:", (a("http://newhavenhelpwithcovid.com/",
                                href="http://newhavenhelpwithcovid.com/",
                                target="_blank"))),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange1", "Date range:",
                           start = first(summary_table$date),
                           end   = last(Sys.Date())),
            
     radioButtons("type_of_data", "Plot type:",
                 c("Plot Daily" = "",
                   "Plot Total" = "_acc"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Summary", 
                                 h4("Monthly Report (accumulative)"),
                                 tableOutput("monthly_report"),
                                 helpText("users: number of registered users"),
                                 helpText("paired_up: number of users paired with projects"),
                                 helpText("projects: number of registered projects"),
                                 helpText("times_volunteered: number of times all users have volunteered"),
                                 helpText("users_vounteered: number of users who volunteered"),
                                 helpText("projects_volunteered: number of projects that have volunteers"),
                                 br(),
                                 h4("Project Overview"),
                                 plotOutput("plot_projects_point")),
                        tabPanel("Plot", 
                                 h4("users"),
                                 plotOutput("plot_users_by_date"),
                                 h4("pair_with_projects"),
                                 plotOutput("plot_pair_with_projects_by_date"),
                                 h4("projects"),
                                 plotOutput("plot_projects_by_date"),
                                 h4("times_volunteered"),
                                 plotOutput("plot_times_volunteered_by_date"),
                                 h4("users_volunteered"),
                                 plotOutput("plot_users_volunteered_by_date"),
                                 h4("projects_volunteered"),
                                 plotOutput("plot_projects_volunteered_by_date"))
                        )
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$monthly_report <- renderTable({
        monthly_report
    }, align = 'c') 
    
    output$plot_projects_point <- renderPlot({
        p1 <- ggplot(data = projects_volunteers, 
                     aes(x = `Volunteers Needed`, y = total_volunteers, 
                         color = level_of_exposure, shape = location, size = `Volunteers Needed`))+
            geom_point(position = "jitter") + ylab("Total Volunteers")
        p1 + theme_classic(base_size = 20)
    })
    
    output$plot_users_by_date <- renderPlot({
        p2 <- ggplot(data = summary_table, aes_string(x = "date", 
            y = paste("`users", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "steelblue")
        p2 + theme_classic(base_size = 20) + xlim(input$daterange1[1], input$daterange1[2])
    })

    output$plot_pair_with_projects_by_date <- renderPlot({
        p3 <- ggplot(data = summary_table, aes_string(x = "date", 
          y = paste("`pair_with_projects", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "steelblue")
        p3 + theme_classic(base_size = 20) + xlim(input$daterange1[1], input$daterange1[2])
    })
    
    output$plot_projects_by_date <- renderPlot({
        p4 <- ggplot(data = summary_table, aes_string(x = "date", 
            y = paste("`projects", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "steelblue")
        p4 + theme_classic(base_size = 20) + xlim(input$daterange1[1], input$daterange1[2])
    })
    
    output$plot_times_volunteered_by_date <- renderPlot({
        p5 <- ggplot(data = summary_table, aes_string(x = "date", 
            y = paste("`times_volunteered", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "forestgreen")
        p5 + theme_classic(base_size = 20)+ xlim(input$daterange1[1], input$daterange1[2])
    })
    
    output$plot_users_volunteered_by_date <- renderPlot({
        p6 <- ggplot(data = summary_table, aes_string(x = "date", 
            y = paste("`users_volunteered", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "forestgreen")
        p6 + theme_classic(base_size = 20)+ xlim(input$daterange1[1], input$daterange1[2])
    })
    
    output$plot_projects_volunteered_by_date <- renderPlot({
        p7 <- ggplot(data = summary_table, aes_string(x = "date", 
                y = paste("`projects_volunteered", input$type_of_data, "`", sep = ""))) + 
            geom_bar(stat = "identity", fill = "forestgreen")
        p7 + theme_classic(base_size = 20)+ xlim(input$daterange1[1], input$daterange1[2])
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
