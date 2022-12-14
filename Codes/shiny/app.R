library(shiny)
library(bslib)
library(shinyvalidate)

#df = read.csv("df.csv")

# Define UI for application that calculates users' body fat
ui <- fluidPage(
    # UI theme
    theme = bs_theme(
        version = 4,
        bootswatch = "minty",
        base_font = font_google("Space Mono"),
        code_font = font_google("Space Mono")
    ),
    
    
    # Application title
    titlePanel(h1("Ice-cream Business Advisor", align = "center")),
    titlePanel(
        h5(
            "Our goal is to help you improve your current ice-cream business or start a successful ice-cream business (^_^)",
            align = "center"
        )
    ),
    br(),
    
    "Please identify you are already an ice-cream business owner or a prospective ice-cream business owner?",
    
    navbarPage(
        "Please choose:",
        
        tabPanel("I'm already an ice-cream business owner",
                 
                 sidebarLayout(
                     # Prompts for users' input
                     sidebarPanel(
                         
                         
                         "Please help us learn more about your current business ∩ˍ∩",
                         
                         br(),
                         br(),
                         
                         numericInput(
                             inputId = "current_star",
                             label = "What is your current star rating?",
                             value = NULL,
                             min = 0,
                             max = 5,
                             step = 0.1
                         ),
                         
                         checkboxGroupInput(
                             inputId = "current_types",
                             label = "What types of ice cream are you currently selling? ",
                             choices = list("Cone", "Shake", "Sundae", "Waffle", "Slush"),
                             selected = NULL
                         ),
                         
                         radioButtons(
                             inputId = "current_bike",
                             label = "Do you have bike-parking spots for your customers?",
                             choices = list("Yes", "No"),
                             selected = character(0)
                         ),
                         
                         div(
                             actionButton("current_submit", label = "Submit",
                                          style = "color: #fff; background-color: #76b5c5; border-color: #abdbe3;"),
                             align = "center"
                         ),
                     ),
                     
                     # Show the result
                     mainPanel(br(),
                               
                               tabsetPanel(
                                   type = "pills",
                                   
                                   tabPanel(
                                       "Summary",
                                       br(),
                                       textOutput("compare"),
                                       br(),
                                       textOutput("table_description"),
                                       tableOutput("effects"),
                                       br(),
                                       hr(),
                                       br(),
                                       verbatimTextOutput("inference")
                                   ),
                                   
                                   tabPanel(
                                       "Simulator",
                                       br(),
                                       strong("You can use this simulator to overview what your star rating will be if you adjust your menu \n"),
                                       br(),
                                       br(),
                                       checkboxGroupInput(
                                           inputId = "simulator_types",
                                           label = "Please select the ice-cream types you wanna have on your menu:",
                                           choices = list("Cone", "Shake", "Sundae", "Waffle", "Slush"),
                                           selected = NULL,
                                           inline = TRUE,
                                           width = 1000
                                       ),
                                       br(),
                                       div(
                                           actionButton("simulate", label = "Simulate",
                                                        style = "color: #fff; background-color: #76b5c5; border-color: #abdbe3;"),
                                           align = "center"
                                       ),
                                       br(),
                                       plotOutput("plot")
                                   )
                               ),)
                 ), ),
        
        
        
        tabPanel(" I wanna start up an ice-cream business",
                 sidebarLayout(
                     # Prompts for users' input
                     sidebarPanel(
                         "Please tell us a little bit about your business plan  ∩ˍ∩",
                         br(),
                         br(),
 
                         checkboxGroupInput(
                             inputId = "prospective_types",
                             label = "What types of ice cream will you supply?",
                             choices = list("Cone", "Shake", "Sundae", "Waffle", "Slush"),
                             selected = NULL
                         ),
                         
                         radioButtons(
                             inputId = "prospective_bike",
                             label = "Can you provide some bike-parking spots for your customers",
                             choices = list("Yes", "No"),
                             selected = character(0)
                         ),
                         
                         div(
                             actionButton("prospective_submit", label = "Submit",
                                          style = "color: #fff; background-color: #76b5c5; border-color: #abdbe3;"),
                             align = "center"
                         ),
                     ),
                     
                     # Show the result
                     mainPanel(br(),
                               
                               tabsetPanel(
                                   type = "pills",
                                   
                                   tabPanel(
                                       "Result",
                                       br(),
                                       "The estimated star rating of your new ice-cream business will be:",
                                       br(),
                                       br(),
                                       textOutput("prospective_estimation"),
                                       
                                       br(),
                                       hr(),
                                   ),
                                   tags$head(tags$style("#prospective_estimation{color: #eab676;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                                   tabPanel(
                                       "Suggestions",
                                       br(),
                                       verbatimTextOutput("prospective_suggestion")
                                   )
                               ),)
                 ), )
    ),
    
    # Sidebar for users to input their information
    
    br(),
    
    fluidRow(column(12,
                    br(),),
             style = "height:25vh;",),
    
    # Contact info
    fluidRow(
        column(
            12,
            br(),
            p(
                "If you have any question about this ice-cream business advisor app, please feel free to contact us by E-mail."
            ),
            "For questions or problems about this web app and its functions, please contact: ",
            strong("jwan23@wisc.edu"),
            br(),
            "For questions or problems about our data analysis and model, please contact: ",
            strong("jjia39@wisc.edu "),
            "| ",
            strong("zzhang2459@wisc.edu ")
        ),
        style = "background-color:#eeeee4; height:175px;",
    )
)

# Define server logic required to estimate users' star ratings and show commercial suggestions & recommendations
server <- function(input, output) {
    # Validate users' input
    current_iv = InputValidator$new()
    current_iv$add_rule("current_star", sv_required())
    current_iv$add_rule("current_star", sv_numeric())
    current_iv$add_rule("current_star", sv_between(0, 5))
    current_iv$add_rule("current_types", sv_required())
    current_iv$add_rule("current_bike", sv_required())
    
    prospective_iv = InputValidator$new()
    prospective_iv$add_rule("prospective_types", sv_required())
    prospective_iv$add_rule("prospective_bike", sv_required())
    
    simulate_iv = InputValidator$new()
    simulate_iv$add_rule("simulator_types", sv_required())
    
    # Click the button to get results for current business owner
    current_estimate = eventReactive(input$current_submit, {
        current_iv$enable()
        
        req(current_iv$is_valid())
        current_iv$disable()
        
        # Get the estimation
        cone = 0
        shake = 0
        sundae = 0
        waffle = 0
        slush = 0
        
        if ("Cone" %in% input$current_types) {
            cone = 1
        }
        if ("Shake" %in% input$current_types) {
            shake = 1
        }
        if ("Sundae" %in% input$current_types) {
            sundae = 1
        } 
        if ("Waffle" %in% input$current_types) {
            waffle = 1
        } 
        if ("Slush" %in% input$current_types) {
            slush = 1
        }
       
        estimate = 0.06 * cone - 0.14 * shake + 0.07 * sundae + 0.37 * waffle - 0.37 * slush + 3.87
        if (estimate > input$current_star) {
            result = "Your current star rating is lower than what we estimated."
        } else if (estimate < input$current_star) {
            result = "Your current star rating is higher than what we estimated."
        } else {
            result = "Your current star rating is the sames as what we estimated."
        }
        
        result
    })
    
    # Display the result
    output$compare = renderText({
        current_estimate()
    })
    
    # Click the button to get results for current business owner
    current_effects = eventReactive(input$current_submit, {
        current_iv$enable()
        
        req(current_iv$is_valid())
        current_iv$disable()
        
        # Get the effects
        effects = c()

        if ("Cone" %in% input$current_types) {
            effects[1] = "Cone"
        } else {
            effects[1] = ""
        }
        if ("Shake" %in% input$current_types) {
            effects[2] = "Shake"
        } else {
            effects[2] = ""
        }
        if ("Sundae" %in% input$current_types) {
            effects[3] = "Sundae"
        } else {
            effects[3] = ""
        }
        if ("Waffle" %in% input$current_types) {
            effects[4] = "Waffle"
        } else {
            effects[4] = ""
        }
        if ("Slush" %in% input$current_types) {
            effects[5] = "Slush"
        } else {
            effects[5] = ""
        }
        
        if ("Yes" %in% input$current_bike) {
            effects[6] = "Bike parking"
        } else {
            effects[6] = ""
        }
        if ("No" %in% input$current_bike) {
            effects[7] = "No bike parking"
        } else {
            effects[7] = ""
        }
        
        effects
    })
    
    output$table_description = renderText({
        req(is.vector(current_effects()))
        "The table below shows some factors that may have positive or negative effects on your ice-cream business:"
    })
    
    output$effects = renderTable({
        data.frame(Positive = c(current_effects()[1], current_effects()[3], current_effects()[4], current_effects()[6]), 
                   Negative = c(current_effects()[2], current_effects()[5], current_effects()[7], ""))
    }, align = "c")
    
    current_simulate = eventReactive(input$simulate, {
        current_iv$enable()
        simulate_iv$enable()
        
        req(current_iv$is_valid())
        req(simulate_iv$is_valid())
        current_iv$disable()
        simulate_iv$disable()
        
        # Get the estimation
        cone = 0
        shake = 0
        sundae = 0
        waffle = 0
        slush = 0
        
        if ("Cone" %in% input$current_types) {
            if (! "Cone" %in% input$simulator_types) {
                cone = -1
            }
        } else {
            if ("Cone" %in% input$simulator_types) {
                cone = 1
            }
        }
        
        if ("Sundae" %in% input$current_types) {
            if (! "Sundae" %in% input$simulator_types) {
                sundae = -1
            }
        } else {
            if ("Sundae" %in% input$simulator_types) {
                sundae = 1
            }
        }
        
        if ("Waffle" %in% input$current_types) {
            if (! "Waffle" %in% input$simulator_types) {
                waffle = -1
            }
        } else {
            if ("Waffle" %in% input$simulator_types) {
                waffle = 1
            }
        } 
        
        if ("Shake" %in% input$current_types) {
            if (! "Shake" %in% input$simulator_types) {
                shake = -1
            }
        } else {
            if ("Shake" %in% input$simulator_types) {
                shake = 1
            }
        } 
        
        if ("Slush" %in% input$current_types) {
            if (! "Slush" %in% input$simulator_types) {
                slush = -1
            }
        } else {
            if ("Slush" %in% input$simulator_types) {
                slush = 1
            }
        } 
        
        result = 0.06 * cone - 0.14 * shake + 0.07 * sundae + 0.37 * waffle - 0.37 * slush + input$current_star
        if (result > 5) {
            result = 5
        }
        result
    })
    
    output$plot = renderPlot({
        req(is.numeric(input$current_star))
        data = c(input$current_star, current_simulate())
        plot = barplot(data, names.arg = c("Current", "Simulated"), ylab = "Star rating", ylim = c(0,6), col = c("#abdbe3", "#eab676"), border = F)
        text(plot, data + 0.4, data, cex = 1)
    })
    
    # Click the button to get results for prospective business owner
    prospective_estimate = eventReactive(input$prospective_submit, {
        prospective_iv$enable()
        
        req(prospective_iv$is_valid())
        prospective_iv$disable()
        
        # Get the result and suggestions
        result = c()
        
        prospective_cone = 0
        prospective_shake = 0
        prospective_sundae = 0
        prospective_waffle = 0
        prospective_slush = 0
        
        suggestions = ""
        
        if ("Cone" %in% input$prospective_types) {
            prospective_cone = 1
        } else {
            suggestions = paste(suggestions, "* Supply of cone may slightly improve your star rating, so we recommend adding it to your menu.\n", sep = "\n")
        }
        if ("Shake" %in% input$prospective_types) {
            prospective_shake = 1
            suggestions = paste(suggestions, "* Supply of shake may negatively affect your star rating. Please pay more attention to customers' experience and reviews about this item.\n", sep = "\n")
        }
        if ("Sundae" %in% input$prospective_types) {
            prospective_sundae = 1
        } else {
            suggestions = paste(suggestions, "* Supply of sundae may slightly improve your star rating, so we recommend adding it to your menu.\n", sep = "\n")
        }
        if ("Waffle" %in% input$prospective_types) {
            prospective_waffle = 1
            suggestions = paste(suggestions, "* It's a great strategy to have waffle on your menu. Please keep investment in this popular ice-cream type.\n", sep = "\n")
        } else {
            suggestions = paste(suggestions, "* Waffle is a very popular ice-cream type, so we recommend adding it to your menu.\n", sep = "\n")
        }
        if ("Slush" %in% input$prospective_types) {
            prospective_slush = 1
            suggestions = paste(suggestions, "* Supply of slush may negatively affect your star rating. Please pay more attention to customers' experience and reviews about this item.\n", sep = "\n")
        }
        
        if ("No" %in% input$prospective_bike) {
            suggestions = paste(suggestions, "* Many customers like riding bikes to an ice-cream shop, so they may prefer the one with bike parking spots.\n  We recommend reconsidering to make some space for customers to park their bikes.\n", sep = "\n")
        }
        
        result[1] = 0.06 * prospective_cone - 0.14 * prospective_shake + 0.07 * prospective_sundae + 0.37 * prospective_waffle - 0.37 * prospective_slush + 3.87
        result[2] = suggestions
        result
    })
    
    # Display the result
    output$prospective_estimation = renderText({
        prospective_estimate()[1]
    })
    
    # Display suggestions
    output$prospective_suggestion = renderText({
        paste(prospective_estimate()[2], "\n")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
