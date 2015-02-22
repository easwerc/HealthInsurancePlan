#Health Insurance Plan

library(shiny)

shinyUI(navbarPage("Health Insurance Plan", 
  tabPanel("How to Use",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h2("Overview"),
          br(),
          p("This is a simple app that can be used to compare two health ",
            "insurance plans and make a decision on which plan to choose. ",
            "There are multiple variables that should be considered to make", 
            "a decision but for simplicity, only few highly significant ",
            "variables are used in this app."),
          br(),
          br(),
          img(src="HealthPlan.png", height=100, width=250),
          p(strong("Disclaimer:"), em("The prediction model and averages were ",
            "obtained with a very limited amount of health insurance releated ",
            "data that I obtained from my collegues. Also I had not considered ",
            "every influencing variable/factors. The health care need for everyones is unique. ",
            "So use this data, model and prediction output at your own discretion"))
        ),
        mainPanel(
          h1("Synopsis"),
          p("Most employers provide health insurance for their employees. ",
            "The insurance plan offered comes with multiple type of plans. Usually one with high ", 
            em("premium"), " but low ", em("deductible"),
            " and atleast one other with low ", em("premium"), " but with high ", 
            em("deductible."), " (Which is usually a HDHP compatiable with HSA.) "
          ),
          p("Employees need to choose one of these plan. They are left with the question,",
            strong("Which plan is best for me?")), 
          "Use the 'Compare Plans' tab to help with the comparison between the plans, ",
          "estimate the overall expenses and make an educated decision.",
          br(),
          h1("Compare Plans"),
          p("Use this tab page to compare two plans. You need to have the following data of each plan"),
          p("* ", strong("Deductible"), "- The Annual deductible amount"),
          p("* ", strong("OOP Maximum"), "- The Maximum Out-of-Pocket Limit amount"),
          p("* ", strong("Premium"), "- The Insurance Premium Amount and if its Bi-Weekly or Monthly"),
          "Get the below details from your claims report for last year or use the ",
          "'Predict Clinic Visits & Expenses' tab to estimate",
          p("* ", strong("Yearly Clinic visit"), "- The estimated number of clinic visit in a year"),
          p("* ", strong("Average Cost per visit"), "- The average cost per visit that includes ",
            "the amount paid by you and the insurance company"),
          br()
        )
      )
    )
  ),
  
  tabPanel("Compare Plans",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h2("Assumptions"),
          p("This page is for comparing two plans; hereafter refered as Plan A and Plan B.",
            "Following assumptions are made for each plan when making the comparison estimate."),
          h4("Plan A",em(" - This is the high premium, low deductible plan")),                 
          p("* For every Clinic visit, you are charged only the copay (~$30) and ",
            "rest is paid by the insurance company"),
          h4("Plan B", em(" - This is the low premium, high deductible plan")),
          p("* For every Clinic visit, you are charged then entire amount until the deductible is met"),
          br(),
          h4("Plan A & B"),
          p("* Insurance company covers 80% after deductible is met"),
          p("* Insurance company covers 100% after Out of Pocket Maximun is met"),
          br()
        ),
        mainPanel(
          fluidRow(
            column(6, h4("Enter the following data from each plan to be compared")),
            column(3, h4("Plan A")),   
            column(3, h4("Plan B"))
          ),
          fluidRow(
            column(6, br(),helpText("Insurance Premium")),
            column(3, numericInput("aPremium", label="", value=169.00)),   
            column(3, numericInput("bPremium", label="", value=81.00))
          ),
          fluidRow(
            column(6, br(),helpText("Premium Contribution is")),
            column(3, radioButtons("aContrib", label="",
                        choices=list("Bi-Weekly"=1, "Monthly"=2), selected=1)),
            column(3, radioButtons("bContrib", label="",
                        choices=list("Bi-Weekly"=1, "Monthly"=2), selected = 1))
          ),
          fluidRow(
            column(6, br(),helpText("Plan Year Deductiable")),
            column(3, numericInput("aDeduct", label="", value=1800.00)),   
            column(3, numericInput("bDeduct", label="", value=3000.00))
          ),
          fluidRow(
            column(6, br(),helpText("Out-of-Packet Maximum")),
            column(3, numericInput("aOOPMax", label="", value=5250.00)),   
            column(3, numericInput("bOOPMax", label="", value=5000.00))
          ),
          br(),
          h4("Enter the following information obtained from your last years",
            "claim report or use prediction tab to predict these values"),            
          fluidRow(  
            column(6, sliderInput("nClinicVisit", label="No of Clinic Visits:", 
                                  min=0, max=60, value=10)),
            column(6, sliderInput("nClinicCost", label="Average Cost per Visit:", 
                                  min=20, max=2000, value=255))
          ),
          hr(),
          h4("Plan A vs Plan B - Comparison Report",align="center"),
          fluidRow(
            column(6, ""),
            column(3, h4("Plan A")),   
            column(3, h4("Plan B"))
          ),
          fluidRow(
            column(6, "Total Premium payable for the year"),
            column(3, textOutput("aYearPremium")),   
            column(3, textOutput("bYearPremium"))
          ),
          fluidRow(
            column(6, "Estimated expenses for the Clinic visits for the year"),
            column(3, textOutput("aVisitExpenses")),   
            column(3, textOutput("bVisitExpenses"))
          ),
          fluidRow(
            column(6, "Estimated Amount payable by you for the above expenses"),
            column(3, textOutput("aYouPay")),   
            column(3, textOutput("bYouPay"))
          ),
          fluidRow(
            column(6, "Your total Health Care expenses for the year"),
            column(3, textOutput("aAllExpenses")),   
            column(3, textOutput("bAllExpenses"))
          ),
          br(),
          h4(textOutput("conclusion"), align="center")
        )
      )
    )
  ),
  
  tabPanel("Predict Clinic Visits & Expenses",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          helpText("Select the appropriate options below to Predict the no of visits and average cost per vist"), 
          selectInput("insWho", label="Who is insured?", 
                      choices = list("Employee Only"=1, "Employee + 1"=2,
                                     "Employee + 2"=3, "Employee+3 or more"=4), selected=3),
          numericInput("dep0Age", "Age of Employee", 30),
          conditionalPanel("input.insWho > 1",   
            numericInput("dep1Age", "Age of Dependent 1", 0)),
          conditionalPanel("input.insWho > 2",   
            numericInput("dep21Age", "Age of Dependent 2", 0)),
          conditionalPanel("input.insWho > 3",   
            numericInput("dep3Age", "Age of Dependent 3", 0)),
          selectInput("hlevel", label="Choose the one that best describes your family health level", 
                      choices = c("Visit Clinic only for preventive care"=1,
                                  "Visit Clinic for seasonal allergies"=2,
                                  "Have medical condition requiring regular Visit"=3),
                             selected=2),
          actionButton("goButton", "Go!")
        ),
        mainPanel(
          plotOutput('plot1'),
          verbatimTextOutput("predictText")
        )
      )
    )                        
  )
))