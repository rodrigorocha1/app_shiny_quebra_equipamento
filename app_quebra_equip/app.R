#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(


    titlePanel("Probabilidade de Falha em Equipamentos"),
    fluidRow(
        column(6, 
               radioButtons("Opcao", label = "Selecione o cálculo", choices = list("Prob. Exata" = 1, "Menos que " = 2, "Mais que" = 3), selected = 1)
               ),
        column(6, 
               numericInput("Ocorrencia", "Ocorrência Atual: ", value = 2, min = 1, max = 99),
               actionButton("Processar", "Processar")
               )
    ),
    
    fluidRow(
        column(12,
               plotOutput("Graf")
               )
    )


   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$Processar,{
        
        lamb = input$Ocorrencia
        
        tipo = input$Opcao
        
        inic = lamb - 2
        fim = lamb + 2
        
        
        if (tipo == 1) {
            x = dpois(inic:fim, lambda = lamb)
            tit = "Probalidade de Ocorrência"
        }
        
        if (tipo == 2) {
            x = ppois(inic:fim, lambda = lamb)
            tit = "Probalidade de Ocorrência Menor que"
        }
        
        
        if (tipo == 3) {
            x = ppois(inic:fim, lambda = lamb, lower.tail = F)
            tit = "Probalidade de Ocorrência Maior que"
        }
        
        
        z = as.character(round(x, 4))
        y = as.character(inic:fim)
        
        lab = paste(y, "Prob: ", z)
        output$Graf = renderPlot({
            barplot(x, names.arg = lab, col = gray.colors(5), main = tit)
            box()
        })
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
