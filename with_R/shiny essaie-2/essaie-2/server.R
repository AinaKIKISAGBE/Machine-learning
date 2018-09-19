#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   # input$nom1
   # input$age1
  

  # renderplot() pour les graph et les sortir avec plotOutput()
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  # pour récupérer le nom et l'age, le convertir en texte 
  # pour qu'il puisse être affiché dans l'output de l'utilisateur
  # avec renderText()
  output$nom1_de_sortie <- renderText(input$nom1)
  output$age1_de_sortie <- renderText(input$age1)
  
  # récupérer le genre et le sortir dans un objet nommé "genre1_de_sortie"
  output$genre1_de_sortie <- renderText(input$genre1)
  
  # recuperer la valeur de la bare1
  output$bare1_de_sortie <- renderText(input$bare1)
  # pour ecrire un texte avec la variable et le mettre dans un objet:
  output$bare1_de_sortie_phrase <- renderText(paste("vous avez choisit la valeur : ",
                                                   input$bare1, sep=" " ))
  # pour ecrire un texte avec le couple de variable (intervalle) et le mettre dans un objet:
  output$bare2_couple_de_sortie_phrase <- renderText(paste("vous avez choisit la valeur : ",
                                                    input$bare2_couple, sep=" " ))
                                                    
  
  
  # afficher les selaction de la liste déroulante ou choix multiple de pays benin, france, canada et belgique
  output$liste_deroulante_choix_multiple_1_couple_de_sortie <- renderText(input$liste_deroulante_choix_multiple_1)

    
  
  
  
  
})
