library(DT)
library(ggplot2)
function(input, output, session){
 
  #first menuItem
  #dataset
  output$dtT = renderTable(
    dico
  )
  
  
  #summary - sans la variable cible ...
  output$summarize = renderPrint(
    summary(dt)
  )
  
  #Second menuItem -- visualisation 
  
  
  #Inscrits
  output$inscrBox = renderValueBox({
    valueBox(dim(infoData(dt,input$annee))[1], "Inscrits", icon = icon("users"),color = "blue")
  })
  
  #admissibles
  output$admissBox = renderValueBox({
    valueBox(dim(infoData(dt %>% filter(ADMISSIBLE == "OUI"),input$annee))[1], "Admissibles", icon = icon("filter"),color = "yellow")
  })
  
  #admis
  output$admBox = renderValueBox({
    valueBox(dim(infoData(dt %>% filter(ADMIS == "OUI"),input$annee))[1], "Admis", icon = icon("thumbs-up" , lib="glyphicon"),color = "green")
  })
  
  #colleges et lycees representes
  output$colbox = renderValueBox({
    valueBox(colbox(dt,input$annee), "etablissements", icon = icon("list"), color = "red")
  })
  
  
  #candidatures feminines
  output$candfem = renderValueBox({
    valueBox(paste(candfem(dt, input$annee),"%"), "candidature feminines", icon = icon("pie-chart"),color = "orange")
  })
  
  #nombreChoix
  output$nombreChoix = renderValueBox({
    valueBox(nombreChoix(dt, input$annee), "Nombre de choix en moyenne",color = "purple")
  })
  
   #inter -- candidature international
  output$inter = renderPlot({
    maps
  })
  
  #plot1 -- barplot candidature H/F
  output$plot1 = renderPlot({
    plot1(dt,input$annee)
  })
  
  #plot2 --  boxplot des candidats aux concours
  output$plot2 = renderPlot({
    plot2(dt, input$annee)
  })
  
  #plot3 -- barplot meilleurs top3 ecole admis
  output$plot3 = renderPlot({
    plot3(dt,input$annee,input$etat)
  })
  
  #piechart--serie admis
  output$piechart = renderPlot({
    piechart(dt,input$annee, input$genre)
  })
  
  #tree-map des series
  output$treemap = renderPlot({
    treemap(dt,input$annee,input$etat)
  })
  
  #distribution des notes au test
  output$boxtest = renderPlot({
    boxtest(dt, input$annee)
  })
  
  #plot4 -- 
  output$plot4 = renderPlot({
    plot4(dt,input$annee)
  })
  
  #nuageA
  output$nuageA = renderPlot({
    nuageA(dt,input$annee)
  })
  
#item3 ----Prediction-----------------------------------------------------------
  data2 = reactiveValues()
  observeEvent(input$go,{
               data2$moyenneDossier = as.numeric(input$moyenneDossier)
               data2$anglais = as.numeric(input$anglais)
               data2$cg = as.numeric(input$cg)
               data2$cs = as.numeric(input$cs)
               
  
  newpredict = data.frame(moyenneDossier = data2$moyenneDossier,
                          anglais = data2$anglais ,
                          cg = data2$cg,
                          cs = data2$cs)
  
  data2$op = predict(reglog, newpredict, type="response")
  data2$op = reponse.modele(data2$op)
  })
  
  output$value = renderPrint({data2$op})

}