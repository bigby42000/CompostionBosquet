#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(xlsx)
library(DT)
library(shinyjqui)
library(shinyjs)




A <- readxl::read_xlsx("https://github.com/bigby42000/CompostionBosquet/tree/main/data/liste-essences-Tommy-V4.xlsx")

Compoalea=function(A,n) {
  b=c()
  i = 1
  while (i <= (n-length(A))){
    b = c(b,sample(A,1))
    i = i+1
    
  }
  Compo = sample(c(A,b))
  return(Compo)
}

Compobosquet=function(A,N){
  l=length(A)
  m=l%/%3
  Ap=A
  B=c()
  for (j in 1:m){
    b=sample(Ap,3)
    B=c(b,B)
    Ap=Ap[! Ap %in% b]}
  if (length(Ap)!=0)
  {bmp1=c(Ap,sample(A[!A %in% Ap],3-length(Ap)))
  B=c(bmp1,B)}
  if (length(B)/3 == N){return(B)}
  
  for (i in 1:(N-length(B)/3)){
    b=sample(A,3)
    B=c(b,B)
  }
  return (B)
}

CompoSB=function(S,A,N){
  B=c()
  Am=A
  m=length(Am)%/%2
  for (s in S){
    if (m>=1){
      a=sample(Am,2)
      B=c(B,c(s,a))
      Am=Am[!Am %in% a]
      m=length(Am)%/%2}
    else if (length(Am)==1) {
      B=c(B,c(s,sample(A[! A %in% Am],1),Am))
      Am=c()
      m=0
      
    }
    else {
      B=c(B,c(s,sample(A,2)))}
  }
  
  
  while (m>=1){
    a=sample(Am,2)
    B=c(B,c(sample(S,1),a))
    Am=Am[!Am %in% a]
    m=length(Am)%/%2
  }
  if (length(Am)!=0){
    B=c(B,c(sample(S,1),sample(A[! A %in% Am],1),Am))
    Am=c()
  }
  
  if (length(B)/3 == N){return(B)}
  for (i in 1:(N-length(B)/3)){
    S1=sample(S,1)
    A2=sample(A,2)
    B=c(B,c(S1,A2))
  }
  return (B)
}

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(id="pagenav",title = strong("Composition de bosquets"),
                         tabPanel("Accueil",
                                  tags$head(tags$style('h3 {color:#00a3a8;}'),tags$script(src = "modfr.js")),
                                  fluidRow( 
                                    column(6,
                                           h3(strong("Que trouverez-vous ici ?")),
                                           p("Un outil interactif pour composer des groupes de bosquets très peu coûteux dans des pelouses ou prairies.
                                    Vous pourrez aussi simuler la diversité finale."),
                                           
                                           h3(strong("Etes vous concerné ?")),
                                           p(strong("Collectivités") ,": vous désirez créer rapidement une canopée rafraichissante,
                                    en maximisant la surface finale sans augmenter votre budget « arbres »."),
                                           p(strong("Propriétaires de pelouses ") ,"(particuliers, entreprises) :
                                    vous pouvez créer un ou quelques bosquets, vous aussi !"),
                                           p(strong("Eleveurs") ,"(de volaille plein air :
                                  cette solution répond au besoin d’ombrage et de nourriture d’appoint.) :
                                    vous pouvez créer un ou quelques bosquets, vous aussi !"),
                                           p(strong("Opérateurs  ") ,"de paysages urbains (pépiniéristes, paysagistes etc) :
                                    proposez cette solution !"),
                                           
                                           h3(strong("Pourquoi ce simulateur de bosquets ?")),
                                           p("Une méthode phare de",strong("l'adaptation au réchauffement climatique"),"est l'augmentation de la surface boisée,
                                  les arbres apportant de nombreux services. Pourtant, les pelouses et prairies urbaines sont souvent vides.
                                  En effet, planter « classiquement » un arbre déjà grand est coûteux.
                                  Planter des jeunes plants revient moins cher, mais comment faire ?"),
                                           p("Certains plantent des \"milliers d'arbres\" en utilisant la méthode de création de forêts urbaines du japonais Miyawaki,
                                  où la densité en jeunes plants est très forte. Mais il ne faut pas",strong("confondre arbre et jeune plant")," :
                                  la surface finale boisée est très faible, pour un coût élevé !
                                  Pourtant, utiliser des jeunes plants mis en compétition favorise la sélection,
                                  utiliser de très nombreuses espèces favorise la diversité, travailler soigneusement le sol favorise la croissance,
                                  mais comment diminuer le coût ?"),
                                           p("Ceci nous a conduit à proposer en alternative de placer ces jeunes plants en petits bosquets accessibles aux humains et animaux via la pelouse,
                                  avec une canopée arborée finale importante.
                                    "),
                                           
                                           h3(strong("Qui propose ce simulateur, qui l’a amélioré ?")),
                                           p("L’unité BioForA d’INRAE, en collaboration avec d’autres unités de recherche et des utilisateurs potentiels.
                                    "),
                                           actionLink("liencredits", h4("Crédits"), onclick = JS("modfr()"))
                                    ),
                                    
                                    
                                    column(6,
                                           h3(strong("Comment créer vos bosquets ?")),
                                           p("Nous proposons",strong("plusieurs façons de répartir les jeunes plants"),
                                             "dans chaque bosquet. On note A1, A2 et A3 les plants de qualité ordinaire de 1 ou 2 ans,
                                  S les plants de qualité supérieure de 2 ans, comme par exemple un Starpot® des pépinières Lemonnier, et v1 v2 v3 et v4 les arbustes ou plantes vivaces.
                                  Un bosquet est composé de 4 arbustes ou plantes vivaces et de 3 espèces différentes d’arbres :
                                  soit A1, A2 et A3 avec 4 plants chacun,
                                  soit 1 plant S et A2, A3 avec 4 plants chacun. 

                                    "),
                                           br(),
                                           
                                           fluidRow(
                                             column(width=9,
                                                    p("Modèle Jade : 12 petits plants et un plant supérieur,
                                        de préférence placé à un angle si c’est un Starpot®,
                                        car le système racinaire sera plus puissant et plus rapide d’installation")),
                                             column(width=3,
                                                    imageOutput("ModelJade",height = "100"))),
                                           br(),
                                           
                                           fluidRow(
                                             column(width=3,
                                                    imageOutput("ModelRemy",height = "100")),
                                             column(width = 9,
                                                    p("Modèle Rémy : 16 petits plants disposés au hasard,
                                        ce qui permet d’acheter des paquets de plants qui ont une seule étiquette,
                                        celle du numéro ou nom du bosquet"))),
                                           br(),
                                           
                                           fluidRow(
                                             column(width=9,
                                                    p("Modèle Tommy : 16 petits plants disposés judicieusement (deux exemples en image),
                                        ce qui nécessite soit un étiquetage par espèce ou individuel,
                                        soit un planteur capable de reconnaître les plants de A1 A2 A3")),
                                             column(width=3,
                                                    imageOutput("ModelTommy",height = "100"))),
                                           
                                           
                                           p("Choisissez vos espèces d’arbres, d’arbustes ou plantes vivaces,
                                    la" ,strong("composition est ensuite optimisée"),"automatiquement ! ",strong("Comparez") ,"la diversité finale,
                                    après croissance et sélection des arbres dominants,
                                    avec une plantation où tous vos plants seraient rassemblés en une seule forêt."),
                                           br(),
                                           
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       actionButton("liencompo", "Je crée les bosquets"),
                                                       actionButton("liencomparaison", "Je compare les diversités"))
                                    )
                                  ),
                                  br(),
                                  fluidRow(column(width=2,
                                                  imageOutput("LogoINRAE")),
                                           column(width=2,
                                                  imageOutput("LogoAGROOF")),
                                           column(width=2,
                                                  imageOutput("LogoMUSEUM"))),
                                  
                         )
                         
                         ,
                         
                         # fill content for tab 2
                         tabPanel("Composition",
                                  fluidPage(sidebarLayout(
                                    sidebarPanel(
                                      
                                      fluidRow( 
                                        column(6,
                                               sliderInput("Nbb", label = paste("Nombre de bosquets"), min = 1, 
                                                           max = 100, value = 5),
                                               
                                               radioButtons("starpot", label = paste("Choisir le type de bosquet"),
                                                            choices = list("12 petits plants et un plus grand plant" = 1, "16 petits plants" = 2)),
                                               
                                               conditionalPanel(condition = "input.starpot == 1",
                                                                radioButtons("NbSC",label = paste("Nombre d'espèces d'arbre pour le grand plant"),
                                                                             choices = list("Nombre maximum suggéré"=1,"Au choix"=2)),
                                                                em(textOutput("ChoixESd")),
                                                                br()),
                                               
                                               
                                               
                                               conditionalPanel(condition = "input.NbSC ==2 & input.starpot == 1 ",
                                                                sliderInput("NbS",label = paste("Choix du nombre d'espèces d'arbre pour le grand plant"),min = 1,max =100,value=1
                                                                )),
                                               
                                               
                                               conditionalPanel(condition = "input.starpot == 1",
                                                                sliderInput("NbES", label = paste("Nombre d'espèces d'arbre pour les petits plants"), min = 2, 
                                                                            max = 2, value = 2,step = 2)),
                                               
                                               
                                               conditionalPanel(condition = "input.starpot == 2",
                                                                sliderInput("NbESS", label = paste("Nombre d'espèces d'arbre pour les petits plants "), min = 2, 
                                                                            max = 3, value = 3,step = 3)),
                                               
                                               sliderInput("Nba", label = paste("Nombre d'espèces d'arbuste"), min = 1, 
                                                           max = 1, value = 1,step = 1)
                                               
                                               
                                               
                                               
                                               
                                               
                                               
                                        ),
                                        
                                        column(6,
                                               
                                               
                                               
                                               radioButtons("HM", label = paste("Hauteur maximum finale souhaitée"), 
                                                            choices = unique(A$`Hauteur`)[-1],
                                                            selected ="20 m et plus" ),
                                               
                                               
                                               
                                               
                                               pickerInput("sol",label=paste("Conditions sol"), # sécheresse et Calcaire +-
                                                           choices = list(
                                                             "Résistant à la secheresse" = c("Tous","oui","non"),
                                                             "Calcaire +-" = c("Tous","acide","acide+neutre","neutre+basique","neutre","acide+neutre+basique")
                                                           )
                                                           ,
                                                           selected = c("Tous","Tous"),
                                                           multiple = TRUE,
                                                           options =  list("max-options-group"=1,
                                                                           "none-Selected-Text" = paste("Choisir au moins 2 conditions"))
                                               ),
                                               
                                               
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 1 &&  input.sol[1]",
                                                 pickerInput('ES', label=paste('Espèce(s) grand plant(s) '), 
                                                             choices ="",multiple=TRUE
                                                 )),
                                               uiOutput("AES"),
                                               
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 1 &&  input.sol[1]  ",
                                                 em(textOutput("ChoixES")),
                                                 br())
                                               
                                               ,
                                               
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 1 & input.ES !=-1 && input.sol[1]",
                                                 pickerInput('ESnd', label=paste('Autres espèces '), 
                                                             choices ="",multiple = TRUE
                                                 )),
                                               
                                               uiOutput("AESnd"),
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 1 &&  input.sol[1]",
                                                 em(textOutput("ChoixESnd")),
                                                 br()
                                               ),
                                               
                                               
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 2 && input.sol[1]",
                                                 pickerInput('ESS', label=paste('Espèce n°' ), 
                                                             choices = "",multiple = TRUE
                                                 )),
                                               
                                               uiOutput("AESS"),
                                               
                                               conditionalPanel(
                                                 condition = "input.starpot == 2 &&  input.sol[1]",
                                                 em(textOutput("ChoixESS")),
                                                 br()
                                               ),
                                               
                                               
                                               
                                               
                                               
                                               conditionalPanel(condition = "input.sol[1]",
                                                                pickerInput("Ea",label=paste("Espèces d'arbuste"), 
                                                                            choices =c("Mélange d'arbustes" ,A[A$`Hauteur`=="Moins de 5m",][[2]])
                                                                            ,multiple = TRUE,
                                                                            options =  list(
                                                                              "max-options" = 4, "noneSelectedText" = paste("Choisir au moins 1 espèce")))
                                                                
                                                                
                                               ),
                                               
                                               conditionalPanel(
                                                 condition = " input.sol[1]",
                                                 em(textOutput("ChoixEa")),
                                                 br()
                                               ),
                                               
                                               
                                               uiOutput("Affichage")
                                               
                                        )  
                                        
                                        
                                        
                                        
                                        
                                      )),
                                    mainPanel(
                                      
                                      conditionalPanel(condition=" input.sol[1] ",
                                                       strong("Liste des espèces respectant les conditions"),
                                                       DTOutput("PlotEint")
                                      ), 
                                      
                                      
                                      conditionalPanel(condition="input.go",
                                                       strong("Exemple de composition"),
                                                       fluidRow(
                                                         column(9,
                                                                tableOutput("distPlot")),
                                                         column(3,
                                                                actionBttn("ChangeNbP", "Changer le nombre de plants par espèce", width="100%",size="sm",style="simple",color="succes"))
                                                       ),
                                                       p("Cliquer ci-dessous pour télécharger le fichier XLS contenant mes choix et la simulation"),
                                                       downloadButton("downloadData", "Télécharger"),
                                                       tags$head(
                                                         tags$script(src = "bookmark_fr.js")
                                                       ),
                                                       bookmarkButton(label="Conserver sa composition", onclick = JS("bookmark_fr()"))
                                      )
                                      
                                    )
                                    
                                  )
                                  )),
                         
                         tabPanel(title="Comparaison"),
                         tabPanel("Toutes les epèces",
                                  DTOutput("dataE")),
                         tabPanel("Sources",
                                  p("Je sais aps"))
))

# Define server logic required to draw a histogram



server <- function(input, output,session) {
  
  urlBioForA <- a("BioForA", href="https://www6.val-de-loire.inrae.fr/biofora/")
  urlSANTI <- a("Frédérique Santi, ", href="https://www6.val-de-loire.inrae.fr/biofora/")
  urlMULLER <- a("Serge Muller, ", href="https://isyeb.mnhn.fr/fr/annuaire/serge-muller-2601 ")
  urlAGROOF <- a("Agroof, ", href="https://agroof.net/ ")
  urlCRETE <- a("Pépinières Crété  ", href="https://pepinieres-crete.fr/ ")
  urlLEMONNIER <- a("Pépinières Lemonnier   ", href="https://www.pepinieres-lemonnier.com/ ")
  urlWW <- a("Pépinières Wadel-Wininger  ", href="https://www.pepinieres-wadel-wininger.fr/ ")
  
  
  observeEvent(input$liencredits, {
    showModal(modalDialog(
      title="Crédits",
      tags$div(style = "text-align: justify;",
               h3("Conception"),
               tags$p("INRAE et ONF (UMR 0588),  ",urlBioForA),
               tags$ul(style = "padding-left: 15px;",
                       lapply(list(tagList(urlSANTI,"chercheuse"),
                                   "Antoine Carrere, BTS Gestion et protection de la Nature,
                                   Cours Diderot, Toulouse, stage 2021,
                                   Préparation de documents pour l’implantation de bosquets",
                                   "Jade Ruffier, L3 Lettres, Langues, Sciences Humaines, Université d’Orléans,
                                   stage 2022, Bosquets en interstices péri-urbains",
                                   "Rémy Margeritat, M1 Master 1 Agrosciences, Environnement, Paysage, Territoire,
                                   Forêt, stage 2023, Planter des poquets arborés permet-il d'accroître la canopée urbaine ?",
                                   "Tommy Mayeur, M1 Mathématiques appliquées, statistiques, stage 2023,
                                   Composition de bosquets d’arbres et arbustes sur pelouses via Rshiny"), tags$li)
               ),
               tags$p("Muséum national d’histoire naturelle (MNHN), Institut de systématique,
                      évolution, biodiversité (UMR 7205 ISYEB, CNRS, MNHN, SU, EPHE)  "),
               tags$ul(style = "padding-left: 15px;",
                       lapply(list(tagList(urlMULLER," professeur")), tags$li)
               ),
               tags$p(urlAGROOF," Société Coopérative et Participative, Bureau d'études spécialisé en Agroforesterie "),
               tags$ul(style = "padding-left: 15px;",
                       lapply(list("Fabien Liagre","XXX"), tags$li)
               ),
               h3("Betatests et conseils des utilisateurs "),
               tags$p("Pépinières forestières"),
               tags$ul(style = "padding-left: 15px;",
                       lapply(tagList(urlCRETE,urlLEMONNIER,urlWW), tags$li)
               ),
               tags$p("Collectivités"),
               tags$ul(style = "padding-left: 15px;",
                       lapply(list("Olivet","Département de l'Orme","Liège"), tags$li)
               )
               
      )
    ))
  })
  
  observeEvent(input$liencompo, {
    newvalue <- "Composition"
    updateTabsetPanel(session, "pagenav", newvalue)
  })
  
  observeEvent(input$liencomparaison, {
    newvalue <- "Comparaison"
    updateTabsetPanel(session, "pagenav", newvalue)
  })
  
  v  <- eventReactive(input$NbS,{
    if (input$NbSC==1){
      input$Nbb%/%2}
    else {
      input$NbS}
  })
  
  
  Hint <-eventReactive(input$HM,{
    Hm = input$HM
    Hmax= unique(A$`Hauteur`)[2]
    i=3
    while (! Hm %in% Hmax ) {
      Hmax = c(Hmax,unique(A$`Hauteur`)[i])
      i = i+1}
    Hmax
    
    
  })
  
  observe({
    
    
    updatePickerInput(session, "ES",
                      label = paste("Choisir au plus ",v()," espèce(s) des grand plant(s)"),
                      choices = c(unique(A[(A$'Sec toléré'== input$sol[1] | input$sol[1]=="Tous") & (A$'Calcaire +-' == input$sol[2] | "Tous" %in% input$sol[2]) & A$`Hauteur` %in%  Hint(),][[2]])[-1],"Autre"=1),
                      options =  list("max-options" = v(),maxOptionsText=paste("Limite atteinte"),noneSelectedText=paste("Choisir au moins 1 espèce"))
    )
    
    updatePickerInput(session, "ESnd",
                      label = paste("Choisir au plus",input$NbES," espèces d'arbre des petits plants"),
                      choices = c(unique(A[ (A$'Sec toléré'== input$sol[1] | input$sol[1]=="Tous") & (A$'Calcaire +-'== input$sol[2] | input$sol[2]=="Tous")  &  A$`Hauteur` %in% Hint(),][[2]])[-1],"Autre"=1),
                      options =  list("max-options" = input$NbES,maxOptionsText=paste("Limite atteinte"),noneSelectedText=paste("Choisir au moins 2 espèces "))
    )
    
    updatePickerInput(session, "ESS",
                      label = paste("Choisir au plus",input$NbESS," espèces d'arbre"),
                      choices = c(unique(A[ (A$'Sec toléré'== input$sol[1] | input$sol[1]=="Tous") & (A$'Calcaire +-'== input$sol[2] | input$sol[2]=="Tous") & A$`Hauteur` %in% input$HM,][[2]])[-1],"Autre"=1),
                      options =  list("max-options" = input$NbESS,maxOptionsText=paste("Limite atteinte"),noneSelectedText=paste("Choisir au moins 3 espèces"))
    )
    
    
    
    
    
  })
  
  observeEvent(input$Nbb,{updateSliderInput(session, "NbES",value=2,min = 2
                                            ,max=input$Nbb*2,step=1
                                            
  )
    updateSliderInput(session, "NbS",value=input$Nbb%/%2,min = 2
                      ,max=input$Nbb,step=1
                      
    )
    
    updateSliderInput(session, "NbESS",value=3,min = 3
                      ,max=input$Nbb*3,step=1
                      
    )
    
    updateSliderInput(session, "Nba",value=1,min = 1
                      ,max=input$Nbb*4,step=1
                      
    )
    
  }) 
  
  
  observeEvent(c(input$sol,input$HM),{    output$PlotEint <- renderDataTable(
    na.omit(A[(A$'Sec toléré'== input$sol[1] | input$sol[1]=="Tous") & (A$'Calcaire +-' == input$sol[2] | input$sol[2]=="Tous" ) & A$`Hauteur` %in% Hint(),]),
    options=list(lengthChange=FALSE,pageLength = 5,
                 order = list(7, 'desc'),
                 lengthChange= FALSE,
                 language = list(paginate = 
                                   list('next'="Suivant", 
                                        previous="Précédent"),
                                 search = "Rechercher",
                                 emptyTable= "Aucune donnée disponible dans le tableau",
                                 info= "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                                 infoEmpty = "Affichage de 0 à 0 sur 0 entrées"
                 ),
                 columnDefs = list(list(visible=FALSE, targets=c(0,1,7))))
  )
  
  
  
  
  })
  
  observeEvent(input$NbSC,{    updateSliderInput(session,"NbS",
                                                 max = input$Nbb,value=1,min=1)})
  
  
  
  observeEvent(input$Nba, { updatePickerInput(session ,"Ea",label=paste("Choisir au plus",input$Nba,"espèce(s) d'arbuste"), 
                                              choices = c("Mélange d'arbustes",A[A$`Hauteur`=="5 m et moins",][[2]]),
                                              options =  list("max-options" = input$Nba , maxOptionsText=paste("Limite atteinte"),"noneSelectedText" = paste("Choisir au moins 1 espèce"))
                                              
                                              
  )})
  
  
  dateheure <-eventReactive(input$go,{
    format(Sys.time(), "%d/%m/%y_%Hh%M")
  })
  
  Compo <- eventReactive(c(input$go,input$AES,input$AESnd),{
    if (input$starpot==1 ){
      if (! 1 %in% input$ES)
      { Starpot=input$ES}
      else {Starpot=c(input$ES[-length(input$ES)],input$AES)}
      if (! 1 %in% input$ESnd)
      { Esnd = input$ESnd}
      else {Esnd=c(input$ESnd[-length(input$ESnd)],input$AESnd)}
      Arbustes=input$Ea
      CA=Compoalea(Arbustes,4*input$Nbb)
      CE=CompoSB(Starpot,Esnd,input$Nbb)
      NbA=as.vector(table(CA))
      SE=c()
      EsndE=c()
      for (k in seq(from=1, to=length(CE), by=3)){
        SE=c(SE,CE[k])
        EsndE=c(EsndE,CE[k+1],CE[k+2])
      }
      
      Ns=as.vector(table(SE))
      Nesnd=as.vector(table(EsndE))*4
      NbT=c(Ns,Nesnd,NbA)
      E=c(Starpot,Esnd,Arbustes)
      
      LatinS=A[A$'Nom français' %in% input$ES,3][[1]]
      LatinSA=rep("",length(Starpot)-length(LatinS))
      
      LatinEsnd=A[A$'Nom français' %in% input$ESnd,3][[1]]
      LatinAEsnd = rep("",length(Esnd)-length(LatinEsnd))
      
      if ("Mélange d'arbustes" %in% input$Ea){
        Latina=c("Mixtio",A[A$'Nom français' %in% input$Ea,3][[1]])
      }
      else {
        Latina=A[A$'Nom français' %in% input$Ea,3][[1]]
      }
      
      
      El = c(LatinS,LatinSA,LatinEsnd,LatinAEsnd,Latina)
      
      
      
      Types=c(rep("Espèces arborées (1 plant par bosquet)",length(Starpot)),rep("Espèces arborées (4 plants par bosquet)",length(Esnd)),
              rep("Arbustes (1 à 4 plants par bosquet)",length(NbA)))
      list(
        
        data.frame(Types,Espèces_français = E,Espèces_latin = El,Nombre_plants = NbT),
        
        
        data.frame(Nom_simulation=rep(paste("Simul",dateheure()),input$Nbb),
                   Numéro=c(1:input$Nbb), Arbre1_x1 = CE[seq(1,length(CE),3)],
                   Arbre2_x4 = CE[seq(2,length(CE)+1,3)],Arbre3_x4 = CE[seq(3,length(CE)+2,3)],
                   Arbuste1_x1 = CA[seq(1,length(CA),4)],Arbuste2_x1 = CA[seq(2,length(CA)+1,4)],
                   Arbuste3_x1 = CA[seq(3,length(CA)+2,4)],Arbuste4_x1 = CA[seq(4,length(CA)+3,4)]))
    }
    else {
      if (1 %in% input$ESS)
      {Es=c(input$ESS[-length(input$ESS)],input$AESS)}
      else 
      {Es=c(input$ESS)}
      Arbustes=input$Ea
      CA=Compoalea(Arbustes,4*input$Nbb)
      CE=Compobosquet(Es,input$Nbb)
      NbA=as.vector(table(CA))
      NbE=as.vector(table(CE))*4
      NbT=c(NbE,NbA)
      E=c(Es,Arbustes)
      
      
      LatinES=A[A$'Nom français' %in% input$ESS,3][[1]]
      LatinAES= rep("",length(Es)-length(LatinES))
      
      if ("Mélange d'arbustes" %in% input$Ea){
        Latina=c("Mixtio",A[A$'Nom français' %in% input$Ea,3][[1]])
      }
      else {
        Latina=A[A$'Nom français' %in% input$Ea,3][[1]]
      }
      
      El = c(LatinES,LatinAES,Latina)
      
      
      Types=c(rep("Espèces arborées (4 plants par bosquet)",length(NbE)),rep("Arbustes (1 à 4 plants par bosquet)",length(NbA)))
      
      list(
        data.frame(Types,Espèces_français = E,Espèces_latin = El,Nombre_plants = NbT),
        
        data.frame(Nom_simulation=c(rep(dateheure(),input$Nbb)),Numéro=c(1:input$Nbb), Arbre1_x4 = CE[seq(1,length(CE),3)],
                   Arbre2_x4 = CE[seq(2,length(CE)+1,3)],Arbre3_x4 = CE[seq(3,length(CE)+2,3)],
                   Arbuste1_x1 = CA[seq(1,length(CA),4)],Arbuste2_x2 = CA[seq(2,length(CA)+1,4)],
                   Arbuste3_x3 = CA[seq(3,length(CA)+2,4)],Arbuste4_x4 = CA[seq(4,length(CA)+3,4)]))
      
    }})
  
  
  
  output$distPlot <- renderTable({
    Compo()[[1]]
    
    
  })
  
  
  
  
  ModalNbP <- function(failed =FALSE){
    modalDialog(
      fluidRow(
        pickerInput("NbP",label=paste("Quel nombre de plant voulez-vous changer ?"), 
                    choices = list("Choisir une espèce" = 0,paste(Compo()[[1]][,2],"/",Compo()[[1]][,1])),
                    options =  list(
                      "noneSelectedText" = paste("Choisir au moins 1 espèce"))),
        verbatimTextOutput("ShowP"),
        uiOutput("NbPS")),
      
      
    )
  }
  
  output$Nb <- renderUI({
    if (input$NbP ==0){
      sliderInput()
      
    }
    
  })
  
  
  output$ShowP <- renderText(input$NbP)
  
  observeEvent(input$ChangeNbP, {
    showModal(ModalNbP())
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Simul",dateheure(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(Compo()[[1]], file, sheetName="Composition totale", row.names = FALSE)
      write.xlsx(Compo()[[2]], file, sheetName="Compositon unique", row.names = FALSE,append=TRUE)
    }
  )
  
  
  
  
  output$dataE <- renderDT(A,options=list(language = list(paginate = 
                                                            list('next'="Suivant", 
                                                                 previous="Précédent"),
                                                          search = "Rechercher",
                                                          emptyTable= "Aucune donnée disponible dans le tableau",
                                                          info= "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                                                          infoEmpty = "Affichage de 0 à 0 sur 0 entrées",
                                                          zeroRecords ="Aucune entrée correspondante trouvée",
                                                          infoFiltered = "(filtrées depuis un total de _MAX_ entrées)"),
                                          pageLength = 18,
                                          lengthChange= FALSE,
                                          columnDefs = list(list(visible=FALSE, targets=c(1))))
                           
  )
  
  output$ModelJade <- renderImage({list(src="WWW/Model_Jade.png")
  },deleteFile = F)
  
  output$ModelRemy <- renderImage({list(src="WWW/Model_Remy.png")
  },deleteFile = F)
  
  output$ModelTommy <- renderImage({list(src="WWW/Model_Tommy.png")
  },deleteFile = F)
  
  
  output$LogoINRAE <- renderImage({list(src="WWW/Logo-INRAE.jpg",width="50%"
  )
  },deleteFile = F)
  
  output$LogoAGROOF <- renderImage({list(src="WWW/Logo-AGROOF.jpg",width="50%"
  )
  },deleteFile = F)
  
  output$LogoMUSEUM <- renderImage({list(src="WWW/Logo-Museum.jpeg",width="50%"
  )
  },deleteFile = F)
  
  
  
  output$AESS <- renderUI({
    if (1 %in% input$ESS && input$starpot==2){
      selectizeInput("AESS","Espèce(s) non décrite(s)",choices=NULL,options=list(create=TRUE,maxItems=input$NbESS-length(input$ESS)+1),multiple=TRUE)
    }
    
  })
  
  output$AES <- renderUI({
    if (1 %in% input$ES && input$starpot==1){
      selectizeInput("AES","Espèce(s) non décrite(s) : Grand plant",choices=NULL,options=list(create=TRUE,maxItems=v()-length(input$ES)+1),multiple=TRUE)
    }
    
  })
  
  output$AESnd <- renderUI({
    if (1 %in% input$ESnd && input$starpot==1){
      selectizeInput("AESnd","Espèce(s) non décrite(s) secondaire(s)",choices=NULL,options=list(create=TRUE,maxItems=input$NbES-length(input$ESnd)+1),multiple=TRUE)
    }
    
  })
  
  
  
  output$Affichage <- renderUI({
    if (input$starpot ==1) {
      if (length(input$Ea)>=1 & length(c(input$ES[! input$ES %in% 1 ],input$AES))>=1 & length(c(input$ESnd[! input$ESnd %in% 1 ],input$AESnd))>=2  ){
        actionButton("go", "Affichage", class = "btn-success")
      }
    }
    else if (input$starpot ==2) {
      if (length(input$Ea)>=1 & length(c(input$ESS[! input$ESS %in% 1 ],input$AESS))>=3 ){
        actionButton("go", "Affichage", class = "btn-success")
      } 
    }
  })
  
  
  output$ChoixES <- renderText({paste("Vous avez choisi",length(c(input$ES[! input$ES %in% 1 ],input$AES)),"espèce(s) d'arbre pour les grands plants ")})
  output$ChoixESd <- renderText({paste("Pour ce nombre de bosquets, nous suggérons",input$Nbb %/% 2,"espèce(s), pour faciliter les compositions ")})
  output$ChoixESnd <- renderText({paste("Vous avez choisi",length(c(input$ESnd[! input$ESnd %in% 1 ],input$AESnd)),"espèce(s) d'arbre pour les petits plants ")})
  output$ChoixESS <- renderText({paste("Vous avez choisi",length(c(input$ESS[! input$ESS %in% 1 ],input$AESS)),"espèce(s) d'arbre ")})
  output$ChoixEa <- renderText({paste("Vous avez choisi",length(c(input$Ea)),"espèce(s) d'arbuste ")})
  
  
  jqui_bookmarking()
}






# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "server")
