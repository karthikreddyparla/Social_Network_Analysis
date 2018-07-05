library(visNetwork)
library(dplyr)
library(igraph)
Server<-(function(input, output,session) {
# Reading data from uplaoded files 
  data <- reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = FALSE,
                   sep = ' '
    )
    names(df)[1]<- "from"
    names(df)[2]<- "to"
    print(str(df))
    return(df)
  })
  dataLink <- reactive({

    req(input$file2)

    df <- read.csv(input$file2$datapath,
                   header = FALSE,
                   sep = ' '
    )
    names(df)[1]<- "id"
    names(df)[2]<- "label"
    test<-(df)[2]
    df[2]<-df[1]
    df[3]<-test
    names(df)[3]<- "group"
    print(str(df))
     return(df)
  })
  
  # To get ramdom data from user input
  mindata <- reactive({
   data()[sample(nrow(data()), input$count), ]
  })
  mindataLink <- reactive({
    filter(dataLink(), id %in% c(mindata()$from, mindata()$to))
  })
# calculating frequency with with mails are sent 
    Senddata <- reactive({
    
    sender1<- data()%>%group_by(from)%>%summarise(Sent_Frequency = n())
    names(sender1)[1]<-"Sender"
    return(sender1)
 })
    # calculating frequency with with mails are received  
  Recivedata <- reactive({
    Reciver1<- data()%>%group_by(to)%>%summarise(Recive_Frequency = n())
    names(Reciver1)[1]<-"Reciver"
    return(Reciver1)
})
  # getting top senders and receivers 
  TopSender <- reactive({
    Senddata()%>%arrange(desc(Sent_Frequency))%>%select(Sender)%>%head(10)
  })
  TopReciver <- reactive({
    Recivedata()%>%arrange(desc(Recive_Frequency))%>%select(Reciver)%>%head(10)
  })
  
  output$TopSender<- renderText(TopSender())
  
  mylist <-as.list(c('layout_with_sugiyama','layout_nicely'))
  print(mylist)
  names(mylist) <- c('hierarchical','simple')
  output$ViewChange = renderUI({
    selectInput('ViewChange', 'change View', mylist)
  })
  
# Calculating Two hop records 
  TowHops <- reactive({
   
       oneHop<-data()%>%filter(from %in% input$SelectSender)%>%group_by(to)
       oneHop1<-data()%>%filter(to %in% input$SelectSender)%>%group_by(from)
       print(c(oneHop1))
       x<-c(oneHop)
       x1<-c(oneHop1)
       print(str(x))
       print(str(x1))
       x2<- x1[c(2,1)]
      inter<- filter(data(), from %in% c(x$to,x1$from,input$SelectSender))
       test21<-filter(inter,!to %in% c(x$to,x1$from, input$SelectSender) )
       inter1<- filter(data(), to %in% c(x1$from,x$to, input$SelectSender))
       test211<-filter(inter1,!from %in% c(x1$from,x$to, input$SelectSender))
       inter12<- inter1[c(2,1)]
       test212<- test211[c(2,1)]
       comb<- rbind(test21,test212)
       comb1<-filter(comb,!to %in% c(x$to,x1$from, input$SelectSender))
       
       comb1<- distinct(comb1,to,.keep_all = TRUE)
       total <- rbind(x,x2,comb1)
  return(total)
  })
  TowHoplink <- reactive({
    
    oneHop<-data()%>%filter(from %in% input$SelectSender)%>%group_by(to)%>%select(to)
    x<-c(oneHop)
    print(str(x))
    twoHop<-data()%>%filter(from%in%x$to )%>%group_by(to)%>%select(to)
    y<-c(twoHop)
    print(str(y))
    return(filter(dataLink(), id %in% c(TowHops()$to, TowHops()$from)))
      })
  
  output$details<- DT::renderDataTable(DT::datatable(TowHops(), options = list(
    lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
    pageLength = 10
    
  )))
  
  # output variable for sender and receiver list
  output$Senddata <- DT::renderDataTable(
    DT::datatable(
      Senddata(), options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10,searching = TRUE
      )
    )
  )
  output$ReciveData <- DT::renderDataTable(
    DT::datatable(
      Recivedata(), options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10
      )
    )
  )
 
  # observer that run once the input files are uploaded
  observeEvent(!(is.null(dataLink())  & is.null(data()) ), {
    
    #Calculating Degree Centrality,inDegree Centrality,betweenness for all ids
    plot1<-graph_from_data_frame(data(),vertices = dataLink())
    degreeCentrality <- c(degree(plot1,v=V(plot1)))
    inDegreeCentrality <- c(degree(plot1,v=V(plot1),mode = "in"))
    edgebetweenness<-c(betweenness(plot1, v = V(plot1), directed = FALSE, weights = NULL))
    print(NROW(edgebetweenness))
    print(edgebetweenness)
    dataupdated<- dataLink()
    dataupdated$DegreeCentrality<-degreeCentrality
    dataLinkUpdated<- data()
    dataupdated$inDegreeCentrality<-inDegreeCentrality
    dataupdated$betweenness<-edgebetweenness
    #Calculating top 10 Ids with highest Degree Centrality,inDegree Centrality,betweenness
    Topdegree <- reactive({
      dataupdated%>%arrange(desc(DegreeCentrality))%>%select(id)%>%head(10)
    })
    TopIndegree <- reactive({
      dataupdated%>%arrange(desc(inDegreeCentrality))%>%select(id)%>%head(10)
    })
    Topbetweenness <- reactive({
      dataupdated%>%arrange(desc(betweenness))%>%select(id)%>%head(10)
    })
    output$Group = renderUI({
      selectInput('SelectType', 'Top 10 of', c('Sender','Reciver','degree centrality','betweenness centrality','inDegree centrality'))
    }) 
    
 
    observeEvent(input$SelectType, {
   optiontest<- switch (input$SelectType, "Sender"=TopSender(), "Reciver"=TopReciver(), "degree centrality"=Topdegree(),"betweenness centrality"=Topbetweenness(), "inDegree centrality"=TopIndegree())
    
    output$columns = renderUI({
      selectInput('SelectSender', paste('select from top 10 of ' ,input$SelectType), optiontest)
    })
    })
    output$centrality1 <- DT::renderDataTable(
      DT::datatable(
        dataupdated, options = list(
          lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
          pageLength = 10
        )
      )
    )
  output$plottest<- renderVisNetwork({visNetwork( mindataLink(),mindata(), height = "100%", width = "100%")%>%
      visEdges(arrows = "to") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1,hover = TRUE)
      ) %>%      visIgraphLayout( layout = "layout_nicely", physics = FALSE,
                                  smooth = FALSE, type = "full", randomSeed = NULL,
                                  layoutMatrix = NULL)%>%
      visPhysics(stabilization = FALSE)%>%visLegend(position='right',main = "Departments") })
  
  
  deq<- merge(data(), dataLink(),by.x="from", by.y="id", sort = FALSE)
  names(deq)[3]<- "label1"
  names(deq)[4]<- "from1"
  deq<- merge(deq, dataLink(),by.x="to", by.y="id", sort = TRUE)
  names(deq)[6]<- "to1"
  deq1<-deq%>%select(from1,to1)%>%group_by(from1,to1)%>%summarise(count=n())
  names(deq1)[1]<-"DeptFrom"
  names(deq1)[2]<-"DeptTo"
  print(deq1)
  output$dept <- DT::renderDataTable(
    DT::datatable(
      deq1, options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10
      )
    )
  )
  
  top10Centrality<-Topdegree()
  names(top10Centrality)[1]<-"DegreeCentrality"
  top10Centrality$DegreeCentrality<-Topdegree()
  top10Centrality$inDegreeCentrality<-TopIndegree()
  top10Centrality$betweenness<-Topbetweenness()
  output$dept1 <- DT::renderDataTable(
    DT::datatable(
      top10Centrality, options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10
      )
    )
  )
  
  })
  #Ploting Two hop neighbor once the input id is selected 
  observeEvent(input$SelectSender, {
   
    output$Summary1<-renderText("Summary: 
1) 5 out of top 10 ids with high centrality are same.This means ids with the higher degree are more likely to have higher betweenness.So, understand clearly, we will compair ids that are not same.
2) Id with higher degree and in degree centrality has more one hop neighbors compair to id with higher betweenness centrality.
3) This states that person with this ids are very popular and has more information and authority.
4) Id with higher betweenness centrality has more two hop neighbors compair to id's with higher degree centrality.
5) This states that person with this ids influence the flow around the email system ")

    print(c(input$SelectSender))
  output$twohopPlot1 <-renderVisNetwork({visNetwork( TowHoplink(),TowHops(), height = "100%", width = "100%")%>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1)) %>%
      visIgraphLayout( physics = FALSE,layout = input$ViewChange,smooth = FALSE, type = "full", randomSeed = NULL,
      layoutMatrix = NULL)%>%visLegend(position='right',main = "Departments")
   
    })
  observe({
    nodes_selection <- input$SelectSender
    visNetworkProxy("twohopPlot1") %>%
      visSelectNodes(id = nodes_selection)
  })

  })
 
})

