library(shiny)
library(reshape)
library(ggplot2)
library(dplyr)
library(shinyapps)
library(igraph)
library(tm)
library(slam)
library(stringr)
library(reshape2)
library(stargazer)
library(RColorBrewer)
library(wordcloud)
library(rvest)
library(stringdist)
library(networkD3)

NUM_MONTE_CARLO = 100

PrepText = function(text){
  dict = read.csv("inquireraugmented.csv")
  dictPos = subset(dict, dict$Positiv == "Positiv")
  dictPos = tolower(dictPos$Entry)
  dictNeg = subset(dict, dict$Negativ == "Negativ")
  dictNeg = tolower(dictNeg$Entry)
  
  #make Corpus
  textC <- Corpus(VectorSource(text))
  textC <- tm_map(textC, removePunctuation)
  
  #build TDM
  DataTDM = TermDocumentMatrix(textC)
  ColSumsWords = colSums(as.matrix(DataTDM))
  
  # compare to lexicon
  PosTDM = TermDocumentMatrix(textC, list(dictionary = dictPos))
  ColSumsPos = colSums(as.matrix(PosTDM))
  
  NegTDM = TermDocumentMatrix(textC, list(dictionary = dictNeg))
  ColSumsNeg = colSums(as.matrix(NegTDM))
  
  PosPercent = ColSumsPos/ColSumsWords 
  NegPercent = ColSumsNeg/ColSumsWords
  Sentiment = (PosPercent - NegPercent)
  Sentiment_absolute = ColSumsPos - ColSumsNeg
  Subjectivity = (PosPercent + NegPercent) #the higher, the more emotional
  Subjectivity_absolute = ColSumsPos + ColSumsNeg
  Opinion = abs(abs(ColSumsPos - ColSumsNeg)/(ColSumsPos + ColSumsNeg))
  
  
  res = data.frame(Pos_Cnt = ColSumsPos, Neg_Cnt = ColSumsNeg,
                   PosPercent,  NegPercent, Sentiment = Sentiment, Subjectivity, 
                   Sentiment_absolute,
                   Subjectivity_absolute, Opinion)
  return(res)
  
}

makeWordCloud = function(text, no_words){
  #make Corpus
  textC <- Corpus(VectorSource(text))
  textC <- tm_map(textC, removePunctuation)
  textC <- tm_map(textC, removeWords, stopwords("english"))
  #build TDM
  DataTDM = TermDocumentMatrix(textC)
  
  m <- as.matrix(DataTDM)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(15, "PuRd")
  pal <- pal[-(1:2)]
  
  
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=no_words, 
            random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  
}

scrapews = function(website){
  ws <- html(website)
  names = html_nodes(ws, xpath = '/html/body')
  html_text(names)
  
}

############################################################################################

                        #PayR source code

############################################################################################

shinyServer(function(input, output, session) {
 
  output$Project = renderText({
    input$Project
  })
  
  
 

  created = reactive({
    if(input$Create > 0){
      if (is.null(isolate(input$TMember))){
        return() 
      }
      TMembers <<- tolower(sort(unlist(strsplit(isolate(input$TMember), split = "\\s*,\\s*"))))
    }
    return(input$Create)
  })

  output$ui <- renderUI({
    if(created()) {
      checkboxGroup = checkboxGroupInput(
        "TMembers_dy",
        "Trip Members",
        choices = TMembers,
        selected = TMembers)
    }
  })

  
  observe({
    if(input$Create>0){
      #updateTabsetPanel(session, "PayR_Steps", selected = "Transactionpanel")
    }
  })
 
  Members = character(0)
  
  observe({
    if (!is.null(input$TMembers_dy)){
      print(Members)
      Members <<- paste(tolower(input$TMembers_dy), collapse= ",")
    }
    print(unique(Members))
  })



  
  #observe({
   #   if(input$CMember > 0){
    #    print(unlist(strsplit(input$CMember, split = "\\s*,\\s*")))
     #   Members <<- paste(Members, ",", tolower(paste(sort(unlist(strsplit(input$CMember, split = "\\s*,\\s*"))), collapse= ",")))
      #  }
    #}) 
  
 
  
  overview = data.frame(
    Project = rep("Easter Camping 2015", 4),
    Members = c("joe,simon,sivan","joe,sivan","joe,simon","joe,simon,sivan"), 
    SpentFor = c("Firewood", "Beer", "Powerbait", "Groceries"), Amount = c(50, 30, 10, 100), Payer = c("joe", "joe", "sivan", "simon"))
  CloseTo = data.frame(Group = character(0))
  
  added = reactive({
      if(input$Add > 0) {  
        print("pups")
        validate(
          need(grepl("^\\d+$", isolate(input$Amount)), "Please enter a number")
        )
        sumPary = overview
        print(overview)
        print(TMembers)
        sumPary = rbind(sumPary, data.frame(
          Project = as.character(isolate(input$Project)), 
          Members = as.character(isolate(Members)),
          SpentFor = as.character(isolate(input$SpentFor)),
          Amount = as.numeric(isolate(input$Amount)), 
          Payer = tolower(as.character(isolate(input$Payer))))) 
        
        
        overview <<- sumPary
        #names(overview) = c("Project", "Members", "SpentFor", "Amount", "Paid By")
        print(overview)
        overview
      }
      
      return(input$Add)
    
  })
  
  deleted = reactive({  
      if(input$Delete > 0){
        overview <<- overview[1:nrow(overview)-1,]
        overview
      } 
      
      return(input$Delete)
  })
  

  #resetted = reactive({  
  #  if(input$Reset > 0){
  #    overview <<- data.frame(Project = character(0), Members = character(0), 
  #                            SpentFor = character(0), Amount = numeric(0), Payer = character(0))
  #    overview
  #  }
  #  return(input$Reset)
  #})
  
  output$sumPary = renderDataTable({
    if(added()  | deleted() | created()){ #| resetted()){
      overview
    }
  })
 
  observe({
    if (input$Compute > 0) {
      #updateTabsetPanel(session, "PayR_Steps", selected = "Results")
    }
  })
  
  
ResultTable = reactive({
    if(input$Compute > 0){ #| input$Optimize > 0) {
      #print("bye")
      allmembers = paste(sort(unique(unlist(split(paste(subset(overview, tolower(Members) != "all")$Members, collapse = ","), ",")))), collapse = ",")
      overview$Members = ifelse(tolower(overview$Members) == "all", allmembers, as.character(overview$Members))
      
      for(i in 1:nrow(overview)){
        #print(overview$Members)
        Members = unlist(strsplit(as.character(overview$Members[i]), split = ","))
        for(Member in Members){
          overview = rbind(overview, data.frame(Project = overview$Project[i], Members = overview$Members[i], 
                                                SpentFor = overview$SpentFor[i], Amount = 0, Payer = Member))
        }
      }
      
    Payments.molten = overview
    
    GroupKlamuster = function(Payments){
      #print(Payments)
      Payments$AmountPP = sum(Payments$Amount)/length(unique(Payments$Payer))
      Payments$PaysGets = Payments$Amount - Payments$AmountPP
      return(Payments)
    }
    
    GroupSum = group_by(Payments.molten, Project, Members, Payer)
    GroupSum = summarize(GroupSum, Amount = sum(Amount))
    
    GroupSum = group_by(GroupSum, Members)
    GroupSum = do(GroupSum, GroupKlamuster(.))
    print(GroupSum)
    GroupSum = group_by(GroupSum, Payer)
    GroupSum = summarize(GroupSum, PaysGets = -(sum(PaysGets)))
    
    Payments = GroupSum
    print(Payments)
    HaveToPay = which(Payments$PaysGets > 0)
    WillReceive = which(Payments$PaysGets < 0)
    
    
    Scoring = lapply(as.character(CloseTo$Group), FUN = function(x){strsplit(x, "\\s*,\\s*")})
    
    counters = c()
    scores = c()
    allpayments = list()
    for (i in 1:NUM_MONTE_CARLO){
      #print(Payments)
      Payments$Pays = ifelse (Payments$PaysGets > 0, Payments$PaysGets, 0)
      Payments$Gets = ifelse(Payments$PaysGets < 0, Payments$PaysGets, 0)
      
      HaveToPay = sample(HaveToPay)
      WillReceive = sample(WillReceive)
      counter = 0
      score = 0
      payments = data.frame()
      for(Payer in HaveToPay){
        for(Receiver in WillReceive){
          TransferableSum = min(abs(Payments$Gets[Receiver]), Payments$Pays[Payer])
          Payments$Gets[Receiver] = Payments$Gets[Receiver] + TransferableSum
          Payments$Pays[Payer] = Payments$Pays[Payer] - TransferableSum #is negative
          if(TransferableSum != 0){
            TransactionScore = 5
            for(group in seq(along=Scoring)){
              groupmembers = Scoring[[group]][[1]]
              if(Payments$Payer[Payer] %in% groupmembers & Payments$Payer[Receiver] %in% groupmembers){
                TransactionScore = 1
              } 
            }
            #print(paste(Payments$Name[Payer], "pays $", TransferableSum, "to", 
            #           Payments$Name[Receiver], TransactionScore))
              
            payments = rbind(payments, data.frame(Payer = Payments$Payer[Payer], Receiver = Payments$Payer[Receiver], 
                                                  Sum = TransferableSum, TransactionScore = TransactionScore))
                               
            counter = counter + 1
            score = score + TransactionScore
          }
        }
      }
      
      counters = c(counter, counters)
      scores = c(score, scores)
      allpayments = rbind(list(payments),allpayments)
    }
    
    
    print(min(counters))
    print(min(scores))
    print(allpayments[[which(scores == min(scores))[1]]])
    return(allpayments[[which(scores == min(scores))[1]]])
    }
  })

output$Logic =  renderDataTable({
  ResultTable()
})


  
simpleGraph = renderPlot({
  df.g <- graph.data.frame(d = ResultTable(), directed = TRUE)
  return(plot(df.g, edge.color="#DB1865",
              edge.label = paste0("            ", round(ResultTable()$Sum)), 
              vertex.frame.color = "white",
              vertex.color = "#DB1865",
              vertex.size = 50, 
              vertex.label.cex = 1.5,
              vertex.label.color = "black",
              edge.label.cex = 1.5, 
              edge.arrow.size=0.5,
              edge.label.color="black"))
})

d3Graph = output$PaymentPlot = renderForceNetwork({
  Nodes = data.frame(ID = 0:(length(TMembers)-1), TMembers)
  LinksName = data.frame(SourceName = ResultTable()$Payer, TargetName = ResultTable()$Receiver, Value = round(as.numeric(ResultTable()$Sum)))
  Links = inner_join(LinksName, Nodes, by= c("SourceName" = "TMembers"))
  Links = rename(Links, Source = ID)
  print(Links)
  Links = inner_join(Links, Nodes, by= c("TargetName" = "TMembers"))
  Links = rename(Links, Target = ID)
  print(Links)
  Links$SourceName = NULL
  Links$TargetName = NULL
  forceNetwork(Links = Links, Nodes = Nodes, Source = "Source",
               Target = "Target", Value = "Value", NodeID = "TMembers", Group = "TMembers",
               linkDistance = 150,
               linkWidth = JS("function(d) { return (d.value/10); }"),
               legend=T, opacity = 0.5)
})

output$PaymentPlot = simpleGraph

output$CloseTo = renderPrint({
  if(input$AddGroup > 0){
    CloseTo <<- rbind(CloseTo, data.frame(Group = isolate(tolower(input$CloseTo))))
  }
  if(input$Delete2 > 0){
    print(CloseTo)
    CloseTo <<- data.frame(Group = CloseTo$Group[1:(length(CloseTo$Group)-1)])
    print(CloseTo)
  }
  return(CloseTo)
})


 # observe(
#    {
#      if(input$Send > 0){
#          auth = c("AKIAIC4WQSQLZB76IPQA" = "QBQhPuneRprOKkcXay4kNvz/Ozq0548e+RbnCCvF")
#          csvwriter = textConnection("csvwriter", "w")
#          write.csv(sumPary, csvwriter)
#          csvString = textConnectionValue(csvwriter)
#          addFile(I(csvString), "enedpayr", input$Project, auth = auth)
#      }
#    })
    
 

#############################################
 ###########   Mood - Scoring   ############
#############################################

output$WordCloud = renderPlot({
  data = isolate(input$UserText)
  if(input$Analyze > 0){
    if(grepl("http.*", data) == TRUE | grepl("www.*", data) == TRUE){
          if(!grepl("http.*", data)){
              data = paste("http://", data, sep = "")
          }
          data = scrapews(data) 
          print(paste("data:", data, sep = " "))
    }
    makeWordCloud(data, 50)
  }
}, bg = "#E6E6E6")

output$PosNegCount = renderPlot({
  data = isolate(input$UserText)
  data = tolower(data)
  if(input$Analyze > 0){
    data = PrepText(data)
    data = data.frame(name = c("positive", "negative"), count = c(data$Pos_Cnt[1], data$Neg_Cnt[1]))
    return(
      ggplot(data, aes(x = name, y = count)) + geom_bar(stat = "identity")
      )
  }
})

output$PosNegCount = renderPlot({
  data = isolate(input$UserText)
  data = tolower(data)
  if(input$Analyze > 0){
    data = PrepText(data)
    data = data.frame(name = c("positive", "negative"), count = c(data$Pos_Cnt[1], data$Neg_Cnt[1]))
    return(
      ggplot(data, aes(x = name, y = count)) + geom_bar(stat = "identity")
    )
  }
})

output$Sentiment = renderText({
  data = isolate(input$UserText)
  data = tolower(data)
  if(input$Analyze > 0){
    data = PrepText(data)
    return(data$Sentiment[1]) 
  }
})


})
