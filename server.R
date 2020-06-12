#### SERVER.R ####
mand<-read.csv("data/berg_full.csv", sep=',', header = T)

library(shiny)
library(shinythemes)
library(knitr)
library(httr)
library(fields)
library(psych)
library(dplyr)
library(PerformanceAnalytics)
library(caret)
library(e1071)
library(DT)
library(MASS)
library(stats)
library(klaR)
library(Morpho)
library(mda)
library(earth)
library(markdown)
library(rmarkdown)
library(HDMD)
library(cluster)
library(ggdendro)
library(randomForest)
library(randomForestSRC)
library(ggRandomForests)
library(ggplot2)
shinyServer(function(input, output) {
  # get the reference data from the selectize input
  refdata <- reactive({
    input$evaluate
    isolate({
      if(length(input$refsamp) == 0) return(NULL)
      switch(input$refsamp,
             "mandible" = mand,
             NULL)
    })

  })


  getdata<-reactive({
  input$evaluate
    filtereddata<-refdata()
    filtereddata<- filtereddata %>% filter(Group %in% c(input$group, input$group1, input$group2, input$group3)) %>% droplevels()
    return(filtereddata)



  })

  # elements is the newdata data.frame
  elements <- reactive({
    input$evaluate
    isolate({
      elements <- c()
      if(!is.na(input$GNI)) elements <- c(elements, "GNI" = input$GNI)
      if(!is.na(input$HML)) elements <- c(elements, "HML" = input$HML)
      if(!is.na(input$TML)) elements <- c(elements, "TML" = input$TML)
      if(!is.na(input$GOG)) elements <- c(elements, "GOG" = input$GOG)
      if(!is.na(input$CDL)) elements <- c(elements, "CDL" = input$CDL)
      if(!is.na(input$WRB)) elements <- c(elements, "WRB" = input$WRB)
      if(!is.na(input$XRH)) elements <- c(elements, "XRH" = input$XRH)
      if(!is.na(input$MLT)) elements <- c(elements, "MLT" = input$MLT)
      if(!is.na(input$MAN)) elements <- c(elements, "MAN" = input$MAN)
      if(!is.na(input$XDA)) elements <- c(elements, "XDA" = input$XDA)
      if(!is.na(input$TLM23)) elements <- c(elements, "TLM23" = input$TLM23)
      if(!is.na(input$CS)) elements <- c(elements, "CS" = input$CS)
      if(!is.na(input$LBM)) elements <- c(elements, "LBM" = input$LBM)
      if(!is.na(input$ARS)) elements <- c(elements, "ARS" = input$ARS)
      if(!is.na(input$GF)) elements <- c(elements, "GF" = input$GF)
      if(!is.na(input$MT)) elements <- c(elements, "MT" = input$MT)
      if(!is.na(input$PREI)) elements <- c(elements, "PREI" = input$PREI)
      if(length(elements) == 0)  return(NULL)
      return(data.frame(as.list(elements)))
    })
  })

  ## create elements input table
  el_names <- c("<h4>Metric</h4>", "<h5>New Data</h5>")
  GNI <- c("GNI",
           "<input id='GNI' class='shiny-bound-input' type='number' value='NA' min='0' max='50'>"

  )

  HML <- c("HML",
           "<input id='HML' class='shiny-bound-input' type='number' value='NA' min='0' max='50'>"


  )

  TML <- c("TML",
           "<input id='TML' class='shiny-bound-input' type='number' value='NA' min='0' max='25'>"

  )

  GOG <- c("GOG",
           "<input id='GOG' class='shiny-bound-input' type='number' value='NA' min='0' max='150'>"

  )

  CDL <- c("CDL",
           "<input id='CDL' class='shiny-bound-input' type='number' value='NA' min='0' max='160'>"

  )

  WRB <- c("WRB",
           "<input id='WRB' class='shiny-bound-input' type='number' value='NA' min='0' max='100'>"

  )

  XRH <- c("XRH",
           "<input id='XRH' class='shiny-bound-input' type='number' value='NA' min='0' max='100'>"

  )

  MLT <- c("MLT",
           "<input id='MLT' class='shiny-bound-input' type='number' value='NA' min='0' max='150'>"

  )

  MAN <- c("MAN",
           "<input id='MAN' class='shiny-bound-input' type='number' value='NA' min='0' max='180'>"

  )

  XDA <- c("XDA",
           "<input id='XDA' class='shiny-bound-input' type='number' value='NA' min='0' max='100'>"

  )

  TLM23 <- c("TLM23",
           "<input id='TLM23' class='shiny-bound-input' type='number' value='NA' min='0' max='100'>"

  )


  output$el_table <- renderTable({
    data.frame(el_names, GNI, HML, TML, GOG, CDL, WRB, XRH, MLT, MAN, XDA, TLM23)

  }, sanitize.text.function = function(x) x, sanitize.rownames.function = function(x) x, sanitize.colnames.function = function(x) x, include.rownames = FALSE, include.colnames = FALSE)

  el_names1 <- c("<h4>Morphoscopic</h4>", "<h5>New Data</h5>")
  CS <- c("Chin Shape",
          "<input id='CS' class='shiny-bound-input' type='number' value='NA' min='1' max='4'>"

  )

  LBM <- c("LBM",
              "<input id='LBM' class='shiny-bound-input' type='number' value='NA' min='1' max='4'>"

  )

  ARS <- c("Ascending Ramus",
              "<input id='ARS' class='shiny-bound-input' type='number' value='NA' min='1' max='4'>"

  )

  GF <- c("Gonial Flare",
          "<input id='GF' class='shiny-bound-input' type='number' value='NA' min='1' max='5'>"

  )

  MT <- c("Mand. Torus",
          "<input id='MT' class='shiny-bound-input' type='number' value='NA' min='1' max='2'>"

  )

  PREI <- c("PREI",
            "<input id='PREI' class='shiny-bound-input' type='number' value='NA' min='1' max='4'>"

  )

  output$el_table1 <- renderTable({
    data.frame(el_names1, CS, LBM, ARS, GF, MT, PREI)

  }, sanitize.text.function = function(x) x, sanitize.rownames.function = function(x) x, sanitize.colnames.function = function(x) x, include.rownames = FALSE, include.colnames = FALSE)

  ## create reference data from new data
  refsamp <- reactive({
    if (is.null(getdata()) | is.null(elements())) return()
    ref <- dplyr::select_(getdata(), .dots = c("Group", names(elements()))) %>% droplevels()
    return(ref)
  })


  ## create lda model, plot, and typicality probabilities
  lda_mod <- eventReactive(input$evaluate, {
    lda_data<-na.omit(refsamp()) %>% droplevels()
    set.seed(42)
    ngroups<-nlevels(lda_data$Group)
    if(length(input$stepw) == 0) return(NULL)
    switch(input$stepw,
           "none" = {
            lda_formula<-as.formula(Group ~ .)
            enames<-elements()
           },
           "wilks" = {
    lda_formula1<-as.formula(Group ~ .)
    gw<-greedy.wilks(lda_formula1, data=lda_data, niveau=0.1)
    lda_formula<-gw$formula
    enames<-elements()[,gw$results[,1]]
           })

    if(length(input$meth)==0) return(NULL)
    switch(input$meth,
           "mda" = {
             if(length(input$numgroups) == 0) return(NULL)
             switch(input$numgroups,
                    "multigroup" = {
    model_group<-mda::mda(lda_formula, data=lda_data, keep.fitted = TRUE)
    estgroup<-data.frame(predict(model_group, elements()))
    df1v<-round((model_group$percent.explained[1]/100), digits=3)
    df2v<-round(((model_group$percent.explained[2]-model_group$percent.explained[1])/100), digits=3)
    groupprob<-predict(model_group, newdata=elements(), type="posterior")
    pp<-as.data.frame(round(groupprob, digits=3))
    p<-predict(model_group, lda_data, type="variates")
    Predicted<-predict(model_group, lda_data, type="class")
    ct<-table(lda_data$Group,Predicted)
    ct1<-model_group$confusion
    cm<-caret::confusionMatrix(ct1)
    con<-cm
    sum<-table(lda_data$Group)
    n<-as.vector(sum)
    classmat<-cbind(n, ct)
    tcc<-paste(sum(diag(ct)), "out of", sum(ct), "correct", "=", (100*(round(sum(diag(prop.table(ct))), digits=3))), "%", "Total Correct Classification")
    percenttab<-tcc
    ppv<-as.data.frame(con$byClass[,3]) ###here is the dimension error - if no comma 2 groups works, with comma more than 2 works - maybe an ifelse statement?
    ppn<-as.data.frame(con$byClass[,4])
    colnames(ppv)<-c("PPV")
    colnames(ppn)<-c("NPV")
    x<-p[,1]
    y<-p[,2]
    Group<-lda_data$Group
    df<-data.frame(Group,x,y)
    centroids<-aggregate(cbind(x,y)~Group,df,mean)
    cen<-as.matrix(centroids)
    estgroup1<-predict(model_group, elements(), type="variates")
    qx<-estgroup1[,1]
    qy<-estgroup1[,2]
    inddist<-data.frame(qx, qy)
    indie<-as.matrix(inddist)
    eucdist<-fields::rdist(cen[,2:3], indie)
    grouplev<-data.frame(levels(lda_data$Group))
    eucdist1<-cbind(grouplev, round(eucdist, digits=3))
    colnames(eucdist1)<-c("Group", "Dist.")
    ldaplot<-ggplot2::ggplot(data=df, aes(x, y, color=Group)) + geom_point(alpha=0.5) + labs(x="DF1", y="DF2") + geom_point(data=centroids, size=5) + geom_text(data=centroids, color="black", aes(label=Group, nudge_y= 1))+ geom_point(aes(x=qx, y=qy), size=6, col="black", pch=8) + stat_ellipse(type="norm", level=0.90)
           },
    "twogroup" = {
      model_group<-mda::mda(lda_formula, data=lda_data, keep.fitted = TRUE)
      estgroup<-data.frame(predict(model_group, elements()))
      df1v<-round((model_group$percent.explained[1]/100), digits=3)
      df2v<-round(((model_group$percent.explained[2]-model_group$percent.explained[1])/100), digits=3)
      groupprob<-predict(model_group, newdata=elements(), type="posterior")
      pp<-as.data.frame(round(groupprob, digits=3))
      p<-predict(model_group, lda_data, type="variates")
      Predicted<-predict(model_group, lda_data, type="class")
      ct<-table(lda_data$Group,Predicted)
      ct1<-model_group$confusion
      cm<-caret::confusionMatrix(ct1)
      con<-cm
      sum<-table(lda_data$Group)
      n<-as.vector(sum)
      classmat<-cbind(n, ct)
      tcc<-paste(sum(diag(ct)), "out of", sum(ct), "correct", "=", (100*(round(sum(diag(prop.table(ct))), digits=3))), "%", "Total Correct Classification")
      percenttab<-tcc
      ppv<-as.data.frame(con$byClass[3])
      ppn<-as.data.frame(con$byClass[4])
      colnames(ppv)<-c("PPV")
      colnames(ppn)<-c("NPV")
      x<-p[,1]
      y<-p[,2]
      Group<-lda_data$Group
      df<-data.frame(Group,x,y)
      centroids<-aggregate(cbind(x,y)~Group,df,mean)
      cen<-as.matrix(centroids)
      estgroup1<-predict(model_group, elements(), type="variates")
      qx<-estgroup1[,1]
      qy<-estgroup1[,2]
      inddist<-data.frame(qx, qy)
      indie<-as.matrix(inddist)
      eucdist<-fields::rdist(cen[,2:3], indie)
      grouplev<-data.frame(levels(lda_data$Group))
      eucdist1<-cbind(grouplev, round(eucdist, digits=3))
      colnames(eucdist1)<-c("Group", "Dist.")
      ldaplot<-ggplot2::ggplot(data=df, aes(x, y, color=Group)) + geom_point(alpha=0.5) + labs(x="DF1", y="DF2") + geom_point(data=centroids, size=5) + geom_text(data=centroids, color="black", aes(label=Group, nudge_y= 1))+ geom_point(aes(x=qx, y=qy), size=6, col="black", pch=8) + stat_ellipse(type="norm", level=0.90)
    })
    },

    "lda" = {
    if(length(input$numgroups) == 0) return(NULL)
    switch(input$numgroups,
           "multigroup" = {
    model_group<-MASS::lda(lda_formula, data = lda_data, prior= rep(1, ngroups)/ngroups)
    model_group1<-MASS::lda(lda_formula, data = lda_data, prior= rep(1, ngroups)/ngroups, CV=TRUE)
    tracetab<-prop.table(model_group$svd^2)
    df1v<-round((tracetab[1]), digits=3)
    df2v<-round((tracetab[2]), digits=3)
    estgroup<-data.frame(predict(model_group, newdata = elements(), type="class", CV=TRUE))
    groupprob<-predict(model_group, newdata=elements(), type="posterior", CV=TRUE)
    pp<-as.data.frame(round(groupprob$posterior, digits=3))
    p<-predict(model_group, lda_data, CV=T)
    ct<-table(lda_data$Group, p$class)
    ct1<-table(p$class, lda_data$Group)
    cm<-caret::confusionMatrix(ct1, reference = lda_data$Group)
    con<-cm
    n<-as.matrix(model_group$counts)
    colnames(n)<-c("n")
    classmat<-cbind(n, ct)
    tcc<-paste(sum(diag(ct)), "out of", sum(ct), "correct", "=", (100*(round(sum(diag(prop.table(ct))), digits=3))), "%", "Total Correct Classification Cross-validated")
    percenttab<-tcc
    ppv<-as.data.frame(con$byClass[,3])
    ppn<-as.data.frame(con$byClass[,4])
    colnames(ppv)<-c("PPV")
    colnames(ppn)<-c("NPV")
    x<-p$x[,1]
    y<-p$x[,2]
    Group<-lda_data$Group
    df<-data.frame(Group,x,y)
    centroids<-aggregate(cbind(x,y)~Group,df,mean)
    cen<-as.matrix(centroids)
    qx<-as.numeric(estgroup$x.LD1)
    qy<-as.numeric(estgroup$x.LD2)
    inddist<-data.frame(qx, qy)
    indie<-as.matrix(inddist)
    eucdist<-fields::rdist(cen[,2:3], indie)
    grouplev<-data.frame(model_group$lev)
    eucdist1<-cbind(grouplev, round(eucdist, digits=3))
    colnames(eucdist1)<-c("Group", "Dist.")
    ldaplot<-ggplot2::ggplot(data=df, aes(x, y, color=Group)) + geom_point(alpha=0.5) + labs(x="DF1", y="DF2") + geom_point(data=centroids, size=5) + geom_text(data=centroids, color="black", aes(label=Group, nudge_y= 1))+ geom_point(aes(x=estgroup$x.LD1, y=estgroup$x.LD2), size=6, col="black", pch=8) + stat_ellipse(type="norm", level=0.90)
           },
    "twogroup" = {
      model_group<-MASS::lda(lda_formula, data = lda_data, prior= rep(1, ngroups)/ngroups, na.action=na.omit)
      model_group1<-MASS::lda(lda_formula, data = lda_data, prior= rep(1, ngroups)/ngroups, CV=TRUE)
      tracetab<-prop.table(model_group$svd^2)
      df1v<-round((tracetab[1]), digits=3)
      df2v<-NA
      estgroup<-data.frame(predict(model_group, newdata = elements(), type="class", CV=TRUE))
      groupprob<-predict(model_group, newdata=elements(), type="posterior", CV=TRUE)
      pp<-as.data.frame(round(groupprob$posterior, digits=3))
      p<-predict(model_group, lda_data, CV=T)
      ct<-table(lda_data$Group, model_group1$class)
      ct1<-table(p$class, lda_data$Group)
      cm<-caret::confusionMatrix(ct1, reference = lda_data$Group)
      con<-cm
      n<-as.matrix(model_group$counts)
      colnames(n)<-c("n")
      classmat<-cbind(n, ct)
      tcc<-paste(sum(diag(ct)), "out of", sum(ct), "correct", "=", (100*(round(sum(diag(prop.table(ct))), digits=3))), "%", "Total Correct Classification Cross-validated")
      percenttab<-tcc
      ppv<-as.data.frame(con$byClass[3])
      ppn<-as.data.frame(con$byClass[4])
      colnames(ppv)<-c("PPV")
      colnames(ppn)<-c("NPV")
      x<-p$x
      Group<-lda_data$Group
      df<-data.frame(Group,x)
      centroids<-aggregate(cbind(x)~Group,df,mean)
      cen<-as.matrix(centroids)
      qx<-as.numeric(estgroup$LD1)
      qy<-NA
      inddist<-data.frame(qx)
      indie<-as.matrix(inddist)
      eucdist<-fields::rdist(cen[,2], indie)
      grouplev<-data.frame(model_group$lev)
      eucdist1<-cbind(grouplev, round(eucdist, digits=3))
      colnames(eucdist1)<-c("Group", "Dist.")
      ldaplot<-ggplot2::ggplot(data=df, aes(x=x, fill=Group)) + geom_histogram(position="dodge") + labs(x="Discriminant Function Score", y="Count") + geom_vline(aes(xintercept=qx))
    }
    )
    })
    return(list(model_group, estgroup,groupprob, p, ct, cm, ldaplot, qx, qy, pp, df1v, df2v, classmat, percenttab, tcc, ppv, eucdist1, ppn, enames, lda_data))

  })

  mahal<- eventReactive(input$evaluate, {
    a<-lda_mod()[[20]]
    group<-matrix(a$Group)
    group<-t(group[,1])
    variables<-lda_mod()[[19]]
    variables<-a[,-1]
    variables<-as.matrix(variables)
    mahala_sq<-pairwise.mahalanobis(x=variables, grouping=group)
    names<-rownames(mahala_sq$means)
    mahala<-sqrt(mahala_sq$distance)
    rownames(mahala) = names
    colnames(mahala)=names
    cluster<-agnes(mahala, diss=TRUE, keep.diss=FALSE,method="complete")
    dg<-as.dendrogram(cluster)
    dg1<-ggdendrogram(dg)
    dg2<-dg1 + labs(title="Mahalanobis Distance\nHierarchical Clustering") + theme(plot.title = element_text(hjust = 0.5))
    return(list(mahala, cluster, dg2))
  })
  
  classmatrix <- eventReactive(input$evaluate, {
    ctab<-lda_mod()[[5]]
    nclassmat<-lda_mod()[[13]]
    classperc<-(100*round(prop.table(ctab,1), digits=3))
    right<-sum(diag(ctab))
    of<-sum(ctab)
    totalcorrect<-100*(round(sum(diag(prop.table(ctab))), digits=3))
    return(list(nclassmat, classperc, right, of, totalcorrect))

    })

  tps <- eventReactive(input$evaluate, {
    tdat<-refsamp()
    sub<-na.omit(tdat)
    ngroups<-nlevels(tdat$Group)
    g<-sub$Group
    g<-as.vector(g)
    fit<-MASS::lda(Group ~., data=sub, prior= rep(1, ngroups)/ngroups)
    p<-predict(fit, sub)
    ref<-as.matrix(p$x[,1], p$x[,2])
    ind<-elements()
    est<-predict(fit, ind)
    pred<-as.matrix(est$x[,1], est$x[,2])
    typClass<-typprobClass(pred, ref, groups = g, method="chisquare", cv=TRUE, sep=T, robust="mcd")
    tp<-as.data.frame(round(typClass$probs, digits=3))
    return(list(tp))


  })

  gm <- eventReactive(input$evaluate, {
    samp<-refsamp()[, colnames(elements())]
    samp1<-refsamp()
    Group<-samp1$Group
    sampmean<-cbind(Group, samp)
    indiv<-elements()
    groupmean<-aggregate(.~ Group, sampmean, mean)
    groupnames<-groupmean$Group
    rgm<-round(groupmean[,-1], digits=2)
    rgm1<-cbind(groupnames, rgm)
    rgm1<-dplyr::rename(rgm1, Group = groupnames)
    try<-as.data.frame(mapply("*", rgm, 0))
    try1<-as.data.frame(mapply("+", try, as.data.frame(indiv))) ###use rgm to subract the matrix of the same size - test with other variables. get rid of all the n2-4 business.
    diff_df<-round(rgm - try1, digits=2)
    groupdiff<-cbind(groupnames, diff_df)
    groupdiff<-dplyr::rename(groupdiff, Group = groupnames)
    return(list(rgm1, groupdiff))
  })

  Tech <-reactive({
    input$tech
  })

  Case <-reactive({
    input$case
  })

statname<-eventReactive(input$meth,{
  a<-input$meth
  b<-ifelse(input$meth == 'lda', 'Linear Discriminant Analysis', 'Mixture Discriminant Analysis')
  return(list(a, b))
})

  TECH<-eventReactive(input$evaluate,{
    techie<-Tech()
    return(list(techie))
  })

  CASE<-eventReactive(input$evaluate,{
    case1<-Case()
    return(list(case1))
  })

  
  ##  summary statistics by group
  datasummary<-eventReactive(input$evaluate, {
    gd<-refsamp()
    gd<-na.omit(gd)
    groupsummary<-psych::describeBy(gd, group='Group')
    return(list(groupsummary))

  })
###output of name of classification statistic
  output$namestat <-renderText({
    paste(statname()[[2]])
  })
  
  ###Mahalanobis distance matrix output
  output$mahout<-renderPrint({
    mahal()[[1]]
  })
  ## output group classification
  output$lda_pred <- renderText({
    if(is.null(lda_mod())) return()
    a<-lda_mod()[[2]]
    paste("Predicted Group =", a[,1])
  })

  ## output group classification report
  output$lda_pred_rep <- reactive({
    if(is.null(lda_mod())) return()
    a<-lda_mod()[[2]]
    paste("Predicted Group =", a[,1])
  })
  
  ##output for model summary
  output$modsum <- renderPrint({
    if(is.null(lda_mod())) return()
    lda_mod()[[1]]

  })

  ## output posterior probabilities
  output$lda_prob <- renderPrint({
    if(is.null(lda_mod())) return()
    posteriors<- lda_mod()[[10]]
    print(posteriors[order(-posteriors[1,])], row.names=FALSE)
  })

  ## output typicality probabilities
  output$typs <- renderPrint({
    if(is.null(tps())) return()
    typsy<- tps()[[1]]
    print(typsy[order(-typsy[1,])], row.names=FALSE)
  })

  ##output distance from centroids
  output$cendist<-renderPrint({
    if(is.null(lda_mod())) return()
    distcen<-lda_mod()[[17]]
    print(distcen[order(distcen[,2]),], row.names=FALSE)
  })

## output confusion matrix
  output$confusionm<-renderPrint({
    if(is.null(classmatrix())) return()
    classmatrix()[[1]]
  })

  ## output percent confusion matrix
  output$confusionm1<-renderPrint({
    if(is.null(classmatrix())) return()
    classmatrix()[[2]]
  })

  ## output total correct classification
  output$confusionm2<-renderText({
    if(is.null(classmatrix())) return()
    paste(classmatrix()[[3]], "out of", classmatrix()[[4]], "=", classmatrix()[[5]],"%", "Total Correct Classification Cross-validated")
  })

  ## output positive predictive value
  output$confusionm3<-renderPrint({
    if(is.null(lda_mod())) return()
    pospred<-lda_mod()[[16]]
    round(pospred, digits=3)
  })

  ## output negative predictive value
  output$NPV<-renderPrint({
    if(is.null(lda_mod())) return()
    negpred<-lda_mod()[[18]]
    round(negpred, digits=3)
  })

  ## output summary statistics
  output$summarystat<-renderPrint({
    if(is.null(datasummary())) return()
    datasummary()[[1]]
  })

##render hierarchical cluster mahalanobis distance plot
  output$mahplot<-renderPlot({
    mahal()[[3]]
  })

  #scatterplot output
  output$ldaplot<- renderPlot({
  if(is.null(lda_mod())) return()
  lda_mod()[[7]]
  })

  #scatterplot output download report
  output$ldaplotrep<- reactive({
    if(is.null(lda_mod())) return()
    lda_mod()[[7]]
  })
  

  
 # New data LD scores
   output$number1 <- renderText({
    if(is.null(lda_mod())) return()
    ld1<-lda_mod()[[8]]
    ldv1<-lda_mod()[[11]]
    paste("Classified Individual's DF1 Score = ", round(ld1, digits=3), "Variation Accounted For in DF1:", round((ldv1*100), digits=2),"%")

  })

   # New data LD scores
   output$number2 <- renderText({
     if(is.null(lda_mod())) return()
     ld2<-lda_mod()[[9]]
     ldv2<-lda_mod()[[12]]
     paste("Classified Individual's DF2 Score = ", round(ld2, digits=3), "Variation Accounted For in DF2:", round((ldv2*100), digits=2),"%")

   })
   ## output model specs
   output$modelspec<-renderPrint({
     if(is.null(lda_mod())) return()
     lda_mod()[[1]]

      })

  ##case number
   output$casenum<- renderPrint({
     cake<-CASE()[[1]]
     print(as.name(cake), row.names=FALSE)
   })

   ##case number
   output$analyst<- renderPrint({
     tach<-TECH()[[1]]
     print(as.name(tach), row.names=FALSE)
   })

   ##output Title and Date
   output$title<-renderPrint({
     today<-Sys.Date()
     cat(sprintf('Sex and Ancestry Estimation Report %s\n', today))
   })

   ##output variables and measures for case
   output$elnamez<-renderPrint({
     e<-lda_mod()[[19]]
     print(e, row.names=FALSE)
   })

   ## output confusion matrix print
   output$confusionmp<-renderPrint({
     if(is.null(classmatrix())) return()
     classmatrix()[[1]]
   })

   ## output total correct classificationprint
   output$confusionm2p<-renderText({
     if(is.null(classmatrix())) return()
     paste(classmatrix()[[3]], "out of", classmatrix()[[4]], "=", classmatrix()[[5]], "%", "Total Correct Classification Cross-validated")
   })

   ## output percent confusion matrix print
   output$confusionm1p<-renderPrint({
     if(is.null(classmatrix())) return()
     classmatrix()[[2]]
   })

    ## output posterior probabilities print
   output$lda_probp <- renderPrint({
     if(is.null(lda_mod())) return()
     posteriors1<-lda_mod()[[10]]
     print(posteriors1[order(-posteriors1[1,])], row.names=FALSE)
   })


   ## output typicality probabilities print
   output$typsp <- renderPrint({
     if(is.null(tps())) return()
     typsy1<- tps()[[1]]
     print(typsy1[order(-typsy1[1,])], row.names=FALSE)
   })

   #scatterplot output print
   output$ldaplotp<- renderPlot({
     if(is.null(lda_mod())) return()
     lda_mod()[[7]]
   })

   ## output group classification
   output$ldapredp <- renderText({
     if(is.null(lda_mod())) return()
     a<-lda_mod()[[2]]
     paste("Predicted Group =", a[,1])
   })
   ##output of group means for print
   output$gmm <- renderPrint({
     if(is.null(gm())) return()
     print(gm()[[1]], row.names=FALSE)
   })

   ##output of difference of group means for print
   output$gmmdiff <- renderPrint({
     if(is.null(gm())) return()
     print(gm()[[2]], row.names=FALSE)
   })

   rf_n_trees <-reactive({
     as.numeric(input$rfntrees)
   })
   
   rfnvar <-reactive({
     as.numeric(input$rfvartest)
   })
   
   rfm<-eventReactive(input$evaluate1, {
     set.seed(32)
     rf_data<-refsamp() %>% droplevels()
     enames<-elements()
     rf_data$Group<-as.factor(rf_data$Group)
     rftrees<-rf_n_trees()
     rf_var_test<-rfnvar()
     withProgress(message = "Growing Random Forest Model",{
       rf<-randomForestSRC::rfsrc(Group ~., data=as.data.frame(rf_data), ntree=rftrees, mtry=rf_var_test, importance=TRUE, tree.err=TRUE, block.size = 1)
       rftarget<-"Group"
       enames1<-names(rf_data[,-1])
       nobs<-nrow(rf_data)
       rfsamp<-rftrain<-sample(nrow(rf_data), 0.7*nobs)
       rfvalidate<-sample(setdiff(seq_len(nrow(rf_data)), rftrain), 0.15*nobs)
       rftest<-setdiff(setdiff(seq_len(nrow(rf_data)), rftrain),rfvalidate)
       incProgress(0.1, detail = "Cross-Validation and Pruning")
       rf1<-randomForest::randomForest(Group ~ ., data=rf_data[rfsamp,c(enames1, rftarget)],ntree=rftrees, mtry=rf_var_test, importance=TRUE, na.action=randomForest::na.roughfix,replace=FALSE)
       pr<-predict(rf1, newdata=na.omit(rf_data))
       ct<-table(pr, rf_data$Group)
       cm<-caret::confusionMatrix(ct, reference = rf_data$Group)
       con<-cm
       incProgress(0.1, detail = "Almost There...")
       n<-as.matrix(summary(rf_data$Group))
       n1<-as.data.frame(n)
       colnames(n)<-c("n")
       classmat1<-cbind(n, ct)
       tcc<-paste(sum(diag(ct)), "out of", sum(ct), "correct", "=", (100*(round(sum(diag(prop.table(ct))), digits=3))), "%", "Total Correct Classification")
       percenttab1<-tcc
       ppv1<-cbind(n1,as.data.frame(ifelse(nlevels(as.factor(rf_data$Group)) > 2, as.data.frame(con$byClass[,3]), as.data.frame(con$byClass[3]))))
       ppn1<-cbind(n1,as.data.frame(ifelse(nlevels(as.factor(rf_data$Group)) > 2, as.data.frame(con$byClass[,4]), as.data.frame(con$byClass[4]))))
       colnames(ppv1)<-c("n","PPV")
       colnames(ppn1)<-c("n","NPV")
       rfclass<-predict(rf1, newdata=enames)
       probz<-predict(rf1, newdata=enames, type='prob')
       ggeplot<-plot(gg_error(rf))
       vi_plot<-plot(gg_vimp(rf))
     })
     return(list(classmat1,percenttab1, probz, rfclass, ggeplot, vi_plot, ppv1, ppn1, ct))
   })
       
rfclassmatrix <- eventReactive(input$evaluate1, {
     ctab1<-rfm()[[9]]
     classperc1<-(100*round(prop.table(ctab1,1), digits=3))
     right1<-sum(diag(ctab1))
     of1<-sum(ctab1)
     totalcorrect1<-100*(round(sum(diag(prop.table(ctab1))), digits=3))
     return(list(ctab1,classperc1, right1, of1, totalcorrect1))
})
 
## output total correct classificationprint RF
output$tccrf<-renderText({
  if(is.null(rfclassmatrix())) return()
  paste(rfclassmatrix()[[3]], "out of", rfclassmatrix()[[4]], "=", rfclassmatrix()[[5]], "%", "Total Correct Classification")
})

output$rmconm<-renderPrint({
  if(is.null(rfm())) return()
  rfm()[[1]]
})

output$rmconmperc<-renderPrint({
  if(is.null(rfclassmatrix())) return()
  rfclassmatrix()[[2]]
})

output$rfprob<-renderPrint({
  if(is.null(rfm())) return()
  p<-rfm()[[3]]
  p<-as.data.frame(round(p, digits=3))
  print(p[order(-p[1,])], row.names=FALSE)
})

output$rfpred <- renderText({
  if(is.null(rfm())) return()
  classp<-rfm()[[4]]
  paste("Predicted Group =", classp[1])
})

output$rfplot<- renderPlot({
  if(is.null(rfm())) return()
  print(rfm()[[5]], row.names=FALSE)
})

output$rfvimp<- renderPlot({
  if(is.null(rfm())) return()
  rfm()[[6]]
})

## output positive predictive value RF
output$RFPPV<-renderPrint({
  if(is.null(rfm())) return()
  pospred1<-rfm()[[7]]
  round(pospred1, digits=3)
})

## output negative predictive value RF
output$RFNPV<-renderPrint({
  if(is.null(rfm())) return()
  pospred2<-rfm()[[8]]
  round(pospred2, digits=3)
})
   })

