start.qc <-
  function(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER", RESettings =list(REpar = "PLACEHOLDER"),SendMail = T, exitPath = NULL, BSAID = "P02769",serverinput = "in the html output or in the _RmqqcFile_Processed folder",rfn = NULL)
  {
    #/Users/henno/temp/mqqc/test/txt/evidence.txt
    require(data.table)
# PreScript -----
    #DataEvidence <- NULL
    require(tcltk)  
    #tk_choose.files(multi = F,caption = "select your evidence.txt",filters = matrix(c("Text",".txt","All files","*"),2,2,byrow = T))50  
    
    #reading data 
    # Peptides
    
    
    cat("\rLoading data",rep(" ",100))
    #    data(NameCounts)

    if(!is.null(DataEvidence)){
      if(is.vector(DataEvidence)){
        SourceTime <- file.info(DataEvidence)$ctime
        .path <- dirname(DataEvidence)
        .name <- basename(DataEvidence)

        
        
        evidence.path <- DataEvidence
        # tryError <- class(try(DataEvidence <- fread(DataEvidence,sep = "\t",stringsAsFactors = F,data.table = F,check.names = T,integer64 = "double")))
        tryError <- class(try(DataEvidence <- read.csv(DataEvidence,sep = "\t",stringsAsFactors = F,check.names = T)))
        
        if(file.exists(RP <- paste(dirname(dirname((.path))),"RawFileName.txt",sep = "/"))){
          rfn <- readLines(RP)
          rfn <-  gsub(".raw$","",rfn)
        }
      }else{
        .path <- getwd()
        .name <- "unknown"
      }	
    }else{
      
      library(tcltk)
      evidence.path <- file.choose()
      SourceTime <- file.info(evidence.path)$ctime
      # tryError <- class(try(DataEvidence <- fread(evidence.path,sep = "\t",colClasses = "character",stringsAsFactors = F,data.table = F,check.names = T,integer64 = "double")))
      tryError <- class(try(DataEvidence <- read.csv(evidence.path,sep = "\t",stringsAsFactors = F,check.names = T)))
      
      .path <- dirname(evidence.path)
      .name <- basename(evidence.path)

    }
    
   
    
    cat("\rData loaded",rep(" ",100))
    
    setwd(.path)
    dir.create(.folder <- paste("mqqc",Sys.Date(),sep = "_"))
    setwd(.folder)
    FolderPath <- getwd()
    
    if(tryError == "try-error"){
      Export <- "Name,System.Time.s,System.Time,msms.count,uniPepCount,ret.peak.shape.0%,ret.peak.shape.25%,ret.peak.shape.50%,ret.peak.shape.75%,ret.peak.shape.100%,ret.width.0%,ret.width.25%,ret.width.50%,ret.width.75%,ret.width.100%,ret.max,total.msms.min,quan.msms.min,mass.error.cal.0%,mass.error.cal.25%,mass.error.cal.50%,mass.error.cal.75%,mass.error.cal.100%,mass.error.uncal.0%,mass.error.uncal.25%,mass.error.uncal.50%,mass.error.uncal.75%,mass.error.uncal.100%,quan.duplicates,quan.duplicates.msms,score.0%,score.25%,score.50%,score.75%,score.100%,quanRetRSD,quanRetSlope.x,RatioIQuan.75%,quanRet50ratio.50%,Intensity.0%,Intensity.25%,Intensity.50%,Intensity.75%,Intensity.100%,missed.cleavages.percent,msmsQuantile.0%,msmsQuantile.25%,msmsQuantile.50%,msmsQuantile.75%,msmsQuantile.100%,msmsMassCount.0%,msmsMassCount.25%,msmsMassCount.50%,msmsMassCount.75%,msmsMassCount.100%,Coverage,msmsEff,TotalScore.MS,TotalScore.MSMS,TotalScore.LC,TotalScore.Total,TotalScoreColor.MS,TotalScoreColor.MSMS,TotalScoreColor.LC,TotalScoreColor.Total,exitPath,SourceTime,Status"
      Export <- unlist(strsplit(Export,","))
      TempInfo <- t(as.matrix(rep(NA,length(Export))))
      colnames(TempInfo) <- Export
      TempInfo[1,grep("exitPath",colnames(TempInfo))] <- paste(unlist(dirname(dirname(.path))),.name,sep = "/",collapse = "/")
      TempInfo[1,grep("^Name$",colnames(TempInfo))] <- basename(dirname(dirname(.path)))
      TempInfo[1,grep("^SourceTime$",colnames(TempInfo))] <- as.numeric(Sys.time())
      TempInfo[1,grep("^System.Time.s$",colnames(TempInfo))] <- as.numeric(Sys.time())
      TempInfo[1,grep("^System.Time$",colnames(TempInfo))] <- as.numeric(Sys.time())
      
      try(write.csv(TempInfo,paste(basename(dirname(dirname(.path))),".csv",sep = ""),quote = F,row.names = F))
      return(NULL)
    }
    
    
    
    raw.files 		<- grep("raw.file",tolower(colnames(DataEvidence)),)
    raw.files.str 	<- unique(DataEvidence[,raw.files])
    rep.v <- raw.files.str
    
    
    if(!is.na(n)){
      if(is.numeric(n)){
        rep.v <- raw.files.str[n]
      }
      if(is.character(n)){
        rep.v <- n
      }
    }
    
    
    
    list.collect <- list(length=length(rep.v))
    #### Test Preread Tables, should fasten readout

    check <- file.exists(peppath<- paste(.path,"peptides.txt",sep = "/"))
    if(check){
      cat("\rLoading peptides",rep(" ",100))
      Peptides <- fread(peppath,sep = "\t",data.table  = F,check.names = T,integer64 = "numeric") 
    }else{Peptides <- NULL}
    # AllPeptides 
    check <- file.exists(peppath<- paste(.path,"allPeptides.txt",sep = "/"))
    if(check){    
      cat("\rLoading AllPeptides",rep(" ",100))
      AllPeptides <- fread(peppath,sep = "\t",stringsAsFactors = F,data.table = F,check.names = T,integer64 = "numeric")
    }else{AllPeptides = NULL}
    
    # MSMS 
    check <- file.exists(peppath<- paste(.path,"msms.txt",sep = "/"))
    if(check){
     
      MSMS <- fread(peppath,stringsAsFactors = F,data.table = F,sep = "\t",header = T,check.names = T,integer64 = "numeric")
      
    }else{
      MSMS <- NULL
    }
    #MSMS Scans
    check <- file.exists(peppath<- paste(.path,"msScans.txt",sep = "/"))
    
    if(check){
      
      msScans <- fread(peppath,sep = "\t",stringsAsFactors = F,data.table = F,check.names = T,integer64 = "numeric")
      
    }else{
      msScans <- NULL
    }
    check <- file.exists(peppath<- paste(.path,"msmsScans.txt",sep = "/"))
    
    if(check){
      msmsScans <- fread(peppath,sep = "\t",stringsAsFactors = F,data.table = F,check.names = T,integer64 = "numeric")
    }else{
      msmsScans <- NULL
    }
    check <- file.exists(peppath<- paste(.path,"summary.txt",sep = "/"))
    if(check){
      file.copy(peppath,"summary.txt")
    }
    check <- file.exists(peppath<- paste(.path,"parameters.txt",sep = "/"))
    if(check){
      file.copy(peppath,"parameters.txt")
    }
    cat("MSMS",dim(MSMS),"ALLP",dim(AllPeptides),"Peptides",dim(Peptides))
    try(data(NameCounts) ) 
    # Protein Groups:
    check <- file.exists(peppath<- paste(.path,"ProteinGroups.txt",sep = "/"))
    
    if(check){
      cat("\rLoading ProteinGroups",rep(" ",100))
      PG <- fread(peppath,sep = "\t",data.table  = F,check.names = T,stringsAsFactors = F,integer64 = "numeric") 
    }else{Peptides <- NULL}
    
    SGFDR <- F
    
    try(hu <- match(strsplitslot(DataEvidence$Leading.Razor.Protein,1,"_"),names(FaNa)),silent = T)
    try(SGFDR <- length(is.na(hu)[!is.na(hu)])/dim(DataEvidence)[1] > 0.4,silent = T)
    if(SGFDR){
      newEvi <-subgroupFDRgenericDB(DataEvidence,0.05)
      DataEvidence <- DataEvidence[newEvi[,1] == 1,]
      EVIid <- paste(DataEvidence$Raw.file,DataEvidence$MS.MS.Scan.Number,DataEvidence$Type)
      MSMSid <- paste(MSMS$Raw.file,MSMS$Scan.number,MSMS$Type)
      AllPeptides$Type <- gsub("MULTI","MULTI-MSMS",AllPeptides$Type)
      # AllPid <- paste(AllPeptides$Raw.file,strsplitslot(AllPeptides$MSMS.Scan.Numbers,1,";"),AllPeptides$Type)
      # cut MSMS
      MSMSex <- match(EVIid,MSMSid)      
      MSMS <- MSMS[MSMSex,]
      # cut ALLpeps 
      AllPid <- paste(AllPeptides$Raw.file,strsplitslot(AllPeptides$MSMS.Scan.Numbers,1,";"),AllPeptides$Type)
      ALLex <- match(EVIid,AllPid)    
      ALLex <-  ALLex[!is.na(ALLex)] 
      AllPeptides$Sequence[ALLex] <- " "
      # cut msmsScans
      msmsScans$Type <- gsub("MULTI","MULTI-MSMS",msmsScans$Type)
      msmsScansID <- paste(msmsScans$Raw.file,msmsScans$Scan.number,msmsScans$Type)
      msmsScansex <- match(EVIid,msmsScansID)    
      msmsScansex <-  msmsScansex[!is.na(msmsScansex)] 
      msmsScans$Sequence[msmsScansex] <- " "
      msmsScans$Identified <- "-"
      msmsScans$Identified[msmsScansex] <- "+"
      
    }
    
    
    # ret <- as.numeric(msScans$Retention.time)
    # 
    # 
    # fun = max
    # MS1 <- aggregate(unfactor(msScans$Base.peak.intensity),list(createWindows(ret)),fun)
    # MS2 <- aggregate(unfactor(msmsScans$Base.peak.intensity),list(createWindows(unfactor(msmsScans$Retention.time)),msmsScans$Identified),fun)
    # MS1$x <- MS1$x/max(MS1$x)
    # MS2$x <- MS2$x/max(MS2$x)
    # plot(MS1$Group.1,MS1$x,type = "n",ylim = c(-1,1),ylab = "relative Intensity")
    # plotPOL(MS1$Group.1,MS1$x,col = "grey40",border = "transparent")
    # plotPOL(MS2$Group.1[MS2$Group.2 == "-"],MS2$x[MS2$Group.2 == "-"]*-1,col = "grey40",border = "transparent",fun = max) 
    # plotPOL(MS2$Group.1[MS2$Group.2 == "+"],MS2$x[MS2$Group.2 == "+"]*-1,col = "red",border = "transparent",fun = max) 

    if(length(rfn) > 0){
      DataEvidence$Raw.file <- rfn
      MSMS$Raw.file <- rfn
      msmsScans$Raw.file <- rfn
      msScans$Raw.file <- rfn
      AllPeptides$Raw.file <-rfn
      temp <- read.table("summary.txt",sep = "\t",stringsAsFactors = F,header = T,check.names = F)
      temp$`Raw file` <- rfn
      write.table(temp,"summary.txt",sep = "\t",quote = F,row.names = F)
      rep.v <- rfn
    }
   
  # final Script ------  
    msf <- ""
    if(length(rep.v)== 1){
      #msf <- list.files(dirname(dirname(.path)),pattern = "msfragger.txt",recursive = T,full.names = T)
      # msf <- paste(dirname(.path),"andromeda/msfragger.txt",sep = "/")
      # msf <- paste(dirname(.path),"andromeda/msfragger.txt",sep = "/")# older msfragger implementation
      # msfpath <<- .path
      msfpath <- paste(dirname(dirname(.path)),"temp.tsv",sep = "/")
      
      if(file.exists(msfpath)){
        er <-try(msf <- PrepareMSFres3(path = msfpath,alpha_pep = 0.01,alpha_prot = 0.05,workingpath = getwd(),pdfname = paste("MSF_",rep.v,".pdf",sep = "")))
        if(class(er) == "try-error"){
          msf <- NULL
        }
        if(length(msf) > 0){
          # try(plot(msf,show.plot = F,pdfname = paste("MSF_",rep.v,".pdf",sep = ""),name = paste("Mass Difference Results\n",rep.v)),silent = F)
          
        }
      }else{msf <- NULL}
      try(msfDat <- as.data.frame(msf$MassShifts[msf$MassShifts > 10]))
      try(write.table(msfDat,"MassDifference.txt",sep = "\t",quote = F,row.names = F))
    }
    i = rep.v
    at = 1
    for(i in rep.v){
      
      setwd(FolderPath)      
      # mqplotFun(paste(path.package("mqqc"),"data",sep = "/"),msapep=AllPeptides,mspep = Peptides,msprot = PG ,msev = DataEvidence,msscans = msScans,msmsscans = msmsScans)
      
      ####
      # Subset evidence
      ####
      
      temp.DataEvidence <- DataEvidence[as.character(DataEvidence[,raw.files]) ==as.character(i),]	
      # temp.DataEvidence <<- temp.DataEvidence
      # temp.DataEvidence <- temp.DataEvidence
      cat("\rstarting qc.prepare",rep(" ",100))
      
      try(tempAllPeptides <- AllPeptides[AllPeptides$Raw.file  == i,])
      try(tempMSMS <- MSMS[MSMS$Raw.file  == i,])
      # msScans <<- msScans$
      # msmsScans <<- msmsScans
      
      ####funfin
      # Calculation of Scores
      ####
 
      
      # LoadSettings(Data = temp.DataEvidence,msmsScans = msmsScans,msSC = msScans, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS,rfn = i)
      # msScans <<- msScans
      tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence,msmsScans = msmsScans,msSC = msScans, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS,rfn = i)))
      if(length(msf) > 0){
        try({
          
        
        if(msf!=""){
          
        
          tabi <- msf$Labels
          if(dim(tabi)[1]>4){
            tabi <- tabi[order(as.numeric(tabi[,4]),decreasing = T),][1:4,]
            
            
          }
          
          ant <- msf$MassShifts
          ant <- ant[ant != 1] 
          sel <- names(ant) < 0.5 & names(ant) >-0.5
          
          percent <- sum(ant)#sum(ant[sel],na.rm = T)
          qc.prepare.data$sd$DependentPeptides <- gsub(",",".",paste(tabi[,1],paste(round(as.numeric(tabi[,4])/percent*100,1),"%"),sep = "##",collapse = "_#_"),fixed = T) 
        }
        })
      }
        
      
      export 	  <- unlist(qc.prepare.data$sd)
      
      ChrPath <- ""
      if(length(qc.prepare.data$IdentifiedProteins) > 0& length(AllPeptides) > 0){
        if(nchar(as.character(qc.prepare.data$IdentifiedProteins)) > 0){
          try(ChrPath <- WriteChromatogram(tempAllPeptides,msSC = msScans,msmsSC = msmsScans,filename = i,BSAID =as.character(qc.prepare.data$IdentifiedProteins) ,jitfac = 0,AllPeptides=AllPeptides,Peptides = Peptides,PG = PG ,DataEvidence = DataEvidence,msScans = msScans,msmsScans = msmsScans,TMT=qc.prepare.data$TMTplot))
        }
        if(ChrPath == ""){
          try(ChrPath <- WriteChromatogram(tempAllPeptides,msSC = msScans,msmsSC = msmsScans,filename = i,BSAID =NULL,jitfac = 0,AllPeptides=AllPeptides,Peptides = Peptides,PG = PG ,DataEvidence = DataEvidence,msScans = msScans,msmsScans = msmsScans,TMT=qc.prepare.data$TMTplot))
        }
      }
      # graphics.off()
      add.vec <- c(rep.v[at],as.numeric(Sys.time()),make.names(Sys.time()))
      names(add.vec) <- c("Name","System.Time.s","System.Time")
      export <- t(as.matrix(c(add.vec ,export)))
      
      if(ChrPath[1] != ""){
        IntensityPercentages <- round(ChrPath$IntPerc,1)[-1]
        IntensityPercentagesN <- c("Identified",        "Non Peptide Contaminants","Peptide Contaminants","Dependent")
        IntensityPercentages <- IntensityPercentages[merge.control(names(IntensityPercentages),IntensityPercentagesN)]
        names(IntensityPercentages) <- paste("RelCumulativeIntensity",IntensityPercentagesN)
        export <- cbind(export,t(as.matrix(IntensityPercentages[-1])))
      }else{
        IntensityPercentages <- rep("no info",4)
        IntensityPercentagesN <- c("Identified",        "Non Peptide Contaminants","Peptide Contaminants","Dependent")
        names(IntensityPercentages) <- paste("RelCumulativeIntensity",IntensityPercentagesN)
        
        export2 <- cbind(export,t(as.matrix(IntensityPercentages[-1])))
      }
      ####
      # BSACheck
      ####
      BSACheck <- gsub(placeholder,"BSA", RESettings$REpar,fixed = T)
      if(length(grep(BSACheck,i)) > 0){
        
        BSACheck <- T
      }else{BSACheck <- F}
      #plotData$retentionTime
      
      ####  AllPepsData
      # Plotting
      ####
      tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,msScans = msScans,msmsScans= msmsScans,data.list = qc.prepare.data,pdf.name = i, open.doc = open.doc,pdfOut = pdfOut, BSACheck = BSACheck)))
      if(exists("ChrPath")){
        try(ASCIIprofileplot(TotalScoreRes$plotData,AllPepsData = ChrPath$all))
        
      }else{
        try(ASCIIprofileplot(TotalScoreRes$plotData))
        
      }
      
      TotalScoreRes <- TotalScoreRes
      ASCIIplot <- NULL
      try(ASCIIplot <- readLines(list.files(pattern = "ASCIIplot.txt",full.name = T)))
      if(length(ASCIIplot) > 0){
        ASCIIplot <- paste("\n\n#########\n","# Plots #\n","#########\n\nFor a correct view, please set your font to Menlo regular.\n\n", paste(ASCIIplot,collapse = "\n"),"\n",sep = "")
      }else{
        ASCIIplot <- ""
      }
      
      
      if(tryError == "try-error"){
        TotalScoreRes <- list(TotalScore = rep("?",4),TotalScoreColor = rep("#666666",4))
      }
      tempScoreList <- t(as.matrix(unlist(TotalScoreRes[1:2])))
      export <- cbind(export, tempScoreList)
      colnames(export)[colnames(export) == "TotalScoreColor.DS"] <- "TotalScoreColor"
      colnames(export)[colnames(export) == "TotalScore.DS"] <- "TotalScore"
      
      # ScoreAdd <- c(TotalScoreRes$TotalScore,qc.prepare.data$sc[grep("combi",names(qc.prepare.data$sc))])
      # names(ScoreAdd)[1] <- "Total"
      # names(ScoreAdd) <- paste(names(ScoreAdd),"Score",sep = "_")
      # export <- cbind(export,t(as.matrix(ScoreAdd)))
      
      if(length(exitPath) > 0){
        exitPath  <-  paste(exitPath,paste(Sys.Date(),gsub(".raw$","raw",rep.v[at]),"folder",sep = "_"),paste(rep.v[at],".csv",sep = ""),sep = "/")
        names(exitPath) <- "filePath"
        SourceTime <- as.numeric(SourceTime)
        names(SourceTime) <- "SourceFileTime"
        Status <- "fresh"
        names(Status) <- "Status"
        if(length(SourceTime) == 0){SourceTime <- "error"}
        export <- cbind(export,exitPath, SourceTime, Status)
        
        
      }
      
      try(write.csv(export,paste(rep.v[at],".csv",sep = ""),quote = F,row.names = F))
      try(save(qc.prepare.data, TotalScoreRes ,file = paste(rep.v[at],".Rdata",sep = ""),quote = F,row.names = F))
      
      exportFUN <<- export
      flatFile <- t(export)
      flatFile <- paste(rownames(flatFile),flatFile[,1],sep = "\t\t")
      flatFileIn <- paste(flatFile,collapse = "\n")
      # AverageTMT Labeling:
      LysLabel <-""
      noLysLabel <- ""
      try({
        LysLabel <- round(qc.prepare.data$TMTplot$Lys[qc.prepare.data$TMTplot$Label=="3-both"],1)
        noLysLabel <- round(qc.prepare.data$TMTplot$`no Lys`[qc.prepare.data$TMTplot$Label=="1-Nterm only"])
      })
      
      try(flatFile <- paste("################\n# MQQC Message #\n################\nYour MQQC Analysis of ",data.frame(export)$Name," has finished.\nYou can evaluate your file ",serverinput,"\n\n\n\n#####################\n#  Selected Values  #\n#####################\nNumber of identified peptides: "
                        ,data.frame(export)$msms.count,"\n",
                        "Coverage (median): ",data.frame(export)$Coverage,"\n",
                        "Score (median): ",data.frame(export)$score.50.,"\n",
                        "Median Fragments/identified MSMS: ",data.frame(export)$msmsMassCount.50.,"\n",
                        "MS log10 Intensity (median): ",log10(as.numeric(as.character(data.frame(export)$Intensity.50.))), ASCIIplot,"\n",
                        
                        "MSMS log10 Intensity (median): ",log10(as.numeric(as.character(data.frame(export)$msmsQuantile.50.))), ASCIIplot,"\n",
                        
                        "Median mass precision [ppm]: ",data.frame(export)$precision.50. ,
                        "\n\n","################\n# TMT Labeling #\n################\n\n",
                        "Fully Labeled Lysine:",LysLabel,"% \n",
                        "Fully Labeled wo Lysines:",noLysLabel,"% \n\n",
                        
      "Mass Difference Peptides:",data.frame(export)$DependentPeptides,"\n",sep = ""))
      
      
      if(!exists("export")){
        export <- "No Info"
      }
      if(any(c(tryError2, tryError1) == "try-error")){
        export <- cbind(export,"An unexpected error occured")
        
      }
      
      #test 		<- HtmlTable(t(export))
      export 	<- data.frame(export)
      graphics.off()
      setwd("../") 
      MailList <- list.files(path.package("mqqc"),pattern = "MailList.txt",recursive=T,full.name = T)
      if(length(MailList) > 0&SendMail){
        try({
          MailList  <- read.table(MailList,sep = "\t",colClasses = "character",stringsAsFactors = F)
          MailList 	<- apply(MailList,2,as.character)
          MailList <- MailList[!is.na(MailList[,1]),]
          if(is.vector(MailList)){MailList <- t(as.matrix(MailList))}
          MailPatterns <- sapply(MailList[,1],function(x){gsub(placeholder,x,RESettings$REmail)})
          if(!all(sapply(MailPatterns,length) == 0)){
            MailID <- sapply(MailPatterns ,function(x){
              xn <- as.character(export$Name)
              try(x  <- grep(x,as.character(export$Name),value = F))
              return(length(x) > 0)
            })
            if(any(MailID)){
              if(!is.vector(MailList)){
                MailID <- MailList[MailID,]
              }else{
                MailID <- MailList[MailID]
              }
              MailID <- grep("@",MailID,fixed = T,value = T)[1]
              for(Mail in MailID){
                # PrepareMail("Hiho","Ich freue mich, dass diese eMail ankommt!",recipient = "henrik.zauber@mdc-berlin.de",MailSecurity = NULL)
                
                PrepareMail(Title = paste("MQQC",data.frame(export)$Name,data.frame(export)$msms.count,"Peptides"),Message = flatFile,recipient=gsub("@","@",as.character(Mail),fixed = T),MailSecurity = NULL)
              }
            }
          }
        })
      }
      list.collect[at]     <- qc.prepare.data
      at <- at+1
    }
    sink()
    
    names(list.collect) <- rep.v
    if(show.path){
      
      "Close tkmessage to finish process"
      try(tkmessageBox(message = paste("Finished QC-Analysis. Result in path:\n",getwd(),sep = "")))
      
    }
    cat("Finished QC Analysis")
    #try(return(list(qc = are.data)))
    #try(system("open ."))
  }

# start.qc("/Users/henno/temp/mqqc/GONZO_123456_HZ_HS_Fun2raw_folder/combined/txt/evidence.txt")
# try(qcResults 	<- start.qc(DataEvidence = tempI,placeholder="PLACEHOLDER",RESettings= RESettings,SendMail= SendMail,exitPath = paste(folder,sucFolder,sep = "/"),BSAID = Param$BSAID,SpeciesTable = T))

# start.qc("/Users/henno/Documents/Skripte/R-Functions/Selbach-Functions/DataAnalysis/PRM/Ubiquitin/JPT/txt/evidence.txt")
# DataEvidence <- "/Volumes/SSD/MQQC/20181111_HZ_ECstd_2raw_folder/combined/txt/evidence.txt"
# start.qc(DataEvidence)
# start.qc()
# start.qc()
# LoadSettings(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER", RESettings =list(REpar = "PLACEHOLDER"),SendMail = T, exitPath = NULL, BSAID = "P02769")
# Param <- mqqcGUI()
# for(i in 1:length(Param)){
#   assign(names(Param)[i],Param[[i]])
# }
# RESettings <- Param[grep("^RE",names(Param))]



# start.qc()
#start.qc("/Users/henno/temp/test/TiffyEC/txt/evidence.txt")
#start.qc()
#system(paste("open ",list.files(pattern = ".pdf",recursive = T,full.name = T)))
# LoadSettings(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER", RESettings =list(REpar = "PLACEHOLDER"),SendMail = T, exitPath = NULL, BSAID = "P02769")
# # LoadSettings(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS)
# start.qc("/Users/henno/temp/mqqc/Neuer Ordner/txt2/evidence.txt")
# tempI <- "/Users/henno/temp/mqqc/ERIKTEST/txt/evidence.txt"
 #try(qcResults 	<- start.qc(tempI,placeholder=placeholder,RESettings= RESettings,SendMail= SendMail,exitPath = paste(folder,sucFolder,sep = "/"),BSAID = Param$BSAID))
# start.qc(DataEvidence <- "/Users/henno/temp/Gladys_12345678_HZ_ECstd_blaRaw_folder/combined/txt/evidence.txt")

