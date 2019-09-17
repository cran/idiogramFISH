#' Function to plot idiograms without centromere
#'
#' @description This function reads a dataframe  with columns: \code{chrName}
#' and \code{chrSize} and plots as idiograms. If more than one species, a column
#' named \code{OTU} is needed. Accepts micrometers and Mb.
#' @description Optionally, it reads another dataframe with the position of
#' marks (sites) \code{dfMarkPos}, in that case the description of marks is
#' necessary in dataframe \code{dfMarkColor}
#'
#' @param dfChrSize dataframe with columns: OTU (optional), chrName, chrSize
#'   (see Mb)
#' @param dfMarkPos dataframe of marks (sites): cols: OTU (opt) chrName,
#'   markName (name of site), markPos, markSize
#' @param dfMarkColor dataframe specifying colors for marks (sites); cols:
#'   markName, markColor, style: square or dots
#' @param mycolors character vector, optional, i.e. \code{c("blue","red","green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the
#' order of your marks use something like: \code{unique(dfMarkPos$markName)}
#' @param origin use "b" if distance to mark (markPos col. in dfMarkPos)
#'   measured from bottom, use "t" for distance to mark from top
#' @param MarkDistanceType character, if cen = the distance you provided is to
#'   the center of the mark, if beg = the distance you provided is to the
#'   beginning of the mark
#' @param Mb boolean, if TRUE distance in dfs is not micrometers but bases, will be shown as Megabases
#' @param ylabline, numeric if \code{Mb=TRUE} modify position of y axis title (Mb)
#' @param orderBySize logical value, when TRUE, sorts chromosomes by total
#'   length from the largest to the smallest
#' @param chrWidth numeric, chromosome width
#' @param chrSpacing numeric, horizontal spacing among chromosomes
#' @param chrColor character, main color for chromosomes
#' @param roundness numeric, shape of vertices of chromosomes and square marks,
#'   higher values more squared
#' @param dotRoundCorr numeric, correct roundness of dots. When style of sites =
#'   dots, an increase in this, makes the horizontal radius of the dot smaller
#' @param karHeight numeric, vertical size of karyotypes
#' @param karSpacing numeric, proportional to karHeight for spacing of
#'   karyotypes, y axis, if overlap, increase
#' @param reduDistKar boolean, reduce distance among karyotypes FALSE = equally
#'   sized karyotypes or TRUE= equally spaced karyotypes
#' @param reduDistKarTol numeric, depends on reduDistKar=TRUE, if zero your
#'   karyotypes will have no distance among them, use 1 to 11, if overlap,
#'   increase this and karSpacing
#' @param chrId character, print name of chromosme, "original" uses the original
#'   name in OTU column of dfChrSize, "simple" (just 1 to ...) or "" (none).
#' @param distTextChr numeric, distance from name of chromosome to chromosome
#' @param indexIdTextSize numeric, font size of chromosome name and A2 index
#' @param OTUTextSize numeric, font size of OTU name (species)
#' @param legend character, "" for no legend; "inline" prints labels near
#'   chromosomes; "aside" prints legend to the right of karyotypes
#' @param markLabelSize numeric, only if legend !="", size of the text of labels
#'   of marks (legend)
#' @param markLabelSpacer numeric, only if legend !="", space from the rightmost
#'   chr. to legend
#' @param lwd.chr numeric, thick of border of chr. or marks
#' @param karIndex logical, add karyotype index A2 (interchromosomal asymmetry,
#'   variation among chromosome sizes)
#' @param karIndexPos numeric, move karyotype index
#' @param addOTUName boolean, if TRUE adds OTU (species) name to karyotype
#' @param revOTUs boolean, The order of species is the one in the main
#'   dataframe, use TRUE to reverse
#' @param ruler boolean, display ruler to the left of karyotype, when FALSE no ruler
#' @param rulerPos numeric, absolute position of ruler, corresponds to pos
#'   argument of axis R plot
#' @param rulerPosMod numeric, modify position of ruler, corresponds to line
#'   argument of axis R plot
#' @param ruler.tck numeric, tick size of ruler, corresponds to tck argument of
#'   axis R plot
#' @param rulerNumberPos numeric, modify position of numbers of ruler
#' @param rulerNumberSize numeric, size of number's font in ruler
#' @param xlimLeftMod numeric, modifies xlim left parameter of plot
#' @param xlimRightMod numeric, xlim modification by adding space to the right
#'   of idiograms
#' @param ylimBotMod numeric, modify ylim bottom parameter of plot
#' @param ylimTopMod numeric, modify ylim top parameter of plot
#'
#' @param ... accepts other arguments for the plot, such as, asp
#' @keywords dataframe chromosome
#' @export
#' @examples
#' data(dfChrSizeHolo)
#' plotIdiogramsHolo(dfChrSizeHolo)
#'
#' @seealso plotIdiograms
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{asymmetryA2}}
#' @seealso \code{\link{chrbasicdataHolo}}
#' @seealso \code{\link{markdataholo}}
#' @seealso \code{\link{dfMarkColor}}
#'
#' @return plot
#' @importFrom graphics par plot mtext segments

plotIdiogramsHolo<-function(dfChrSize, dfMarkPos, dfMarkColor, mycolors, MarkDistanceType="beg", origin="b", orderBySize=TRUE,
                            Mb=FALSE, ylabline=0,
                            chrWidth=1.5, chrSpacing=1.5,chrColor="gray",
                            roundness=4, dotRoundCorr=1.5,
                            karHeight=1.2,karSpacing=1.6,reduDistKar=TRUE,reduDistKarTol=9,
                            chrId="original", distTextChr=.3,
                            indexIdTextSize=.4, OTUTextSize=.6,
                            legend="inline" ,markLabelSize=.4, markLabelSpacer=2, lwd.chr =1,
                            karIndex=TRUE, karIndexPos=.5,
                            addOTUName=TRUE,revOTUs=FALSE,
                            ruler=TRUE,rulerPos=-.5, rulerPosMod=0, ruler.tck=-0.004, rulerNumberPos=.2, rulerNumberSize=.4,
                            xlimLeftMod=1,  xlimRightMod=10, ylimBotMod=.2, ylimTopMod=.2,
                            ...)
{
  if(!missing(dfChrSize)){
    dfChrSizeInternal<-as.data.frame(dfChrSize)
  } else {
      message(crayon::red("Missing mandatory dfChrSize dataframe"))
    return(NA)
  }
  # df of marks
  if(!missing(dfMarkPos)){
    dfMarkPosInternal<-as.data.frame(dfMarkPos)
  }
  if(!missing(dfMarkColor)){
    dfMarkColorInternal<-as.data.frame(dfMarkColor)
  }

  message(crayon::black(paste("Making checks\n")) )
  message(crayon::black(paste("In case of error see messages and the help ?functionName\n"))
  )

  #
  #   dfChrSizeInternal
  #

  if(exists("dfChrSizeInternal")){
    message(crayon::black("\nChecking mandatory columns from dfChrSize: chrName, chrSize\n (column OTU  is necessary if more than one species)\n"
    )) # cat
    if(length( setdiff(c("chrName", "chrSize"),
                       colnames(dfChrSizeInternal) ) )>0 ){
        message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "chrSize"),
                                      colnames(dfChrSizeInternal) ) ), sep="\n", collapse = " " )
      )
      )# cat
        message(crayon::red(paste("\nwill quit now\n") )
      )#c
      return(NA)
    } # fi
    else { # if no error in column names
      if(Mb==FALSE & max(dfChrSizeInternal$chrSize)>10000 ){
          message(crayon::red(paste("\nChromosome size too large, use Mb=TRUE, you are probably working in Mb, not micrometers\n") )
        )#c
        return(NA)
      } else if (Mb & max(dfChrSizeInternal$chrSize)<10000 ) {
          message(crayon::red(paste("\nChromosome size in Mb too small, use Mb=FALSE, you are probably working with micrometers\n") )
        )#c
        return(NA)
      }
        message(crayon::black("\nOK\n") )
        dfChrSizeInternal$chrSize <-as.numeric(dfChrSizeInternal$chrSize)
    }
  } # fi dfChrSize

  #
  #   dfMarkPosInternal
  #

  if(exists("dfMarkPosInternal")){
      message(crayon::black("\nChecking mandatory columns from dfMarkPos: chrName, markName, origin,markPos,markSize\n (column OTU  is necessary if more than one species)\n"
    ) ) # cat
    if(!"origin" %in% colnames(dfMarkPosInternal)){
      dfMarkPosInternal$origin<-"b"
        message(crayon::black(paste("\n Missing column origin was created with value: b\n")
      ))# cat
    } # origin present
    if(length (setdiff(c("chrName", "markName", "origin","markPos","markSize"),
                       colnames(dfMarkPosInternal) ) )>0 ){
        message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName", "origin","markPos","markSize"),
                                      colnames(dfMarkPosInternal) ) ) , sep="\n", collapse = " " )
      )
      ) # cat
        message(crayon::red("\nERRORS PRESENT, see above, dfMarksPos REMOVED\n")
        )#m
      remove(dfMarkPosInternal)
    } # fi
    else { # if no error
        message(crayon::black("\nOK\n")
        )#m

      dfMarkPosInternal$markPos<-as.numeric(dfMarkPosInternal$markPos)
      dfMarkPosInternal$markSize<-as.numeric(dfMarkPosInternal$markSize)

      if(origin=="t"){
        dfMarkPosInternal$markPos2<-dfMarkPosInternal$markPos
        dfMarkPosInternal$chrSize<-
          dfChrSizeInternal[match(interaction(dfMarkPosInternal[c("OTU", "chrName")]),
                                  interaction(dfChrSizeInternal[c("OTU", "chrName")])),]$chrSize
        dfMarkPosInternal$markPos<-dfMarkPosInternal$chrSize-dfMarkPosInternal$markPos2
      }
      if(MarkDistanceType=="cen"){
        dfMarkPosInternal$markPos<-dfMarkPosInternal$markPos-(dfMarkPosInternal$markSize/2)
      } # if
    } # else
  } # fi

  #
  #   checkNameChrDfMarks
  #

  if(exists("dfMarkPosInternal")){
      message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n")
      )#m
    FstCheck<-checkNameOTUdfMarks(dfChrSizeInternal,dfMarks=dfMarkPosInternal)
    if(FstCheck){
      SndCheck<-checkNameChrDfMarks(dfChrSizeInternal,dfMarks=dfMarkPosInternal)
      if(SndCheck){
        allMarkNames<-unique(dfMarkPosInternal$markName)
        allMarkMaxSize<-max(dfMarkPosInternal$markSize)
      } else { # SndCheck
          message(crayon::red("\nERRORS PRESENT, see above, dfMarkPos REMOVED\n")
        )#cat
        remove(dfMarkPosInternal)
      }
    } else { # FstCheck
        message(crayon::red("\nERRORS PRESENT, see above, dfMarkPos REMOVED\n")
      )#cat
      remove(dfMarkPosInternal)
    }
  } # 1st if

  #
  #   dfMarkColor
  #

  if(exists("dfMarkColorInternal")){
      message(crayon::black("\n####\nChecking mandatory columns from dfMarkColor: markName, markColor, style\n"
    ) )#cat
    if(!"style" %in% colnames(dfMarkColorInternal) ){
        message(crayon::red("\ndfMarkColor style column missing, created with string: square\n") )
      dfMarkColorInternal$style<-"square"
    }

    if (length( setdiff(c("markName", "markColor","style"),
                        colnames(dfMarkColorInternal) ) )>0 ){
        message(crayon::red(paste(c("ERROR Missing column:",
                              setdiff(c("markName", "markColor","style"),
                                      colnames(dfMarkColorInternal) ) ) , sep="\n", collapse = " ")
      )
      ) # cat
      remove(dfMarkColorInternal)
        message(crayon::red("\nError in dfMarkColor, REMOVED\n")
        )#m
    } #fi
    else { # column names ok
      if(exists("allMarkNames")){
        dfMarkColorInternal<-dfMarkColorInternal[which(dfMarkColorInternal$markName %in% allMarkNames) ,]
        if (nrow(dfMarkColorInternal)==0){
            message(crayon::red("\nError in dfMarkColor Names respect to Marks dataframes, dfMarkColor REMOVED\n")
          )#cat
          remove(dfMarkColorInternal)
        } else { # nrow not 0
            message(crayon::black("\nCheck OK\n"))
        }
      } # fi # end allmarknames exist
      else { #all Mark Names does not exist
          message(crayon::red("\nError in dfMarkColor Names respect to Marks dataframes, dfMarkColor REMOVED\n")
        )
        remove(dfMarkColorInternal)
      } # else
    } # else column names ok
  } #fi
  ######################################################################################################
  #   for > 1.0.0
  ######################################################################################################
  else if (missing(mycolors) ) { # if dfMarkColor not exist and missing mycolors
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColor(allMarkNames,c(chrColor) )
    } # allmarknames
  } else if (!missing(mycolors) ){ # if dfMarkColor not exist if mycolors exist
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColorMycolors(allMarkNames,c(chrColor), mycolors )
    }
  } # elif

  #
  #   CREATION OF CHILD DATAFRAMES MARKS
  #

  if(exists("dfMarkPosInternal") & exists("dfMarkColorInternal")  ){
    if(FstCheck & SndCheck){
      if(Mb){
        dfMarkPosInternal$markPos<-dfMarkPosInternal$markPos/1000000
        dfMarkPosInternal$markSize<-dfMarkPosInternal$markSize/1000000
      }
      ###################
      # require STYLE
      ###################
      dfMarkPosInternal$style<-dfMarkColorInternal$style[match(dfMarkPosInternal$markName, dfMarkColorInternal$markName)]
      dfMarkPosSq<-dfMarkPosInternal[which(dfMarkPosInternal$style=="square"),]
      if(nrow(dfMarkPosSq)==0){
          message(crayon::black("\nno square style marks to plot\n") )
        remove(dfMarkPosSq)
      }
      dfMarkPosCr<-dfMarkPosInternal[which(dfMarkPosInternal$style=="dots"),]
      if(nrow(dfMarkPosCr)==0){
          message(crayon::black("\nno dots style marks to plot\n") )
        remove(dfMarkPosCr)
      }
    } #  CHECK
  } # EXISTS
############################################################################## lists
  #
  #   dfChrSize to list
  #

  if(Mb){
    dfChrSizeInternal$chrSize<-dfChrSizeInternal$chrSize/1000000
  }

  if("OTU" %in% colnames(dfChrSizeInternal)){
    listOfdfChromSize<-base::split(dfChrSizeInternal, dfChrSizeInternal$OTU )
    names(listOfdfChromSize)<-unique(dfChrSizeInternal$OTU)
  } else {
    listOfdfChromSize<-list(dfChrSizeInternal)
    names(listOfdfChromSize)<-1

    addOTUName<-FALSE
  }

  listOfdfChromSize<-rev(listOfdfChromSize)

  if(revOTUs){
    listOfdfChromSize<-rev(listOfdfChromSize)
  }

  {
    chrWidth<-chrWidth/length(listOfdfChromSize)
    chrSpacing<-chrSpacing/length(listOfdfChromSize)
  }

  #
  #   dfMarkPosSq to list
  #

  if (exists("dfMarkPosSq")){
    if("OTU" %in% colnames(dfMarkPosSq)){
      listOfdfMarkPosSq<-base::split(dfMarkPosSq, dfMarkPosSq$OTU )
      names(listOfdfMarkPosSq)<-unique(dfMarkPosSq$OTU) #1:(length(listOfdfMarkPosSq))
    } else {
      listOfdfMarkPosSq<-list(dfMarkPosSq)
      names(listOfdfMarkPosSq)<-1#unique(dfMarkPosSq$OTU) # 1:(length(listOfdfMarkPosSq))
    }
  } # end missing dfMarkPosSq

  #
  #   dfMarkPosCr to list
  #

  if (exists("dfMarkPosCr")){
    if("OTU" %in% colnames(dfMarkPosCr)){
      listOfdfMarkPosCr<-base::split(dfMarkPosCr, dfMarkPosCr$OTU )
      names(listOfdfMarkPosCr)<-unique(dfMarkPosCr$OTU) #1:(length(listOfdfMarkPosCr))
    } else {
      listOfdfMarkPosCr<-list(dfMarkPosCr)
      names(listOfdfMarkPosCr)<-1
    }
  } # end missing dfMarkPosSq

  #######################################################
  #
  #   If Marks missing, rename duplicates of chrNames
  #
  #######################################################

  mybooleanChrName <- !exists("listOfdfMarkPosSq") & !exists("listOfdfMarkPosCr")

  listOfdfChromSize<-fixChrNameDup(listOfdfChromSize, mybooleanChrName)


  ######################
  #   total size of chro and normalize
  ######################

  {
    totalLength<-lapply(listOfdfChromSize, function(x) x$chrSize)
    ifelse(class(totalLength)=="matrix",
           totalLength <- base::split(totalLength, col(totalLength) )
           ,NA)
    normalizeToOne<-karHeight/max(unlist(totalLength) , na.rm=TRUE)
  }

  ######################################
  # order size
  ######################################

  if(orderBySize==TRUE) {
    orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
  } else { # if not want to order by size, set order by name of chro
    orderlist<-lapply(listOfdfChromSize, function(x) order(x$chrName) )
  }

  # { # remove

    ##############################################
    #
    #   add column of new chro index to dataframes
    #
    ##############################################

    for (s in 1:length(listOfdfChromSize)){

    listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][orderlist[[s]], ]
    listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]] )
    #
    # listOfdfChromSize[[s]]$neworder <- sapply(1:(nrow(listOfdfChromSize[[s]])), function(x) grep(paste0("^",x,"$"), orderlist[[s]]) )
    }

    ###################################################
    #     after 1.1.0
    ###################################################

    if("group" %in% colnames(dfChrSizeInternal)){
      message("group column present - remove column if not using")
      grouporderlist<-lapply(listOfdfChromSize, function(x) order(x$group) )
      for (s in 1:length(listOfdfChromSize)){
        listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][grouporderlist[[s]], ]
        listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
        #
        # listOfdfChromSize[[s]]$neworder <- sapply(1:(nrow(listOfdfChromSize[[s]])), function(x) grep(paste0("^",x,"$"), grouporderlist[[s]]) )
        #
      } # end for
      orderlist<-grouporderlist
    } # if group
    ##
    # order by group and chromosome
    ##
    orderingcolumn<-"neworder"

    #####################################################
    #
    #   important - add new indexer to df of marks
    #
    #####################################################
    if(exists("dfMarkPosSq") ){

      for (s in 1:length(listOfdfChromSize)){
        selecteddfChromData<-which(names(listOfdfMarkPosSq)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfChromData)>0){
          listOfdfMarkPosSq[[selecteddfChromData]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfMarkPosSq[[selecteddfChromData]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      }
    } # end if presence of dfMarkPosSq order

    if(exists("dfMarkPosCr") ){
      for (s in 1:length(listOfdfChromSize)){
        selecteddfChromData<-which(names(listOfdfMarkPosCr)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfChromData)>0){
          listOfdfMarkPosCr[[selecteddfChromData]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfMarkPosCr[[selecteddfChromData]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      }
    } # end if presence of dfMarkPosSq order

  ######################
  # main plot #
  ######################

  {
    chromosome_ns<-sapply(listOfdfChromSize, function(x) nrow(x) )
    num_species<-length(listOfdfChromSize)
  }

  { # processing for main plot
    rownumber<-1
    chromRepVector<-lapply(chromosome_ns, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))
    x<-list()
    y<-list()
    xm<-list()
    ym<-list()

    for (s in 1:num_species){
      croytop<-list()
      croxleft<-list()
      croxright<-list()

      for (i in 1:length(chromRepVector[[s]])){
        # croytop[i]<-list(c((0+chromSizeOrdered[[s]][i]),
          croytop[i]<-list(c(   (0+ (listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]   ),
          rep(0,2),
          # (0+chromSizeOrdered[[s]][i])
          (0+(listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]  )
        ) # c
        ) # list
      } # for

      crox<-matrix(rep(NA,length(chromRepVector[[s]])*4),ncol=4, nrow=length(chromRepVector[[s]]) )

      for (i in 1:length(chromRepVector[[s]])){
        crox[i,] <- c(
          rep( ( (chrSpacing*(i-1)+((i-0)*chrWidth))+chrWidth),2),
          rep( ( (chrSpacing*(i-1)+((i-0)*chrWidth))     ),2)
        ) # c rox
      } # for

      xm[[s]] <- crox
      x[[s]] <- base::split(xm[[s]], row(xm[[s]]))
      ifelse(any(is.na(x[[s]]) ),x[[s]]<-NA,"")
      ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + (karSpacing*(s-1)  )
    } # for species

    ymCopyC<-ymCopy<-ym

    if (length(ym)==1){
      reduDistKar=FALSE
    }

    if(reduDistKar){
      for (s in 1:(length(ym)-1)) {
        diffnext<-abs(min(ym[[s+1]] ) - max(ym[[s]]) )
        diffnext<-(diffnext-((distTextChr/3)*reduDistKarTol))
        ym[[s+1]]=ym[[s+1]]-diffnext
      }
    }

    names(ym)<-names(listOfdfChromSize)
    y<-lapply(1:length(ym), function(s) base::split(ym[[s]], row(ym[[s]] )) )

    #####################
    #   main  plot DO
    #####################

    n2<-25
    if(!karIndex){
      xlimLeftMod<-xlimLeftMod*(1/3)
    }
    leftmodifier<-(xlimLeftMod*karIndexPos)+chrWidth

        #####################################################################################################################
    graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-leftmodifier),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                   ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ), ylab = "", xaxt='n',
                   xlab="", yaxt='n',main = NULL, frame.plot = FALSE, ...)
    if (Mb){
      opar<-graphics::par(no.readonly = TRUE)
      on.exit(suppressWarnings(par(opar)) )
      par(las=1)
      mtext("Mb", side=2, line=ylabline, at=max(unlist(y), na.rm = TRUE) )
    }
    #xlab="", yaxt='n',main = NULL, frame.plot = FALSE)

    xsizeplot<-(max(unlist(x), na.rm=TRUE)+xlimRightMod )- ( (min(unlist(x), na.rm=TRUE)-(leftmodifier)) )
    ysizeplot<- max(unlist(y) )
    yfactor<-(ysizeplot/xsizeplot)*dotRoundCorr

    ######################################################################################################################


    roundPlot(roundness, x, y, col=chrColor, border=chrColor, chrWidth, yfactor,n2,lwd.chr) #

  } # end main plot

  ################################
  #
  #   ruler calculate
  #
  ################################

  if (ruler){
    maxChrRound<-lapply(1:length(listOfdfChromSize), function(x) ceiling(max(listOfdfChromSize[[x]]$chrSize)) )
    fromZerotoMaxChr<-lapply(maxChrRound, function(x) seq(0,x, by=1) )
    ycoordChrRound  <-lapply(1:length(fromZerotoMaxChr), function(x)
      unlist(
        lapply(1:length(fromZerotoMaxChr[[x]]), function(y)
          (0+(fromZerotoMaxChr[[x]][y]*normalizeToOne))+(1*karSpacing*(x-1)) )
      ) # u
    ) # l

    if(reduDistKar){
      for (s in 1:(length(ymCopy)-1) ) {
        diffnext<-abs(min(ymCopy[[s+1]] ) - max(ymCopy[[s]]) )
        diffnext<-(diffnext-((distTextChr/3)*reduDistKarTol))
        ymCopy[[s+1]]=ymCopy[[s+1]]-diffnext
        ycoordChrRound[[s+1]]<-ycoordChrRound[[s+1]]-diffnext
      }
    } # redu if

    ####################
    #
    #   add rulers
    #
    ####################

    # if(Mb){
    #   fromZerotoMaxChr<-lapply(fromZerotoMaxChr, function(x) x*1000000)
    #   fromZerotoMaxChr<-lapply(fromZerotoMaxChr, function(x) format(x, scientific=FALSE) )
    # }
    opar<-graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(opar)) )
    graphics::par(mgp=c(3,rulerNumberPos,0) )
    lapply(1:length(ycoordChrRound), function(x)
      graphics::axis(side=2, at=unlist(ycoordChrRound[[x]]),
                     labels = unlist(fromZerotoMaxChr[[x]] ),
                     cex.axis=rulerNumberSize
                     ,las=1
                     ,line=rulerPosMod
                     ,pos= rulerPos
                     ,tck=ruler.tck
      ) # axis
    ) # l
  }   # end rulers

    #
    #   groups line > 1.1.0
    #

    if("group" %in% colnames(dfChrSizeInternal)){
      for (s in 1:length(xm)){
        ngroup<-length(table(listOfdfChromSize[[s]]$group ) )
        for (g in 1: ngroup){
          x0= xm[[s]][,3][ifelse(length(cumsum(table(listOfdfChromSize[[s]]$group))[g-1] )==0,
                                 1,
                                 cumsum(table(listOfdfChromSize[[s]]$group) )[g-1]+1
          )]
          x1= xm[[s]][,3][cumsum(table(listOfdfChromSize[[s]]$group) )[g]  ]+chrWidth
          segments(x0=x0,
                   y0=(min(ym[[s]])-(distTextChr/3)),
                   x1=x1,
                   y1=(min(ym[[s]])-(distTextChr/3) )
          ) # seg
          ########################
          #     group name, after 1.1.0
          ########################3
          text( (x0+x1)/2,
                (min(ym[[s]])-(distTextChr/3)*2 ),
                labels = names( table(listOfdfChromSize[[s]]$group)[g] ),
                cex=indexIdTextSize
          )# text end
        } # for group
      } # for sp
    } # if



  #################################
  # horizontal chromosome names
  #################################

  chrNameDistance<-ifelse(exists("grouporderlist"),3,1)

  if(chrId=="original"){
    # original names of chromosomes
    lapply(1:length(xm), function(i)
      graphics::text(xm[[i]][,3][1:(nrow(xm[[i]]))]+chrWidth/2,
                     rep( (min(ym[[i]])-(distTextChr/3)*chrNameDistance),(nrow(xm[[i]])) ),
                     labels = listOfdfChromSize[[i]][,"chrName"],
                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  }  else if (chrId=="simple"){
    # i<-1
    # Simple numbering from 1 to ...
    lapply(1:length(xm), function(i)
      graphics::text(xm[[i]][,3][1:(nrow(xm[[i]]))]+chrWidth/2,
                     rep( (min(ym[[i]])-(distTextChr/3)*chrNameDistance ),(nrow(xm[[i]]) ) ),
                     # rep( (min(ym[[i]])-.1),(nrow(xm[[i]])/2) ),
                     labels = 1:(nrow(xm[[i]])),
                     cex=indexIdTextSize
      ) # t
    ) # lapply
  } # elif

  #################################
  # vertical karyotype index
  #################################

  if(karIndex){
    ind<-asymmetryA2(dfChrSizeInternal)
    if(!is.null(ind)){
      # generate indexes
      lapply(1:length(xm), function(s)
        graphics::text(c( (xm[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),# (xm[[s]][,3][1]-(xlimLeftMod*.66) )+karIndexPos ),#
                       rep( (min(ym[[s]][,1])-(distTextChr/3) ) , 1 ), # avoid overlap
                       labels = paste("A2",ind$A2[[s]] ),
                       cex=indexIdTextSize,
                       adj=c(1) # 0.5 centered
        ) # end graphics::text
      ) # end lapply
    } # null
  } # fi

  #########################################################################
  # add species names
  #########################################################################

  distVector<- ifelse(exists("grouporderlist"), ((distTextChr/3)*4 ),((distTextChr/3)*3 ) )

  if(addOTUName){
    lapply(1:length(xm), function(s)
      graphics::text( c( (xm[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                      ydistance<-min(ym[[s]]) - distVector,
                      labels = paste("",names(listOfdfChromSize)[[s]] ),
                      cex=OTUTextSize,
                      adj=c(0) # justif 0 =left
      ) # end graphics::text
    ) # end lapply
  } # fi

  ########################################
  #
  #   painting Marks square
  #
  ########################################

  if (exists("dfMarkPosSq") & exists("dfMarkColorInternal") ) {
    yMark<-list()
    xMark<-list()
    for (sm in 1:length(listOfdfMarkPosSq)){
      yMark1<-NULL
      xMark1<-NULL
      for (m in 1:nrow(listOfdfMarkPosSq[[sm]])){
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosSq)[[sm]] )

        ysup<-ym[[corr_index]][(listOfdfMarkPosSq[[sm]][,orderingcolumn][m]),2]+
          (listOfdfMarkPosSq[[sm]][m,"markPos"]                                      *normalizeToOne*1)
        yinf<-ym[[corr_index]][(listOfdfMarkPosSq[[sm]][,orderingcolumn][m]),2]+
          ((listOfdfMarkPosSq[[sm]][m,"markPos"]+listOfdfMarkPosSq[[sm]][m,"markSize"])*normalizeToOne*1)

        yMark1[[m]]<-c(ysup,yinf,yinf,ysup)
        xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosSq[[sm]][,orderingcolumn][m],]
      }
      yMark[[sm]]<-yMark1
      xMark[[sm]]<-xMark1
    } # end for


    #####################
    #   add marks to plot
    #####################
    roundPlotMark(roundness, xMark, yMark,
              dfMarkColorInternal,
              listOfdfMarkPosSq,
              chrWidth,
              yfactor,
              n2,
              lwd.chr) #

  } # painting marks square fi

  ########################################
  #
  # dot style of marks
  #
  ########################################

  if (exists("dfMarkPosCr") & exists("dfMarkColorInternal") ){
    xfactor<-(xsizeplot/ysizeplot  )/dotRoundCorr

    yMarkCr<-list()
    xMarkCr<-list()
    rad<-list()
    colCr<-list()

    for (k in 1:length(listOfdfMarkPosCr)){
      yMarkCr1<-NULL
      xMarkCr1<-NULL
      rad1<-NULL
      colCr1<-NULL

      for (i in 1:nrow(listOfdfMarkPosCr[[k]])){
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosCr)[[k]] )
        ysupCr<-ym[[corr_index]][(listOfdfMarkPosCr[[k]][,orderingcolumn][i]),2]+
          (listOfdfMarkPosCr[[k]][i,"markPos"]                                      *normalizeToOne*1)
        yinfCr<-ym[[corr_index]][(listOfdfMarkPosCr[[k]][,orderingcolumn][i]),2]+
          ((listOfdfMarkPosCr[[k]][i,"markPos"]+listOfdfMarkPosCr[[k]][i,"markSize"])*normalizeToOne*1)
        yMarkCr1[[i]]<-rep(list(mean(c(yinfCr,ysupCr))),2)
        xBoundaries<-xm[[corr_index]][listOfdfMarkPosCr[[k]][,orderingcolumn][i],3:2]
        xBoundariesQuar<-(xBoundaries[2]-xBoundaries[1])/4
        xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) )
        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        colCr1[[i]] <- rep(list(dfMarkColorInternal$markColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
      }
      yMarkCr[[k]]<-yMarkCr1
      xMarkCr[[k]]<-xMarkCr1
      rad[[k]]<-rad1
      colCr[[k]]<-colCr1
    } # end for

    #####################
    #   add to plot MarkCrs DOTS
    #####################

    lapply(1:length(xMarkCr), function(w)
      lapply(1:length(xMarkCr[[w]] ), function(u)
        mapply(function(x,y,r,z) {
          pts2=seq(0, 2 * pi, length.out = 25)
          xy2 <- cbind(x + r * sin(pts2)*xfactor , y + r * cos(pts2) )
          graphics::polygon(xy2[,1],xy2[,2], col=z, border = z)
        }, # m
        x=xMarkCr[[w]][[u]],
        y=yMarkCr[[w]][[u]],
        r=rad[[w]][[u]],
        z=colCr[[w]][[u]]
        ) # mapply
      ) # lapply
    ) # l
  } # end painting MarkCrs

  #
  #   labels inline
  #

  if(legend=="inline" & exists("dfMarkColorInternal") & exists("dfMarkPosInternal") & exists("listOfdfMarkPosSq") ) {
    lapply(1:length(xMark), function(s)
      lapply(1:length(xMark[[s]]), function (m)
        mapply(function(x,y,z) graphics::text(x=x,
                                              y=y,
                                              label=z,
                                              cex=markLabelSize,
                                              # pos=4,
                                              adj=0
        ),
        x=xMark[[s]][[m]][1]+chrSpacing*.1,
        y=(yMark[[s]][[m]][1]+yMark[[s]][[m]][2])/2,
        z=listOfdfMarkPosSq[[s]]$markName[m]
        # t
        ) #m
      )# l
    )# l
  }

  if(legend=="inline" & exists("dfMarkColorInternal") & exists("dfMarkPosInternal") & exists("listOfdfMarkPosCr") ) {
    lapply(1:length(xMarkCr), function(s)
      lapply(1:length(xMarkCr[[s]]), function (m)
        mapply(function(x,y,z) graphics::text(x=x,
                                              y=y,
                                              label=z,
                                              cex=markLabelSize,
                                              # pos=4,
                                              adj=0
        ),
        x=xMarkCr[[s]][[m]][[2]]+xBoundariesQuar+chrSpacing*.1,
        y=(yMarkCr[[s]][[m]][1]),
        z=listOfdfMarkPosCr[[s]]$markName[m]
        # t
        ) #m
      )# l
    )# l
  }

  ##############################################
  #
  #   labels to the right
  #
  ##############################################

    if(legend=="aside" & exists("dfMarkColorInternal") ){
      plotlabelsright(x,y, markLabelSpacer,chrWidth,dfMarkColorInternal,allMarkMaxSize,normalizeToOne,
                      markLabelSize,xfactor)
    }


}# end of function
