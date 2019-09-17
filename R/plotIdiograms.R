#' Function to plot idiograms of karyotypes with centromere
#' @description This function reads a dataframe  with columns: \code{chrName}
#' \code{shortArmSize} and \code{longArmSize} and plots as idiograms. If more
#' than one species, a column named \code{OTU} is needed. For Mb use preferably
#' \code{plotIdiogramsHolo}
#'
#' @description Optionally, it reads another dataframe with the position of
#' marks (sites) \code{dfMarkPos}, in which case a dataframe for mark color is
#' necessary \code{dfMarkColor}
#'
#' @param dfChrSize mandatory dataframe, with columns: OTU (optional), chrName,
#'   shortArmSize, longArmSize
#' @param dfMarkPos dataframe of marks (sites): cols: OTU (opt) chrName,
#'   markName (name of site), markArm, markDistCen, markSize; column markArm:
#'   use p for short and q for long; col markDistCen: use distance from
#'   centromere to mark
#' @param dfCenMarks dataframe, specific for centromeric marks. cols: chrName
#'   and markName
#' @param dfMarkColor dataframe, optional, specifying colors and style for marks (sites);
#'   cols: \code{markName}, \code{markColor}, \code{style}: \code{square} or \code{dots}. (if \code{style} missing: all \code{square})
#' @param mycolors character vector, optional, i.e. \code{c("blue","red","green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the
#' order of your marks use something like: \code{unique(c(dfMarkPos$markName,dfCenMarks$markName) ) }
#' @param MarkDistanceType character, if \code{cen} = the distance you provided is to
#'   the center of the mark, if \code{beg} = the distance you provided is to the
#'   beginning of the mark
#' @param orderBySize logical value, when \code{TRUE}, sorts chromosomes by total
#'   length from the largest to the smallest
#' @param centromereSize numeric, this establishes the apparent size of cen in
#'   the plot in \eqn{\mu}m
#' @param chrWidth numeric, chromosome width
#' @param chrSpacing numeric, horizontal spacing among chromosomes
#' @param chrColor character, main color for chromosomes
#' @param cenColor character, color for centromeres
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
#' @param chrId character, print name of chromosme, \code{"original"} uses the original
#'   name in OTU column of dfChrSize, \code{"simple"} (just 1 to ...) or \code{""} (none).
#' @param distTextChr numeric, distance from name of chromosome to chromosome,
#'   also affects vertical separation of indices
#' @param indexIdTextSize numeric, font size of chr. and kar. indices and
#'   chromosome name
#' @param OTUTextSize numeric, font size of OTU name (species)
#' @param legend character, "" for no legend; "inline" prints labels near
#'   chromosomes; "aside" prints legend to the right of karyotypes
#' @param markLabelSize numeric, only if legend != "", size of the text of
#'   labels of marks (legend)
#' @param markLabelSpacer numeric, only if legend="aside", space from the
#'   rightmost chr. to legend
#' @param pattern REGEX pattern to remove from names of marks
#' @param chrIndex logical, add arm ratio and centromeric index
#' @param nameChrIndexPos numeric, modify position of name of chr. indices
#' @param karIndex logical, add karyotype indices A (intrachromosomal -
#'   centromere pos.) and A2 (interchromosomal asymmetry, variation among
#'   chromosome sizes)
#' @param karIndexPos numeric, move karyotype index
#' @param morpho boolean, if TRUE prints the Guerra and Levan classif of cen.
#'   position. see ?armRatioCI
#' @param addOTUName boolean, if TRUE adds OTU (species) name to karyotype
#' @param revOTUs boolean, The order of species is the one in the main
#'   dataframe, use \code{TRUE} to reverse
#' @param ruler boolean, display ruler to the left of karyotype, when FALSE no ruler
#' @param rulerPos numeric, absolute position of ruler, corresponds to pos
#'   argument of axis R plot
#' @param rulerPosMod numeric, modify position of ruler, corresponds to line
#'   argument of axis R plot
#' @param ruler.tck numeric, tick size of ruler, corresponds to tck argument of
#'   axis R plot
#' @param rulerNumberPos numeric, modify position of numbers of ruler
#' @param rulerNumberSize numeric, size of number's font in ruler
#' @param xlimLeftMod numeric, modifies \code{xlim} left argument of plot
#' @param xlimRightMod numeric, \code{xlim} modification by adding space to the right
#'   of idiograms
#' @param ylimBotMod numeric, modify \code{ylim} bottom argument of plot
#' @param ylimTopMod numeric, modify \code{ylim} top argument of plot
#' @param ... accepts other arguments for the plot, such as, \code{asp}
#' @param lwd.chr thick of border of chr. and marks.
#'
#' @keywords dataframe chromosome
#'
#' @importFrom graphics par plot segments
#'
#' @export
#'
#' @examples
#' data(dfOfChrSize)
#' plotIdiograms(dfOfChrSize)
#' @seealso \code{\link{plotIdiogramsHolo}}
#' @seealso \code{\link{asymmetry}}
#' @seealso \code{\link{armRatioCI}}
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{markpos}}
#' @seealso \code{\link{cenmarkdata}}
#' @seealso \code{\link{dfMarkColor}}
#'
#' @return plot

plotIdiograms<-function(dfChrSize, dfMarkPos, dfCenMarks, dfMarkColor, mycolors, MarkDistanceType="beg",orderBySize=TRUE,
                        centromereSize =1, chrWidth=1.5, chrSpacing=1.5,chrColor="gray", cenColor="gray",
                        roundness=4, dotRoundCorr=1.5,
                        karHeight=1.2,karSpacing=1.6,reduDistKar=TRUE,reduDistKarTol=9,
                        chrId="original", distTextChr=.3,
                        indexIdTextSize=.4, OTUTextSize=.6,
                        legend="inline", markLabelSize=.4, markLabelSpacer=2,
                        chrIndex=TRUE, nameChrIndexPos=2, karIndex=TRUE, karIndexPos=.5,
                        morpho=TRUE,
                        addOTUName=TRUE,revOTUs=FALSE,
                        ruler=TRUE,rulerPos=-.5, rulerPosMod=0, ruler.tck=-0.004, rulerNumberPos=.2, rulerNumberSize=.4,
                        xlimLeftMod=1,  xlimRightMod=10, ylimBotMod=.2,ylimTopMod=.2,
                        lwd.chr=2, pattern="",
                        ...)
{
  if(!missing(dfChrSize)){
    dfChrSizeInternal<-makeNumCols(dfChrSize)

  } else {
    message(crayon::red("Missing mandatory dfChrSize dataframe"))
    return(NA)
  }
  if(!missing(dfMarkPos)){
    dfMarkPosInternal<-makeNumCols(dfMarkPos)
  } # df of marks
  # str(dfMarkPos)
  if(!missing(dfCenMarks)){
    dfCenMarksInternal<-makeNumCols(dfCenMarks)
  }
  if(!missing(dfMarkColor)){
    dfMarkColorInternal<-makeNumCols(dfMarkColor)
  }

  message(crayon::black(paste("Making checks\n")) )
  message(crayon::black(paste("In case of error see messages and the help ?functionName\n")) )

  #
  #   dfChrSizeInternal
  #

  if(exists("dfChrSizeInternal")){
    message(crayon::black("\nChecking mandatory columns from dfChrSize: chrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
    ) ) # cat
    if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                       colnames(dfChrSizeInternal) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "shortArmSize","longArmSize"),
                                      colnames(dfChrSizeInternal) ) ), sep="\n", collapse = " " )
      )
      )# cat
      message(crayon::red(paste("\nwill quit now\n") )
      )#c
      return(NA)
    } # fi
    else { # if no error
      message(crayon::black("\nOK\n") )
      dfChrSizeInternal$shortArmSize<-as.numeric(dfChrSizeInternal$shortArmSize)
      dfChrSizeInternal$longArmSize <-as.numeric(dfChrSizeInternal$longArmSize)
    }
  } # fi

  #
  #   dfMarkPosInternal
  #

  if(exists("dfMarkPosInternal")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos: chrName, markName, markArm,markDistCen,markSize\n (column OTU  is necessary if more than one species)\n"
    ) )# cat
    if(length (setdiff(c("chrName", "markName", "markArm","markDistCen","markSize"),
                       colnames(dfMarkPosInternal) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName", "markArm","markDistCen","markSize"),
                                      colnames(dfMarkPosInternal) ) ) , sep="\n", collapse = " " )
      )
      ) # cat
      message(crayon::red("\nERRORS PRESENT, see above, dfMarksPos REMOVED\n")
      )#m
      remove(dfMarkPosInternal)
    } # fi
    else { # if no error
      message(crayon::black("\nOK\n")
              ) #m
      dfMarkPosInternal$markDistCen<-as.numeric(dfMarkPosInternal$markDistCen)
      dfMarkPosInternal$markSize<-as.numeric(dfMarkPosInternal$markSize)

      if(MarkDistanceType=="cen"){
        dfMarkPosInternal$markDistCen<-dfMarkPosInternal$markDistCen-(dfMarkPosInternal$markSize/2)
      } # if

    } # else
  } # fi

  #
  #   dfCenMarksInternal
  #

  if(exists("dfCenMarksInternal")){
    message(crayon::black("\nChecking mandatory columns from dfCenMarks: chrName, markName\n")
    ) # cat
    if(length(setdiff(c("chrName", "markName"),
                      colnames(dfCenMarksInternal) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName"),
                                      colnames(dfCenMarksInternal) ) ), sep="\n", collapse = " " )
      )
      )# cat
      message(crayon::red("\nERRORS PRESENT, see above, dfCenMarks REMOVED\n") )
      remove(dfCenMarksInternal)
    } # fi
    else { # if no error
      message(crayon::black("\nOK\n") )
    }
  } # fi

  #
  #   checkNameChrDfMarks
  #

  if(exists("dfMarkPosInternal")){
    message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n") )
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
  #   check OTU and chr names among dfcenmarks and dfchrsize
  #

  if(exists("dfCenMarksInternal")) {
    message(crayon::black("\n####\ndfCenMarks exists, if error will be removed\n") )
    FstCheckCen<-checkNameOTUdfMarks(dfChrSizeInternal,dfMarks=dfCenMarksInternal)
    if(FstCheckCen){
      SndCheckCen<-checkNameChrDfMarks(dfChrSizeInternal,dfMarks=dfCenMarksInternal)
      if(SndCheckCen){
        if(exists("allMarkNames")){
          allMarkNames<-unique(c(allMarkNames,dfCenMarksInternal$markName) )
        } else {
          allMarkNames<-unique(dfCenMarksInternal$markName)
        } # allmarkname check
      } else { # if sndcheck == FALSE
        message(crayon::red(paste("\nError in checks done for df of cen marks, it will not be plotted, dfCenMarks REMOVED\n")
        )
        )#cat
        remove(dfCenMarksInternal)
      } #else
    } # fst
    else {
      message(crayon::red(paste("\nError in checks done for df of cen marks, it will not be plotted, dfCenMarks REMOVED\n")
      )
      )#cat
      remove(dfCenMarksInternal)
    }
  } # exists dfCenMarksInternal

  #
  #   check compatibility of columns dfMarkColor
  #

  if(exists("dfMarkColorInternal") ) {
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
      )# cat
      remove(dfMarkColorInternal)
      message(crayon::red("\nError in dfMarkColor, REMOVED\n") )
    } #fi
    else { # column names ok
      if(exists("allMarkNames")){
        dfMarkColorInternal<-dfMarkColorInternal[which(dfMarkColorInternal$markName %in% allMarkNames) ,]
        if (nrow(dfMarkColorInternal)==0){
          message(crayon::red("\nError in dfMarkColor Names respect to Marks dataframes, dfMarkColor REMOVED\n")
          )#cat
          remove(dfMarkColorInternal)
        } else { # nrow not 0
          message(crayon::black("\nCheck OK\n") )
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
  #   after 1.0.0
  ######################################################################################################
  else if (missing(mycolors) ) { # if dfMarkColor not exist and missing mycolors
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColor(allMarkNames,c(chrColor,cenColor) )
      } # allmarknames
  } else if (!missing(mycolors) ){ # if dfMarkColor not exist if mycolors exist
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColorMycolors(allMarkNames,c(chrColor,cenColor), mycolors )
    }
  } # elif
 # else

  #
  #   CREATION OF CHILD DATAFRAMES MARKS
  #
  ###################
  # REQUIRES STYLE
  ###################

  if(exists("dfMarkPosInternal") & exists("dfMarkColorInternal")  ){
    if(FstCheck & SndCheck){
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

  #
  # generate Chromosome indexes
  #

  if(chrIndex | morpho){
    dfChrSizeInternal<-armRatioCI(dfChrSizeInternal)
  }

  #################################################################### LISTS
  #
  # transform dfs to list of df dfChrSizeInternal
  #

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

  #
  #	change of size based on number of sps
  #

  {
    chrWidth<-chrWidth/length(listOfdfChromSize)
    chrSpacing<-chrSpacing/length(listOfdfChromSize)
  }  # dfMarkPosSq to listOfdfMarkPosInternalSq

  #
  # transform dfs to list of df dfMarkPosSq
  #

  if (exists("dfMarkPosSq")){

    if("OTU" %in% colnames(dfMarkPosSq)){
      listOfdfMarkPosSq<-base::split(dfMarkPosSq, dfMarkPosSq$OTU )
      names(listOfdfMarkPosSq)<-unique(dfMarkPosSq$OTU)
    } else {
      listOfdfMarkPosSq<-list(dfMarkPosSq)
      names(listOfdfMarkPosSq)<-1
    }
  } # end missing dfMarkPosSq

  #
  # transform dfs to list of df dfMarkPosCr
  #
  if (exists("dfMarkPosCr")){

    if("OTU" %in% colnames(dfMarkPosCr)){
      listOfdfMarkPosCr<-base::split(dfMarkPosCr, dfMarkPosCr$OTU )
      names(listOfdfMarkPosCr)<-unique(dfMarkPosCr$OTU)
    } else {
      listOfdfMarkPosCr<-list(dfMarkPosCr)
      names(listOfdfMarkPosCr)<-1
    }
  } # end missing dfMarkPosSq

  #
  # transform dfs to list of df dfCenMarksInternal
  #

  if (exists("dfCenMarksInternal")){

    if("OTU" %in% colnames(dfCenMarksInternal)){
      listOfdfDataCen<-base::split(dfCenMarksInternal, dfCenMarksInternal$OTU )
    } else {
      listOfdfDataCen<-list(dfCenMarksInternal)
      names(listOfdfDataCen)<-1
    }
  } # end missing dfCenMarksInternal


  #######################################################
  #
  #   If Marks missing, rename duplicates of chrNames
  #
  #######################################################

  mybooleanChrName <- !exists("listOfdfMarkPosSq") & !exists("listOfdfMarkPosCr") & !exists("listOfdfDataCen")

  listOfdfChromSize<-fixChrNameDup(listOfdfChromSize, mybooleanChrName)

  #####################
  #   total size of chr
  #####################
  {
    shortArmSize<-lapply(listOfdfChromSize, function(x) x$shortArmSize)
    longArmSize <-lapply(listOfdfChromSize, function(x) x$longArmSize )
    totalLengh <- mapply(function(x,y) x+y, x=shortArmSize,y=longArmSize)
    ifelse(class(totalLengh)=="matrix",
           totalLengh <- base::split(totalLengh, col(totalLengh))
           ,NA)
    #
    #	standardize height of karyotypes
    #
    normalizeToOne<-karHeight/max(unlist(totalLengh) , na.rm=TRUE)
  }

  #
  #   add column total to dataframes
  #

  #####################
  # order by size
  #####################
  {
    if(orderBySize==TRUE) {
        orderlist<-lapply(totalLengh, function(x) order(x, decreasing = TRUE) )
    } else { # if not want to order by size, set order by name of chro
        orderlist<-lapply(listOfdfChromSize, function(x) order(x$chrName) )
    } #else

    ##################################################
    #
    #   add column of new chro index to dataframes
    #
    ##################################################

    for (s in 1:length(listOfdfChromSize)){
      listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][orderlist[[s]], ]
      listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
      #
      # listOfdfChromSize[[s]]$neworder <- sapply(1:(nrow(listOfdfChromSize[[s]])), function(x) grep(paste0("^",x,"$"), orderlist[[s]]) )
      #
    } # end for

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
    ##################################################
    #
    #   important - add new indexer to df of marks
    #
    ##################################################
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
        # s<-1
        selecteddfChromData<-which(names(listOfdfMarkPosCr)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfChromData)>0){
          listOfdfMarkPosCr[[selecteddfChromData]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfMarkPosCr[[selecteddfChromData]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      }
    } # end if presence of dfMarkPosSq order

    ######################################################
    #
    #   important - add new indexer to df DataCen
    #
    ######################################################

    if (exists("dfCenMarksInternal")){

      for (s in 1:length(listOfdfChromSize)){
        selecteddfDataCen<-which(names(listOfdfDataCen)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfDataCen)>0){
          listOfdfDataCen[[selecteddfDataCen]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfDataCen[[selecteddfDataCen]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      } # end for
    }  # end presence of dfCenMarksInternal
  } # ordering chunk

  #
  # ordering and transformation (normalize height)
  #

  # {
  #   shortArmSizeOrdered<-mapply(function(x,y) x[y], x=shortArmSize, y=orderlist)
  #   # if
  #   # shortArmSizeOrdered<-mapply(function(x,y) x[y], x=shortArmSizeOrdered, y=orderlist)
  #
  #   ifelse(class(shortArmSizeOrdered)=="matrix",
  #          shortArmSizeOrdered <- base::split(shortArmSizeOrdered, col(shortArmSizeOrdered))
  #          ,NA)
  #   shortArmSizeOrdered<-lapply(shortArmSizeOrdered, function(x) x*normalizeToOne)
  #
  #   longArmSizeOrdered<-mapply(function(x,y) x[y], x=longArmSize , y=orderlist)
  #   ifelse(class(longArmSizeOrdered)=="matrix",
  #          longArmSizeOrdered <- base::split(longArmSizeOrdered, col(longArmSizeOrdered))
  #          ,NA)
  #   longArmSizeOrdered<-lapply(longArmSizeOrdered, function(x) x*normalizeToOne)
  # }

  #######################
  #
  #      main plot
  #
  #######################

  {
    chromosome_ns<-sapply(listOfdfChromSize, function(x) nrow(x) )
    arms_number<-sapply(chromosome_ns, function(x) x*2)
    num_species<-length(listOfdfChromSize)
  }

  { # processing for main plot
    rownumber<-2
    armRepVector<-lapply(arms_number, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))
    x<-list()
    y<-list()
    xm<-list()
    ym<-list()

    for (s in 1:num_species){
      croybot<-list()
      croytop<-list()
      croxleft<-list()
      croxright<-list()
      for (i in 1:length(armRepVector[[s]])){
        croybot[i]<-list(c(karHeight,
                           # rep((karHeight-longArmSizeOrdered[[s]][i]),2),
                           rep((karHeight-(listOfdfChromSize[[s]][,"longArmSize"]*normalizeToOne)[i]),2),
                           karHeight
                          )#c
                        )# list
        # croytop[i]<-list(c((karHeight+(centromereSize*normalizeToOne)+shortArmSizeOrdered[[s]][i]),
        croytop[i]<-list(c((karHeight+(centromereSize*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] ),
                           rep((karHeight+(centromereSize*normalizeToOne)),2),
                           (karHeight+(centromereSize*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] )
                           # (karHeight+(centromereSize*normalizeToOne)+shortArmSizeOrdered[[s]][i])
        ) # c
        ) # list
      } # for

      crox<-matrix(rep(NA,length(armRepVector[[s]])*4),ncol=4, nrow=length(armRepVector[[s]]) )

      for (i in 1:length(armRepVector[[s]])){
        crox[i,] <- c(
          rep( ( (chrSpacing*(i-1)+((i-0)*chrWidth))+chrWidth),2),
          rep( ( (chrSpacing*(i-1)+((i-0)*chrWidth))     ),2)
        ) # c rox
      } # for

      xm[[s]] <- rbind(crox,crox)
      x[[s]]  <- base::split(xm[[s]], row(xm[[s]]))
      # x<-x
      ifelse(any(is.na(x[[s]]) ),x[[s]]<-NA,"")
      ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)}) ) + (karSpacing*(s-1)  )
    } # for species

    ymCopyC<-ymCopy<-ym

    if (length(ym)==1){
      reduDistKar=FALSE
    }

    #
    #	reducing distance among OTUs
    #

    if(reduDistKar){
      for (s in 1:(length(ym)-1)) {
        diffnext<-abs(min(ym[[s+1]] ) - max(ym[[s]]) )
        diffnext<-(diffnext-((distTextChr/3)*reduDistKarTol))
        ym[[s+1]]=ym[[s+1]]-diffnext
      }
    }

    names(ym)<-names(listOfdfChromSize)
    y<-lapply(1:length(ym), function(s) base::split(ym[[s]], row(ym[[s]] )) )

    #####################3
    #   main  plot DO
    #######################

    if(!karIndex){
      xlimLeftMod<-xlimLeftMod*(1/3)
    }

    leftmodifier<-(xlimLeftMod*karIndexPos)+chrWidth

    #####################################################################################################################

    graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-leftmodifier),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                   ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) , ylab = "", xaxt='n',
                   xlab="", yaxt='n',main = NULL, frame.plot = FALSE, ...)
    # xlab="", yaxt='n',main = NULL, frame.plot = FALSE)

    ######################################################################################################################
    xsizeplot<-(max(unlist(x), na.rm=TRUE)+xlimRightMod )- ( (min(unlist(x), na.rm=TRUE)-(leftmodifier)) )
    ysizeplot<- max(unlist(y) )
    yfactor<-(ysizeplot/xsizeplot)*dotRoundCorr

    { # plot types
      if(roundness<1){
        roundness<-1
      }
      if(roundness>15){
        lapply(1:length(y), function(z) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrColor),
                                               x=x[[z]], y=y[[z]]
        )#m
        )#l
      } else {
        if (centromereSize>0){

        n2<-25
        pts_1 <- seq(-pi/2, 0, length.out = n2)
        pts_2 <- seq( 0, pi/2, length.out = n2)
        pts_3 <- seq(pi, pi*1.5, length.out = n2)
        pts_4 <- seq(pi/2, pi, length.out = n2)
        r2 <- chrWidth/(roundness*2)
        yMod<-y

        topBotline_x<-list()
        x2_1<-list()
        x2_2<-list()

        for (s in 1:length(yMod) ) {
          topBotline_x[[s]]<-list()
          x2_1[[s]]<-list()
          x2_2[[s]]<-list()
          for (c in 1 : length(yMod[[s]]) ) {
            topBotline_x[[s]][[c]]<-c(min(x[[s]][[c]])+r2,max(x[[s]][[c]])-r2 )
            x2_1[[s]][[c]]<-min(x[[s]][[c]])+r2
            x2_2[[s]][[c]]<-max(x[[s]][[c]])-r2
          } # f
        } # f

        y2<-list()
        topline_y<-list()
        xy_1<-list()
        xy_2<-list()
        newLongx<-list()
        newLongy<-list()
        bottomline_y<-list()

        for (s in 1:length(yMod) ) {
          topline_y[[s]]<-list()
          y2[[s]]<-list()
          xy_1[[s]]<-list()
          xy_2[[s]]<-list()
          newLongx[[s]]<-list()
          newLongy[[s]]<-list()
          bottomline_y[[s]]<-list()
          for (p in ( (length(yMod[[s]])/2)+1 ): length(yMod[[s]]) ) {
            topline_y[[s]][[p]]<-rep(max(yMod[[s]][[p]]),2)
            y2[[s]][[p]]<-max(y[[s]][[p]])-r2*yfactor
            # xy_1 <- cbind(x2_1 + r2 * sin(pts_1), y2 + r2 * cos(pts_1))
            xy_1[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_1)*1, y2[[s]][[p]] + (r2 * cos(pts_1)*yfactor) )
            # xy_2 <- cbind(x2_2 + r2 * sin(pts_2), y2 + r2 * cos(pts_2))
            xy_2[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_2)*1, y2[[s]][[p]] + (r2 * cos(pts_2)*yfactor) )
            # yMod[which(yMod==max(yMod))]<-yMod[which(yMod==max(yMod))]-r2
            yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]-r2*yfactor
            # newLongx<-c(x,xy_1[,1],topBotline_x,xy_2[,1])
            newLongx[[s]][[p]]<-c(x[[s]][[p]],xy_1[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_2[[s]][[p]][,1] )
            # newLongy<-c(yMod,xy_1[,2],topline_y,xy_2[,2])
            newLongy[[s]][[p]]<-c(yMod[[s]][[p]],xy_1[[s]][[p]][,2],topline_y[[s]][[p]],xy_2[[s]][[p]][,2] )
          } # for
          for (q in 1: (length(yMod[[s]])/2 ) ) {
            bottomline_y[[s]][[q]]<-rep(min(yMod[[s]][[q]]),2)
            y2[[s]][[q]]<-min(y[[s]][[q]])+r2*yfactor
            xy_1[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_3)*1, y2[[s]][[q]] + (r2 * cos(pts_3)*yfactor) )
            # xy_2 <- cbind(x2_2 + r2 * sin(pts_4), y2 + r2 * cos(pts_4) )
            xy_2[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_4)*1, y2[[s]][[q]] + (r2 * cos(pts_4)*yfactor) )
            # yMod[which(yMod==min(yMod))]<-yMod[which(yMod==min(yMod))]+r2
            yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]+r2*yfactor
            # newLongx<-c(x[1:2],xy_2[,1],topBotline_x,xy_1[,1], x[3:4])
            newLongx[[s]][[q]]<-c(x[[s]][[q]][1:2],xy_2[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_1[[s]][[q]][,1],x[[s]][[q]][3:4] )
            # newLongy<-c(yMod[1:2],xy_2[,2],bottomline_y,xy_1[,2], yMod[3:4])
            newLongy[[s]][[q]]<-c(yMod[[s]][[q]][1:2],xy_2[[s]][[q]][,2],bottomline_y[[s]][[q]],xy_1[[s]][[q]][,2],yMod[[s]][[q]][3:4] )
          } # for
        } # for

        # polygon(newLongx,newLongy, col="red")
        lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrColor),
                                               x=newLongx[[s]], y=newLongy[[s]]
        )#m
        )#l
      } # else if cen > 0
       # new else centromere

      else { # cen ==0

        n2<-25
        pts_1 <- seq(-pi/2, 0, length.out = n2)
        pts_2 <- seq( 0, pi/2, length.out = n2)
        pts_3 <- seq(pi, pi*1.5, length.out = n2)
        pts_4 <- seq(pi/2, pi, length.out = n2)

        r2 <- chrWidth/(roundness*2)
        yMod<-y

        topBotline_x<-list()
        x2_1<-list()
        x2_2<-list()
        # y2<-list()
        topline_y<-list()
        xy_1<-list()
        xy_2<-list()
        newLongx<-list()
        newLongy<-list()
        bottomline_y<-list()

        # NEW round up and down
        y2_1<-list()
        y2_2<-list()
        xy_3<-list()
        xy_4<-list()

        for (s in 1:length(yMod) ) {
          topBotline_x[[s]]<-list()
          x2_1[[s]]<-list()
          x2_2[[s]]<-list()
          for (c in 1 : length(yMod[[s]]) ) {
            topBotline_x[[s]][[c]]<-c(min(x[[s]][[c]])+r2,max(x[[s]][[c]])-r2 )
            x2_1[[s]][[c]]<-min(x[[s]][[c]])+r2
            x2_2[[s]][[c]]<-max(x[[s]][[c]])-r2
          } # f
        } # f



        for (s in 1:length(yMod) ) {
          topline_y[[s]]<-list()

          # y2[[s]]<-list()

          y2_1[[s]]<-list() # NEW
          y2_2[[s]]<-list() # NEW

          xy_1[[s]]<-list()
          xy_2[[s]]<-list()

          xy_3[[s]]<-list() # NEW
          xy_4[[s]]<-list() # NEW

          newLongx[[s]]<-list()
          newLongy[[s]]<-list()
          bottomline_y[[s]]<-list()

          # x2_1[[s]]<-list() #new
          # x2_2[[s]]<-list() # new



          for (p in ( (length(yMod[[s]])/2)+1 ): length(yMod[[s]]) ) {
            bottomline_y[[s]][[p]]<-rep(min(yMod[[s]][[p]]),2)

            topline_y[[s]][[p]]<-rep(max(yMod[[s]][[p]]),2)
            # y2[[s]][[p]]<-max(y[[s]][[p]])-r2*yfactor

            y2_1[[s]][[p]]<-max(y[[s]][[p]])-r2*yfactor
            y2_2[[s]][[p]]<-min(y[[s]][[p]])+r2*yfactor

            # xy_1 <- cbind(x2_1 + r2 * sin(pts_1), y2 + r2 * cos(pts_1))
            xy_1[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_1)*1, y2_1[[s]][[p]] + (r2 * cos(pts_1) *yfactor) )
            # xy_2 <- cbind(x2_2 + r2 * sin(pts_2), y2 + r2 * cos(pts_2))
            xy_2[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_2)*1, y2_1[[s]][[p]] + (r2 * cos(pts_2) *yfactor) )

            xy_3[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_3), y2_2[[s]][[p]] + (r2 * cos(pts_3) *yfactor) ) # new
            # xy_4 <- cbind(x2_2 + r2 * sin(pts_4), y2_2 + r2 * cos(pts_4))
            xy_4[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_4), y2_2[[s]][[p]] + (r2 * cos(pts_4) *yfactor) ) # new

            # yMod[which(yMod==max(yMod))]<-yMod[which(yMod==max(yMod))]-r2
            yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]-r2*yfactor
            # yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]<-yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]-r2*yfactor
            # yMod[which(yMod==min(yMod))]<-yMod[which(yMod==min(yMod))]+r2
            yMod[[s]][[p]][which(yMod[[s]][[p]]==min(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==min(yMod[[s]][[p]]))]+r2*yfactor
            # newLongx<-c(x,xy_1[,1],topBotline_x,xy_2[,1])
          # newLongx[[s]][[p]]<-c(x[[s]][[p]],     xy_1[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_2[[s]][[p]][,1] )

            newLongx[[s]][[p]]<-c(x[[s]][[p]][1:2],xy_4[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_3[[s]][[p]][,1],
                                  x[[s]][[p]][3:4],xy_1[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_2[[s]][[p]][,1])

            # newLongy<-c(yMod,xy_1[,2],topline_y,xy_2[,2])
          # newLongy[[s]][[p]]<-c(yMod[[s]][[p]],     xy_1[[s]][[p]][,2],topline_y[[s]][[p]],xy_2[[s]][[p]][,2] )

            newLongy[[s]][[p]]<-c(yMod[[s]][[p]][1:2],xy_4[[s]][[p]][,2],bottomline_y[[s]][[p]],xy_3[[s]][[p]][,2],
                                  yMod[[s]][[p]][3:4],xy_1[[s]][[p]][,2],topline_y[[s]][[p]],xy_2[[s]][[p]][,2])

          } # for
          for (q in 1: (length(yMod[[s]])/2 ) ) {
            bottomline_y[[s]][[q]]<-rep(min(yMod[[s]][[q]]),2)
            topline_y[[s]][[q]]   <-rep(max(yMod[[s]][[q]]),2)

            # y2[[s]][[q]]<-min(y[[s]][[q]])+r2*yfactor

            y2_1[[s]][[q]]<-max(y[[s]][[q]])-r2*yfactor# new
            y2_2[[s]][[q]]<-min(y[[s]][[q]])+r2*yfactor# new

            xy_1[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_1)*1, y2_1[[s]][[q]] + (r2 * cos(pts_1)*yfactor) )
            # xy_2 <- cbind(x2_2 + r2 * sin(pts_4), y2 + r2 * cos(pts_4) )
            xy_2[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_2)*1, y2_1[[s]][[q]] + (r2 * cos(pts_2)*yfactor) )

            xy_3[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_3), y2_2[[s]][[q]] + (r2 * cos(pts_3) *yfactor) ) # new
            # xy_4 <- cbind(x2_2 + r2 * sin(pts_4), y2_2 + r2 * cos(pts_4))
            xy_4[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_4), y2_2[[s]][[q]] + (r2 * cos(pts_4) *yfactor) ) # new

            # yMod[which(yMod==min(yMod))]<-yMod[which(yMod==min(yMod))]+r2
            yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]+r2*yfactor
            yMod[[s]][[q]][which(yMod[[s]][[q]]==max(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==max(yMod[[s]][[q]]))]-r2*yfactor

            # newLongx[[s]][[q]]<-c(x[[s]][[q]][1:2],xy_2[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_1[[s]][[q]][,1],x[[s]][[q]][3:4] )
          # newLongy[[s]][[q]]<-c(yMod[[s]][[q]][1:2],xy_2[[s]][[q]][,2],bottomline_y[[s]][[q]],xy_1[[s]][[q]][,2], yMod[[s]][[q]][3:4] )

            newLongx[[s]][[q]]<-c(x[[s]][[q]][1:2],xy_4[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_3[[s]][[q]][,1],
                                  x[[s]][[q]][3:4],xy_1[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_2[[s]][[q]][,1])

            newLongy[[s]][[q]]<-c(yMod[[s]][[q]][1:2],xy_4[[s]][[q]][,2],bottomline_y[[s]][[q]],xy_3[[s]][[q]][,2],
                                  yMod[[s]][[q]][3:4],xy_1[[s]][[q]][,2],topline_y[[s]][[q]],xy_2[[s]][[q]][,2])

          } # for

        } # for

        # polygon(newLongx,newLongy, col="red")
        lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrColor),
                                               x=newLongx[[s]], y=newLongy[[s]]
        )#m
        )#l
      } # else cen == 0

      } # roundness not > 15


    } # plot types

  } # end main plot

  ################################
  #
  #   ruler calculate
  #
  ################################

  if (ruler){
    maxShortRound<-lapply(1:length(listOfdfChromSize), function(x) ceiling(max(listOfdfChromSize[[x]]$shortArmSize)) )
    maxLongRound<-lapply(1:length(listOfdfChromSize), function(x) ceiling(max(listOfdfChromSize[[x]]$longArmSize)) )

    fromZerotoMaxShort<-lapply(maxShortRound, function(x) seq(0,x, by=1) )
    fromZerotoMaxLong<-lapply(maxLongRound, function(x) seq(0,x, by=1) )

    ycoordLongRound <-lapply(1:length(fromZerotoMaxLong), function(x)
      unlist(
        lapply(1:length(fromZerotoMaxLong[[x]]), function(y) (karHeight-(fromZerotoMaxLong[[x]][y]*normalizeToOne))+(1*karSpacing*(x-1))
        ) #l
      ) #u
    )#l

    ycoordShortRound  <-lapply(1:length(fromZerotoMaxShort), function(x)
      unlist(
        lapply(1:length(fromZerotoMaxShort[[x]]), function(y)
          (karHeight+(centromereSize*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karSpacing*(x-1))
        )# l
      )# u
    )#l

    if(reduDistKar){
      for (s in 1:(length(ymCopy)-1) ) {
        diffnext<-abs(min(ymCopy[[s+1]] ) - max(ymCopy[[s]]) )
        diffnext<-(diffnext-((distTextChr/3)*reduDistKarTol))
        ymCopy[[s+1]]=ymCopy[[s+1]]-diffnext
        ycoordLongRound[[s+1]] <-ycoordLongRound[[s+1]] -diffnext
        ycoordShortRound[[s+1]]<-ycoordShortRound[[s+1]]-diffnext
      }
    }

    ####################
    #
    #   add rulers
    #
    ####################

    ###############
    # short arm ruler
    ###############
    opar<-graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(opar) ))
    graphics::par(mgp=c(3,rulerNumberPos,0))
    lapply(1:length(ycoordShortRound), function(x)
      graphics::axis(side=2, at=unlist(ycoordShortRound[[x]]),
                     labels = unlist(fromZerotoMaxShort[[x]] ),
                     cex.axis=rulerNumberSize
                     ,las=1
                     ,line=rulerPosMod
                     ,pos= rulerPos
                     ,tck=ruler.tck
      ) # axis
    ) # l

    ################
    # long arm ruler
    ################
    lapply(1:length(ycoordLongRound), function(x)
      graphics::axis(side=2, at=unlist(ycoordLongRound[[x]]),
                     labels = unlist(fromZerotoMaxLong[[x]])
                     ,cex.axis=rulerNumberSize
                     ,las=1
                     ,line=rulerPosMod
                     ,pos= rulerPos
                     ,tck=ruler.tck
      )
    ) # l
  }   # end rulers if

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
  #  chromosome names
  #################################

  chrNameDistance<-ifelse(exists("grouporderlist"),3,1)
  # original
  if(chrId=="original"){
    lapply(1:length(xm), function(i)
      graphics::text(xm[[i]][,3][1:(nrow(xm[[i]])/2)]+chrWidth/2,
                     rep( (min(ym[[i]])-(distTextChr/3)*chrNameDistance ),(nrow(xm[[i]])/2) ),
                     # labels = listOfdfChromSize[[i]][,"chrName"][orderlist[[i]]],
                     labels = listOfdfChromSize[[i]][,"chrName"],

                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  } # fi
  else if (chrId=="simple"){
    # Simple numbering from 1 to ...
    lapply(1:length(xm), function(i)
      graphics::text(xm[[i]][,3][1:(nrow(xm[[i]])/2)]+chrWidth/2,
                     rep( (min(ym[[i]])-(distTextChr/3)*chrNameDistance ),(nrow(xm[[i]])/2) ),
                     # rep( (min(ym[[i]])-.1),(nrow(xm[[i]])/2) ),
                     labels = 1:(nrow(xm[[i]])/2),
                     cex=indexIdTextSize
      ) # t
    ) # lapply
  } # elif

  #################################
  # horizontal chromosome index
  #################################

  chrIndDistance<-ifelse(exists("grouporderlist"),4,2)

  chrIndbool<-(chrIndex & "AR" %in% colnames(listOfdfChromSize[[1]]) )
  if(chrIndbool){
    lapply(1:length(xm), function(s)
      graphics::text(c(xm[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xm[[s]][,3][1:(nrow(xm[[s]])/2)]+(chrWidth/2)),
                     rep( (min(ym[[s]])-( ( (distTextChr/3)*chrIndDistance)) ),(nrow(xm[[s]])/2)+1 ),
                     # labels = c("CI",listOfdfChromSize[[s]][,"CI"][orderlist[[s]]]),
                     labels = c("CI",listOfdfChromSize[[s]][,"CI"] ),

                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
    lapply(1:length(xm), function(s)
      graphics::text(c(xm[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xm[[s]][,3][1:(nrow(xm[[s]])/2)]+chrWidth/2),
                     rep( (min(ym[[s]])-( ( (distTextChr/3)*(chrIndDistance+1) )) ),(nrow(xm[[s]])/2)+1 ),
                     # labels = c("r",listOfdfChromSize[[s]][,"AR"][orderlist[[s]]] ),
                     labels = c("r",listOfdfChromSize[[s]][,"AR"] ),
                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  } # fi

  #################################
  # horizontal chromosome morphology categories
  #################################

  chrIndboolGue<-(morpho & "AR" %in% colnames(listOfdfChromSize[[1]]) )

  # distVectorGue<-ifelse(exists("grouporderlist"),c(6,0,4,0),c(4,0,2,0) ) # does not work

  distVectorGue<-if(exists("grouporderlist") ){
    a<-c(6,0,4,0)
  } else {
    a<-c(4,0,2,0)
  }

  decVector<-ifelse(chrIndboolGue & chrIndex,1, # chrind and Guerra
                    ifelse(chrIndboolGue==FALSE & chrIndbool,2, # only chrInd                  # not necessary
                           ifelse(chrIndboolGue & chrIndex==FALSE,3, # only Guerra
                                  ifelse(chrIndboolGue==FALSE & chrIndbool==FALSE,4,NA) # none # not necessary
                           )
                    )
  )

  #
  #   add Guerra and Levan
  #

  if(chrIndboolGue){
    lapply(1:length(xm), function(s)
      graphics::text(c(xm[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xm[[s]][,3][1:(nrow(xm[[s]])/2)]+chrWidth/2),
                     rep( (min(ym[[s]])-( ( (distTextChr/3)*distVectorGue[decVector])) ),(nrow(xm[[s]])/2)+1 ),
                     # labels = c("Guerra",listOfdfChromSize[[s]][,"Guerra"][orderlist[[s]]]),
                     labels = c("Guerra",listOfdfChromSize[[s]][,"Guerra"]),

                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
    lapply(1:length(xm), function(s)
      graphics::text(c(xm[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xm[[s]][,3][1:(nrow(xm[[s]])/2)]+chrWidth/2),
                     rep( (min(ym[[s]])-( ( (distTextChr/3)*(distVectorGue[decVector]+1))) ),(nrow(xm[[s]])/2)+1 ),
                     # labels = c("Levan",listOfdfChromSize[[s]][,"Levan"][orderlist[[s]]]),
                     labels = c("Levan",listOfdfChromSize[[s]][,"Levan"]),
                     cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  } # fi

  #################################
  # vertical karyotype index
  #################################

  if(karIndex){
    ind<-asymmetry(dfChrSizeInternal)
    if(!is.null(ind)){
      lapply(1:length(xm), function(s)
        graphics::text( c( (xm[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                        rep( min(ym[[s]][,1]), 1 ), #2
                        labels = paste("A ",ind$A[[s]] ),
                        cex=indexIdTextSize,
                        adj=c(1) # justif
        ) # end graphics::text
      ) # end lapply
      lapply(1:length(xm), function(s)
        graphics::text(c( (xm[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
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

  distVector<-if(exists("grouporderlist") ){
    a<-c((distTextChr/3)*9 ,(distTextChr/3)*7,(distTextChr/3)*7,(distTextChr/3)*5 )
  } else {
    a<-c((distTextChr/3)*7 ,(distTextChr/3)*5,(distTextChr/3)*5,(distTextChr/3)*3 )
  }
  # distVector<-c((distTextChr/3)*7 ,(distTextChr/3)*5,(distTextChr/3)*5,(distTextChr/3)*3 )

  if(addOTUName){
    lapply(1:length(xm), function(s)
      graphics::text( c( (xm[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                      ydistance<-min(ym[[s]]) - distVector[decVector],
                      labels = paste("",names(listOfdfChromSize)[[s]] ),
                      cex=OTUTextSize,
                      adj=c(0) # justif 0 =left
      ) # end graphics::text
    ) # end lapply
  } # fi

  #######################################
  #
  #   painting Marks
  #
  #######################################
  if (exists("dfMarkPosSq") & exists("dfMarkColorInternal") ){
    yMark<-list()
    xMark<-list()
    for (sm in 1:length(listOfdfMarkPosSq)){
      yMark1<-NULL
      xMark1<-NULL
      for (m in 1:nrow(listOfdfMarkPosSq[[sm]])){
        ifelse(listOfdfMarkPosSq[[sm]][m,"markArm"]=="q",longORshort<-0,longORshort<-1)
        ifelse(listOfdfMarkPosSq[[sm]][m,"markArm"]=="q",column<-1,column<-2)
        ifelse(listOfdfMarkPosSq[[sm]][m,"markArm"]=="q",mySign<--1,mySign<-1)
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosSq)[[sm]] )

        ysup<-ym[[corr_index]][nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPosSq[[sm]][,orderingcolumn][m]),column]+
          (listOfdfMarkPosSq[[sm]][m,"markDistCen"]                                      *normalizeToOne*mySign)
        yinf<-ym[[corr_index]][nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPosSq[[sm]][,orderingcolumn][m]),column]+
          ((listOfdfMarkPosSq[[sm]][m,"markDistCen"]+listOfdfMarkPosSq[[sm]][m,"markSize"])*normalizeToOne*mySign)

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


  } # if presence end painting marks

  ########################################
  # dot style of marks
  ########################################
  if (exists("dfMarkPosCr") & exists("dfMarkColorInternal") ) {
    xfactor<-(xsizeplot/ysizeplot  )/dotRoundCorr

    yMarkCr<-list()
    xMarkCr<-list()
    rad<-list()
    colCr<-list()

    for (k in 1:length(listOfdfMarkPosCr)){
      # k=1
      yMarkCr1<-NULL
      xMarkCr1<-NULL
      rad1<-NULL
      colCr1<-NULL
      for (i in 1:nrow(listOfdfMarkPosCr[[k]])){
        # i=1
        ifelse(listOfdfMarkPosCr[[k]][i,"markArm"]=="q",longORshort<-0,longORshort<-1)
        ifelse(listOfdfMarkPosCr[[k]][i,"markArm"]=="q",column<-1,column<-2)
        ifelse(listOfdfMarkPosCr[[k]][i,"markArm"]=="q",mySign<--1,mySign<-1)
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosCr)[[k]] )
        # i
        ysupCr<-ym[[corr_index]][nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPosCr[[k]][,orderingcolumn][i]),column]+
          (listOfdfMarkPosCr[[k]][i,"markDistCen"]                                      *normalizeToOne*mySign)

        yinfCr<-ym[[corr_index]][nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPosCr[[k]][,orderingcolumn][i]),column]+
          ((listOfdfMarkPosCr[[k]][i,"markDistCen"]+listOfdfMarkPosCr[[k]][i,"markSize"])*normalizeToOne*mySign)
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
        }, # f
        x=xMarkCr[[w]][[u]],
        y=yMarkCr[[w]][[u]],
        r=rad[[w]][[u]],
        z=colCr[[w]][[u]]
        ) # mapply
      ) # lapply
    ) # la
  } # end painting MarkCrs

  ################################
  #
  #   centromeres calculate
  #
  ################################

  if(centromereSize>0){

  {
    CentsList<-lapply(1:length(listOfdfChromSize), function(x) rep(0, nrow(listOfdfChromSize[[x]]) ) )

    ycoordCents <-lapply(1:length(CentsList), function(x)
      t(replicate(length(CentsList[[x]])*2,(c(rep(  karHeight+                                (karSpacing*(x-1)),2  ),
                                              rep(  karHeight+(centromereSize*normalizeToOne)+(karSpacing*(x-1)),2  )
      )
      )
      ) # r
      ) #t
    ) #l
    if(reduDistKar){
      for (s in 1:(length(ymCopyC)-1) ) {
        diffnext<-abs(min(ymCopyC[[s+1]] ) - max(ymCopyC[[s]]) )
        diffnext<-(diffnext-((distTextChr/3)*reduDistKarTol))
        ymCopyC[[s+1]]=ymCopyC[[s+1]]-diffnext
        ycoordCents[[s+1]] <-ycoordCents[[s+1]] -diffnext
      }
    } # redu

    ycoordCentsS<- lapply(1:length(ycoordCents), function(j) base::split(ycoordCents[[j]], row(ycoordCents[[j]]) ) )
    xcoordCents<-lapply(1:length(xm), function(x) cbind(xm[[x]][,2:3],xm[[x]][,2:3] ) )
    xcoordCentsS<- lapply(1:length(xcoordCents), function(j) base::split(xcoordCents[[j]], row(xcoordCents[[j]]) ) )

    lapply(1:length(xcoordCentsS), function(w) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                        col=z,
                                                                                        lwd=1,
                                                                                        border=z),
                                                      x=xcoordCentsS[[w]],
                                                      y=ycoordCentsS[[w]],
                                                      z=cenColor
    ) #m
    ) #l
  } # end centromeres

  ###################
  #   centromeric with marks
  ###################


  if (exists("dfCenMarksInternal") & exists("dfMarkColorInternal") ) {
    names(xcoordCents)<-names(listOfdfChromSize) #1:length(xcoordCents)

    yMarkCen<-list()
    xMarkCen<-list()
    for (k in 1:length(listOfdfDataCen)){
      yMarkCen1<-NULL
      xMarkCen1<-NULL
      for (i in 1:nrow(listOfdfDataCen[[k]])){
        corr_index<-which(names(xcoordCents) %in% names(listOfdfDataCen)[[k]] )

        ysup<-ycoordCents[[corr_index]][(listOfdfDataCen[[k]][,orderingcolumn][i]),3]
        yinf<-ycoordCents[[corr_index]][(listOfdfDataCen[[k]][,orderingcolumn][i]),1]

        yMarkCen1[[i]]<-c(yinf,yinf,ysup,ysup)
        xMarkCen1[[i]]<-xcoordCents[[corr_index]][listOfdfDataCen[[k]][,orderingcolumn][i],]

      } # each mark
      yMarkCen[[k]]<-yMarkCen1
      xMarkCen[[k]]<-xMarkCen1
    } # each df

    #####################
    #   add to plot marks CENTROMERE MARKED
    #####################

    lapply(1:length(xMarkCen), function(w) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                    col=z, lwd=1, border=z),
                                                  x=xMarkCen[[w]],
                                                  y=yMarkCen[[w]],
                                                  z=dfMarkColorInternal$markColor[match(listOfdfDataCen[[w]]$markName, dfMarkColorInternal$markName)]
    ) # m
    ) #l
  } # end centromeres with marks

  ###########################################
  #
  #   plotting labels inline
  #
  ###########################################

  # cen labels
  if(legend=="inline" & exists("dfMarkColorInternal") & exists("dfCenMarksInternal") & exists("listOfdfDataCen") ) {
    lapply(1:length(xMarkCen), function(s)
      lapply(1:length(xMarkCen[[s]]), function (m)
        mapply(function(x,y,z) graphics::text(x=x,
                                              y=y,
                                              label=z,
                                              cex=markLabelSize,
                                              # pos=4,
                                              adj=0
        ), # text
        x=xMarkCen[[s]][[m]][1]+chrSpacing*.1,
        y=(yMarkCen[[s]][[m]][1]+yMarkCen[[s]][[m]][3])/2,
        z=listOfdfDataCen[[s]]$markName[m]
        ) #m
      )# l
    )# l
  }

  } # only if centromereSize >0 ##################################################################


  # square labels not cen
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
        z=sub(pattern,"",listOfdfMarkPosSq[[s]]$markName[m])
        # t
        ) #m
      )# l
    )# l
  }

  # dots
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
        z=sub(pattern,"",listOfdfMarkPosCr[[s]]$markName[m])
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
