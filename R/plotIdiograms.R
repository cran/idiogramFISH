#' FUNCTION to plot idiograms of karyotypes with and without centromere
#' @description This function reads a data.frame  with columns: \code{chrName} and
#' \code{shortArmSize} and \code{longArmSize} for monocentrics or a column \code{chrSize} for holocentrics and produces a plot of idiograms. If more
#' than one species, a column named \code{OTU} is needed.
#'
#' @description Optionally, it reads another data.frame with the position of
#' marks (sites) \code{dfMarkPos} \code{\link{markpos}}, in which case a data.frame for mark characteristics
#' can be used \code{\link{dfMarkColor}} or a vector for \code{mycolors}
#'
#' @param dfChrSize mandatory data.frame, with columns: \code{OTU} (optional), \code{chrName} (mandatory),
#'   \code{shortArmSize}, \code{longArmSize} for monocen. or \code{chrSize} for holocen.
#' @param dfMarkPos data.frame of marks (sites): cols: \code{OTU} (opt) \code{chrName},
#'   \code{markName} (name of site), \code{markArm} (for monocen), \code{markDistCen} (for monocen.),
#'   \code{markPos} (for holocen.), \code{markSize}; column \code{markArm}:
#'   use \code{p} for short arm, \code{q} for long arm and \code{cen} for centromeric mark; col. \code{markDistCen}: use distance from
#'   centromere to mark, not necessary for cen. marks. See param. \code{MarkDistanceType}
#' @param dfCenMarks data.frame, specific for centromeric marks. cols: \code{chrName}
#'   and \code{markName}. See also \code{dfMarkPos} for another option to pass cen. marks
#' @param dfMarkColor data.frame, optional, specifying colors and style for marks (sites);
#'   cols: \code{markName}, \code{markColor}, \code{style}. \code{style} accepts: \code{square} or \code{dots}.
#'   (if \code{style} missing all are plotted as \code{square})
#' @param mycolors character vector, optional, i.e. \code{c("blue","red","green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the
#' order of your marks use something like: \code{unique(c(dfMarkPos$markName,dfCenMarks$markName) ) }
#' @param MarkDistanceType character, if \code{cen} = the distance you provided is to
#'   the center of the mark, if \code{beg} = the distance you provided is to the
#'   beginning of the mark
#' @param addMissingOTUAfter character, when you want to add space (ghost OTUs) after one or several OTUs, pass the names of OTUs preceding the desired space in a character vector i.e. \code{c("species one","species five")}
#' @param missOTUspacings numeric, when you use \code{addMissingOTUAfter} this numeric vector should have the same length and corresponds to the number of free spaces (ghost OTUs) to add after each OTU respectively
#' @param orderBySize logical value, when \code{TRUE}, sorts chromosomes by total
#'   length from the largest to the smallest
#' @param centromereSize numeric, this establishes the apparent size of cen in
#'   the plot in \eqn{\mu}m
#' @param origin, For non-monocentric chr. (for holocentrics only) Use \code{"b"} if distance to mark in (\code{"markPos"} col. in \code{"dfMarkPos"}) data.frame measured from bottom of chromosome, use \code{"t"} for distance to mark from top
#' @param chrWidth numeric, relative chromosome width
#' @param chrSpacing numeric, horizontal spacing among chromosomes, see also  \code{chrWidth}
#' @param chrColor character, main color for chromosomes
#' @param cenColor character, color for centromeres
#' @param roundness numeric, shape of vertices of chromosomes and square marks,
#'   higher values more squared
#' @param dotRoundCorr numeric, correct roundness of dots and vertices of chromosomes. When  \code{style} of sites =
#'    \code{dots}, an increase in this, makes the horizontal radius of the dot smaller
#' @param karHeight numeric, vertical size of karyotypes. See also  \code{karHeiSpace}
#' @param karHeiSpace numeric, vertical size of karyotypes including spacing. Proportional to \code{karHeight}, if overlap, increase
#' @param karSepar boolean, reduce distance among karyotypes \code{FALSE} = equally
#'   sized karyotypes or \code{TRUE} = equally spaced karyotypes. Incompatible with \code{addMissingOTUAfter}
#' @param amoSepar numeric, depends on \code{karSepar=TRUE}, if zero your
#'   karyotypes will have no distance among them, if overlap,
#'   increase this and \code{karHeiSpace}
#' @param chrId character, print name of chromosome, \code{"original"} uses the original
#'   name in OTU column of dfChrSize, \code{"simple"} (just 1 to ...) or \code{""} (none).
#' @param distTextChr numeric, distance from name of chromosome to chromosome,
#'   also affects vertical separation of indices
#' @param indexIdTextSize numeric, font size of chr. and kar. indices and
#'   chromosome name
#' @param OTUTextSize numeric, font size of OTU name (species)
#' @param legend character, \code{""} for no legend; \code{"inline"} prints labels near
#'   chromosomes; \code{"aside"} prints legend to the right of karyotypes. See \code{markLabelSpacer}
#' @param markLabelSize numeric, only if legend != (not) "", size of the text of
#'   labels of marks (legend)
#' @param markLabelSpacer numeric, only if \code{legend="aside"}, space from the
#'   rightmost chr. to legend
#' @param pattern REGEX pattern to remove from names of marks
#' @param chrIndex logical, add arm ratio and centromeric index
#' @param nameChrIndexPos numeric, modify position of name of chr. indices
#' @param karIndex logical, add karyotype indices A (intrachromosomal -
#'   centromere pos.) and A2 (interchromosomal asymmetry, variation among
#'   chromosome sizes)
#' @param karIndexPos numeric, move karyotype index
#' @param morpho boolean, if \code{TRUE} prints the Guerra and Levan classif of cen.
#'   position. see \code{?armRatioCI}
#' @param addOTUName boolean, if \code{TRUE} adds OTU (species) name to karyotype
#' @param revOTUs boolean, The order of species is the one in the main
#'   data.frame, use \code{TRUE} to reverse
#' @param ruler boolean, display ruler to the left of karyotype, when \code{FALSE} no ruler
#' @param rulerPos numeric, absolute position of ruler, corresponds to \code{pos}
#'   argument of \code{axis} R plot
#' @param rulerPosMod numeric, modify position of ruler, corresponds to \code{line}
#'   argument of \code{axis} R plot
#' @param ruler.tck numeric, tick size of ruler, corresponds to \code{tck} argument of
#'   \code{axis} R plot
#' @param rulerNumberPos numeric, modify position of numbers of ruler
#' @param rulerNumberSize numeric, size of number's font in ruler
#' @param xlimLeftMod numeric, modifies \code{xlim} left argument of plot
#' @param xlimRightMod numeric, \code{xlim} right side modification by adding space to the right
#'   of idiograms
#' @param ylimBotMod numeric, modify \code{ylim} bottom argument of plot
#' @param ylimTopMod numeric, modify \code{ylim} top argument of plot
#' @param ... accepts other arguments for the plot, such as, \code{asp}
#' @param lwd.chr thick of border of chr. and marks.
#' @param legendWidth factor to increase width of squares and of legend
#' @param legendHeight factor to increase height of squares and dots of legend
#' @param Mb, boolean, if your measures are in Megabases use \code{TRUE}
#' @param ylabline, numeric if \code{Mb=TRUE} modify position of y axis title (Mb)
#' @param n, numeric vertices number for round corners
#'
#' @keywords data.frame chromosome
#'
#' @importFrom graphics par plot segments mtext
#'
#' @export
#'
#' @examples
#' data(dfOfChrSize)
#' plotIdiograms(dfOfChrSize)
#' plotIdiograms(dfChrSizeHolo)
#' @seealso \code{\link{asymmetry}}
#' @seealso \code{\link{armRatioCI}}
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{chrbasicdataHolo}}
#' @seealso \code{\link{markpos}}
#' @seealso \code{\link{markdataholo}}
#' @seealso \code{\link{dfMarkColor}}
#'
#' @return plot
#'
#' @export


plotIdiograms<-function(dfChrSize, dfMarkPos, dfCenMarks, dfMarkColor, mycolors, MarkDistanceType="beg",orderBySize=TRUE,
                        centromereSize =1, chrWidth=1.5, chrSpacing=1.5,chrColor="gray", cenColor="gray",
                        roundness=4, dotRoundCorr=1.5,
                        karHeight=1.2,karHeiSpace=1.6,karSepar=TRUE,amoSepar=9,
                        chrId="original", distTextChr=.3,
                        indexIdTextSize=.4, OTUTextSize=.6,
                        legend="inline", markLabelSize=.4, markLabelSpacer=2,
                        chrIndex=TRUE, nameChrIndexPos=2, karIndex=TRUE, karIndexPos=.5,
                        morpho=TRUE,
                        addOTUName=TRUE,revOTUs=FALSE,
                        ruler=TRUE,rulerPos=-.5, rulerPosMod=0, ruler.tck=-0.004, rulerNumberPos=.2, rulerNumberSize=.4,
                        xlimLeftMod=1,  xlimRightMod=10, ylimBotMod=.2,ylimTopMod=.2,
                        lwd.chr=2, pattern="", addMissingOTUAfter=NA,missOTUspacings=0,
                        legendWidth=1.7, legendHeight=NA, Mb=FALSE, ylabline=0, origin="b", n=50,
                        ...)
{
  if(!missing(dfChrSize)){
    dfChrSizeInternal<-makeNumCols(dfChrSize)
  } else {
    message(crayon::red("Missing mandatory dfChrSize data.frame"))
    return(NA)
  }
  if(!missing(dfMarkPos)){
    dfMarkPosInternal1 <- dfMarkPosInternal <- makeNumCols(dfMarkPos)
    # dfMarkPosInternal  <- dfMarkPosInternal[which(dfMarkPosInternal$markArm!="cen"),]
    # if(nrow(dfMarkPosInternal)==0 ){
      # remove(dfMarkPosInternal)
    # }
    dfCenMarksInternal <- dfMarkPosInternal1[which(dfMarkPosInternal1$markArm=="cen"),]
    if(nrow(dfCenMarksInternal)==0 ){
      remove(dfCenMarksInternal)
    }
  } # df of marks
  if(!missing(dfCenMarks)  ){
    dfCenMarksInternal2<-makeNumCols(dfCenMarks)
  }
  if(exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
    dfCenMarksInternal<-plyr::rbind.fill(dfCenMarksInternal,dfCenMarksInternal2)
  }
  if(!exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
    dfCenMarksInternal<-dfCenMarksInternal2
  }
  if(!missing(dfMarkColor)){
    dfMarkColorInternal<-makeNumCols(dfMarkColor)
  }

  message(crayon::black(paste("Making checks\n")) )
  message(crayon::black(paste("In case of error see messages and the help ?functionName\n")) )

  #
  #   dfChrSizeInternal
  #

  #################################################################### LISTS

  #
  # transform dfs to list of df dfChrSizeInternal
  #

  if("OTU" %in% colnames(dfChrSizeInternal)){
    listOfdfChromSize<-base::split(dfChrSizeInternal, factor(dfChrSizeInternal$OTU,levels = unique(dfChrSizeInternal$OTU)) )
    names(listOfdfChromSize)<-unique(dfChrSizeInternal$OTU)
  } else {
    listOfdfChromSize<-list(dfChrSizeInternal)
    names(listOfdfChromSize)<-1
    addOTUName<-FALSE
  }

  #
  #   Classify data.frames from list as monocen or holocen Add attribute cenType
  #

for (i in 1:length(listOfdfChromSize)) {

    #
    # remove columns without info. per karyotype
    #

    listOfdfChromSize[[i]][listOfdfChromSize[[i]]==""]<-NA
    listOfdfChromSize[[i]]<-  listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

    # Does the data.frame have short and long info?
    message(crayon::black("\nChecking mandatory columns from dfChrSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
    ) ) # mess

    #
    #   let see if it is monocen
    #

    if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                       colnames(listOfdfChromSize[[i]]) ) )==0 ){
      message(crayon::green(paste("\nOTU ",names(listOfdfChromSize)[[i]],"has all columns with info to have monocen. If not, you have to clean your data"))
      )# message
      attr(listOfdfChromSize[[i]],'cenType') <- "monocen"


      if(Mb){
        listOfdfChromSize[[i]]$longArmSize<-listOfdfChromSize[[i]]$longArmSize/1000000
        listOfdfChromSize[[i]]$shortArmSize<-listOfdfChromSize[[i]]$shortArmSize/1000000
      }

      if(Mb==FALSE  & max(listOfdfChromSize[[i]]$longArmSize)>10000 ){
        message(crayon::red(paste("\nChromosome size too large, use Mb=TRUE, you are probably working in Mb, not micrometers\n") )
        )#c
        # return(NA)
        listOfdfChromSize[[i]]<-NA
      } else if (Mb & (max(listOfdfChromSize[[i]]$longArmSize)*1000000)<(10000) ) {
        message(crayon::red(paste("\nChromosome size in Mb too small, use Mb=FALSE, you are probably working with micrometers\n") )
        )#c
        # return(NA)
        listOfdfChromSize[[i]]<-NA
      }

    } # if monocen success

    #
    #   let see if it is holocen
    #

    else if(length( setdiff(c("chrName", "chrSize"),
                            colnames(listOfdfChromSize[[i]]) ) )==0 ){
      message(crayon::green(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," has all columns with info have holocen. If not, you have to clean your data")))
      )# message
      attr(listOfdfChromSize[[i]], 'cenType') <- "holocen"

        if(Mb){
          listOfdfChromSize[[i]]$chrSize<-listOfdfChromSize[[i]]$chrSize/1000000
        }

      if(Mb==FALSE  & max(listOfdfChromSize[[i]]$chrSize)>10000 ){
        message(crayon::red(paste("\nChromosome size too large, use Mb=TRUE, you are probably working in Mb, not micrometers\n") )
        )#c
        message(crayon::red(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," was removed")))
        ) # mess
        # return(NA)
        listOfdfChromSize[[i]]<-NA
      } else if (Mb & (max(listOfdfChromSize[[i]]$chrSize)*1000000) < 10000 ) {
        message(crayon::red(paste("\nChromosome size in Mb too small, use Mb=FALSE, you are probably working with micrometers\n") )
        )#c
        message(crayon::red(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," was removed")))
        ) # mess
        # return(NA)
        listOfdfChromSize[[i]]<-NA
      }

    } # if holocen success

    #
    # let see if it is not monocen
    #

    else if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                       colnames(listOfdfChromSize[[i]]) ) )>0 ){
      message(crayon::green(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," does not have all columns with info to make a monocen.: ",
                                    paste0(c(setdiff(c("chrName", "shortArmSize","longArmSize"),
                                                     colnames(listOfdfChromSize[[i]])
                                    )," will be removed" # setdiff
                                    ), collapse = " "
                                    ) # paste
      )) # pas
      ) # crayon
      ) # message
      listOfdfChromSize[[i]]<-NA
    } # fi
    #
    #   let see if it is not holocen
    #
    else if (length( setdiff(c("chrName", "chrSize"),
                        colnames(listOfdfChromSize[[i]]) ) )>0 ){
      message(crayon::black("\nChecking mandatory columns from dfChrSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
      ) ) # mess
      message(crayon::green(paste(c("\nOTU",names(listOfdfChromSize)[[i]],"does not have all columns with info to make a holocen:",
                                    paste0(c(setdiff(c("chrName", "chrSize"),
                                                     colnames(listOfdfChromSize[[i]])
                                    )," will be removed" # setdiff
                                    ), collapse = " "
                                    ) # paste
      )))
      ) #mess
      listOfdfChromSize[[i]]<-NA
    }


} # end for for (i in 1:length(listOfdfChromSize)) {

  listOfdfChromSize<-listOfdfChromSize[!is.na(listOfdfChromSize)]

  #
  #     calculate armRatioCI only for chr. with centromere attr cenType =  holocen.
  #

  #
  #    generate Chromosome indexes
  #

  if(chrIndex | morpho){
    for (i in 1:length(listOfdfChromSize)) {
      if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){
        listOfdfChromSize[[i]]<-armRatioCI(listOfdfChromSize[[i]])
      } # if
    } # for
  } # if chrIndex

   ##############################################################################
  #
  #   dfMarkPosInternal to list
  #

{
  monocenVector<-integer()
  for(i in 1:length(listOfdfChromSize)){
    if(attr(listOfdfChromSize[[i]],"cenType")=="monocen"){monocenVector<-c(monocenVector,i)}
  }
  monocenNames<-names(listOfdfChromSize)[monocenVector]

  holocenVector<-integer()
  for(i in 1:length(listOfdfChromSize)){
    if(attr(listOfdfChromSize[[i]],"cenType")=="holocen"){holocenVector<-c(holocenVector,i)}
  }
  holocenNames<-names(listOfdfChromSize)[holocenVector]
}
  if (exists("dfMarkPosInternal")){

    if("OTU" %in% colnames(dfMarkPosInternal)){
      listOfdfMarkPosInternal<-base::split(dfMarkPosInternal, factor(dfMarkPosInternal$OTU,levels = unique(dfMarkPosInternal$OTU)  ) )
      names(listOfdfMarkPosInternal)<-unique(dfMarkPosInternal$OTU)
    } else {
      listOfdfMarkPosInternal<-list(dfMarkPosInternal)
      names(listOfdfMarkPosInternal)<-1
    }
    listOfdfMarkPosMonocen<-listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% monocenNames)]
    # names(listOfdfMarkPosMonocen)
    if(length(listOfdfMarkPosMonocen)==0){
      remove(listOfdfMarkPosMonocen)
    } else {
      for (i in 1:length(listOfdfMarkPosMonocen)){
      listOfdfMarkPosMonocen[[i]]  <- listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$markArm!="cen"),]
      } # for
      # row0 <- which(sapply(listOfdfMarkPosMonocen, nrow) < 1)
      # listOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[-row0]
      listOfdfMarkPosMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfMarkPosMonocen)
    }# else

    listOfdfMarkPosHolocen<-listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% holocenNames)]
    if(length(listOfdfMarkPosHolocen)==0){
      remove(listOfdfMarkPosHolocen)
    }

  } # end missing dfMarkPosInternal

  if (exists("dfCenMarksInternal")){

    if("OTU" %in% colnames(dfCenMarksInternal)){
      listOfdfDataCen<-base::split(dfCenMarksInternal, factor(dfCenMarksInternal$OTU,levels = unique(dfCenMarksInternal$OTU) ) )
      names(listOfdfDataCen)<-unique(dfCenMarksInternal$OTU)
    } else {
      listOfdfDataCen<-list(dfCenMarksInternal)
      names(listOfdfDataCen)<-1
    }

    listOfdfDataCen<-listOfdfDataCen[which(names(listOfdfDataCen) %in% monocenNames)]
    if(length(listOfdfDataCen)==0){
      remove(listOfdfDataCen)
    } else {
      #
      #   remove columns without info.
      #
      for (i in 1:length(listOfdfDataCen)){
        listOfdfDataCen[[i]][listOfdfDataCen[[i]]==""]<-NA
        listOfdfDataCen[[i]]<-  listOfdfDataCen[[i]][, !apply(is.na(listOfdfDataCen[[i]]), 2, all)]
      } # for
    } # else
  } # end missing dfCenMarksInternal

  #
  #   for each d.f. of dfmarkpos check columns
  #

  ############################################################################################################################
  #
  #   Monocen check marks
  #

  if(exists("listOfdfMarkPosMonocen")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos: chrName, markName, markArm,markDistCen,markSize\n (column OTU  is necessary if more than one species)\n"
    ) )# cat

    for (i in 1:length(listOfdfMarkPosMonocen ) ) {

      listOfdfMarkPosMonocen[[i]][listOfdfMarkPosMonocen[[i]]==""]<-NA
      listOfdfMarkPosMonocen[[i]]<-  listOfdfMarkPosMonocen[[i]][, !apply(is.na(listOfdfMarkPosMonocen[[i]]), 2, all)]

      #
      #   rename col markpos if necessary
      #

      if(!"markDistCen" %in% colnames(listOfdfMarkPosMonocen[[i]]) & "markPos" %in% colnames(listOfdfMarkPosMonocen[[i]])  ){
        message(crayon::red(paste(c("Columns markPos in d.f. of marks of OTU",names(listOfdfMarkPosMonocen)[[i]] ,"renamed to markDistCen")))
                ) # mess
        colnames(listOfdfMarkPosMonocen[[i]])[which(names(listOfdfMarkPosMonocen[[i]])=="markPos")]<-"markDistCen"
      }
      #
      #   column error
      #
      if(length (setdiff(c("chrName", "markName", "markArm","markDistCen","markSize"),
                         colnames(listOfdfMarkPosMonocen[[i]]) ) )>0 ) {
        message(crayon::red(paste(c("ERROR Missing columns in d.f. of marks of OTU",names(listOfdfMarkPosMonocen)[[i]] ,":",
                                setdiff(c("chrName", "markName", "markArm","markDistCen","markSize"),
                                        colnames(listOfdfMarkPosMonocen[[i]]) ) ) , sep="\n", collapse = " " )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(listOfdfMarkPosMonocen)[[i]] ,"REMOVED\n")
        ) )#m
        listOfdfMarkPosMonocen[[i]]<-NA
      } # fi setdiff
      #
      #   column without error
      #
      else { # if no error
        message(crayon::black(paste("\nOK marks of OTU",names(listOfdfMarkPosMonocen)[[i]],"checked \n")
                ) ) #m
        if(MarkDistanceType=="cen"){
          listOfdfMarkPosMonocen[[i]]$markDistCen<-listOfdfMarkPosMonocen[[i]]$markDistCen-(listOfdfMarkPosMonocen[[i]]$markSize/2)
        } # if

      } # else No Error
    } # for each data.frame of Marks of Monocen


  listOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[!is.na(listOfdfMarkPosMonocen)]
  # do as befor with holo 27/09

  for (i in 1:length(listOfdfMarkPosMonocen)) {
    if(Mb){
      listOfdfMarkPosMonocen[[i]]$markDistCen<-listOfdfMarkPosMonocen[[i]]$markDistCen/1000000
      listOfdfMarkPosMonocen[[i]]$markSize<-listOfdfMarkPosMonocen[[i]]$markSize/1000000
    }
  }

} # fi listOfdfMarkPosMonocen

  ##################################################################################################################
  #
  #   holocen check mark
  #

  if(exists("listOfdfMarkPosHolocen")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos (without cen.): chrName, markName, markPos,markSize\n (column OTU  is necessary if more than one species)\n"
    ) )# mess

    for (i in 1:length(listOfdfMarkPosHolocen ) ) {

      listOfdfMarkPosHolocen[[i]][listOfdfMarkPosHolocen[[i]]==""]<-NA
      listOfdfMarkPosHolocen[[i]]<-  listOfdfMarkPosHolocen[[i]][, !apply(is.na(listOfdfMarkPosHolocen[[i]]), 2, all)]
      #
      #   rename col markdistcen if necessary
      #

      if(!"markPos" %in% colnames(listOfdfMarkPosHolocen[[i]]) & "markDistCen" %in% colnames(listOfdfMarkPosHolocen[[i]])  ){
        message(crayon::red(paste(c("Columns markDistCen in d.f. of marks of OTU",names(listOfdfMarkPosHolocen)[[i]] ,"renamed to markPos")))
        ) # mess
        colnames(listOfdfMarkPosHolocen[[i]])[which(names(listOfdfMarkPosHolocen[[i]])=="markDistCen")]<-"markPos"
      }

      #
      #   column error
      #

      if(length (setdiff(c("chrName", "markName", "markPos","markSize"),
                           colnames(listOfdfMarkPosHolocen[[i]]) ) )>0 ){
          message(crayon::red(paste(c("ERROR Missing columns:",
                                      setdiff(c("chrName", "markName", "markPos","markSize"),
                                              colnames(listOfdfMarkPosHolocen[[i]]) ) ) , sep="\n", collapse = " " )
          )
          ) # cat
          message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(listOfdfMarkPosHolocen)[[i]] ,"REMOVED\n")
          ) ) #m
          # listOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[-i]
          listOfdfMarkPosHolocen[[i]]<-NA
        } # fi
      #
      #   column without error
      #

      else { # if no error
        message(crayon::black(paste("\nOK marks of OTU",names(listOfdfMarkPosHolocen)[[i]],"checked \n")
        ) ) #m
        # listOfdfMarkPosHolocen[[i]]$markDistCen<-as.numeric(listOfdfMarkPosHolocen[[i]]$markDistCen)
        # listOfdfMarkPosHolocen[[i]]$markSize<-as.numeric(listOfdfMarkPosHolocen[[i]]$markSize)

        if(origin=="t"){
          listOfdfMarkPosHolocen[[i]]$markPos2<-listOfdfMarkPosHolocen[[i]]$markPos
          listOfdfMarkPosHolocen[[i]]$chrSize<-
            #
            #
            #
            dfChrSizeInternal[match(interaction(listOfdfMarkPosHolocen[[i]][c("OTU", "chrName")]),
                                    interaction(dfChrSizeInternal[c("OTU", "chrName")])),]$chrSize

          listOfdfMarkPosHolocen[[i]]$markPos<-listOfdfMarkPosHolocen[[i]]$chrSize-listOfdfMarkPosHolocen[[i]]$markPos2
        }

        if(MarkDistanceType=="cen"){
          listOfdfMarkPosHolocen[[i]]$markPos<-listOfdfMarkPosHolocen[[i]]$markPos-(listOfdfMarkPosHolocen[[i]]$markSize/2)
        } # if

      } # else No Error
    } # for each data.frame of Marks of Monocen

  listOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[!is.na(listOfdfMarkPosHolocen)]

  for (i in 1:length(listOfdfMarkPosHolocen)) {
    if(Mb){
      listOfdfMarkPosHolocen[[i]]$markPos<-listOfdfMarkPosHolocen[[i]]$markPos/1000000
      listOfdfMarkPosHolocen[[i]]$markSize<-listOfdfMarkPosHolocen[[i]]$markSize/1000000
    }
  } # for
} # fi holocen exists

  ################################################################################################################################
  #
  #   cen Mark check
  #

  if(exists("listOfdfDataCen")){
    message(crayon::black("\nChecking mandatory columns from dfCenMarks: chrName, markName\n (column OTU  is necessary if more than one species)\n")
    ) # mess

    for (i in 1:length(listOfdfDataCen)){
      #
      #   columns with error
      #

    if(length(setdiff(c("chrName", "markName"),
                      colnames(listOfdfDataCen[[i]]) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName"),
                                      colnames(listOfdfDataCen[[i]]) ),"in OTU", names(listOfdfDataCen)[[i]]   ), sep="\n", collapse = " " )
      )
      ) # cat
      message(crayon::red(paste("\nERRORS PRESENT, see above, dfCenMarks of OTU", names(listOfdfDataCen)[[i]] ,"REMOVED\n")
      ) ) #m
      listOfdfDataCen[[i]]<-NA

    } # fi

      #
      #   columns without error
      #

    else { # if no error
      message(crayon::black(paste("\nOK cen. marks of OTU",names(listOfdfDataCen)[[i]],"checked \n")
      ))# mess
    } # else
    } # for

  listOfdfDataCen<-listOfdfDataCen[!is.na(listOfdfDataCen)]

  } # fi   if(exists("listOfdfDataCen"))

  ##############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("listOfdfMarkPosMonocen")){
    # message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n") )

    if(length(setdiff(names(listOfdfMarkPosMonocen ),
                      names(listOfdfChromSize) ) )>0){
      diff<-setdiff(names(listOfdfMarkPosMonocen ),
                    names(listOfdfChromSize) )
      message(
        crayon::red(
        paste(c("\nWarning:",
                diff,
                "OTU(s) of dfMarkPos data.frame NOT in Chr. size (main) data.frame, they will not be plotted"),
              sep=" ", collapse = " "
              ) # pas
      ) ) #message

      #
      #   delete names in diff
      #

      listOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[which(names(listOfdfMarkPosMonocen) %in% diff ) ]

    }
  } # exists

  ############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("listOfdfMarkPosHolocen")){
    # message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n") )

    if(length(setdiff(names(listOfdfMarkPosHolocen ),
                      names(listOfdfChromSize) ) )>0){
      diff <- setdiff(names(listOfdfMarkPosHolocen ),
                    names(listOfdfChromSize) )
      message(
        crayon::red(
          paste(c("\nWarning:",
                  diff,
                  "OTU(s) of dfMarkPos data.frame NOT in Chr. size (main) data.frame, they will not be plotted"),
                sep=" ", collapse = " "
          ) # pas
        ) ) #message

      #
      #   delete names in diff
      #

      listOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[which(names(listOfdfMarkPosHolocen) %in% diff ) ]

    }
  } # exists

  #
  #     check chromosomes names  from d.f. marks to chr. size. d.f.
  #

  if(exists("listOfdfMarkPosMonocen") ) {
  listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfMarkPosMonocen)
  listOfdfChromSize<-listOfChecksChr[[1]]
  listOfdfMarkPosMonocen<-listOfChecksChr[[2]]
  if(length(listOfdfMarkPosMonocen)==0){
    remove(listOfdfMarkPosMonocen)
  }
  allMarkNames<-unique(listOfChecksChr[[3]])
  if(length(listOfChecksChr[[4]])>0){
  allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
  }
  }

  if(exists("listOfdfMarkPosHolocen") ) {
  listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfMarkPosHolocen)
  listOfdfChromSize<-listOfChecksChr[[1]]

  listOfdfMarkPosHolocen<-listOfChecksChr[[2]]
  if(length(listOfdfMarkPosHolocen)==0){
    remove(listOfdfMarkPosHolocen)
  }
  if(exists("allMarkNames")){
  allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
  } else {
    allMarkNames<-unique(listOfChecksChr[[3]])
  }
  if(length(listOfChecksChr[[4]])>0){
    if (exists("allMarkMaxSize")){
    allMarkMaxSize<-max(c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) ), na.rm=TRUE)
    } else {
      allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
    }
  }
  } # holocen

  if(centromereSize>0){
  if(exists("listOfdfDataCen") ) {
    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfDataCen)
    listOfdfChromSize<-listOfChecksChr[[1]]
    listOfdfDataCen<-listOfChecksChr[[2]]
    if(length(listOfdfDataCen)==0){
      remove(listOfdfDataCen)
    }
    # allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
    if(exists("allMarkNames")){
      allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
    } else {
      allMarkNames<-unique(listOfChecksChr[[3]])
    }
    if(length(listOfChecksChr[[4]])>0){
    # allMarkMaxSize<-c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) )
      if (exists("allMarkMaxSize")){
        allMarkMaxSize<-max(c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) ), na.rm=TRUE)
      } else {
        allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
      }
    }
  }
  } # cen 0

  #############################################################################################################
  #
  #   check compatibility of columns dfMarkColor
  #

  {
  if(exists("dfMarkColorInternal") ) {
    message(crayon::black("\n####\nChecking mandatory columns from dfMarkColor: markName, markColor, style\n"
    ) )#cat

    #
    #   create style column when missing
    #

    if(!"style" %in% colnames(dfMarkColorInternal) ) {
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
    else {  # column names ok
      if(exists("allMarkNames")){
        dfMarkColorInternal<-dfMarkColorInternal[which(dfMarkColorInternal$markName %in% allMarkNames) ,]
        if (nrow(dfMarkColorInternal)==0){
          message(crayon::red("\nError in dfMarkColor markNames respect to Marks pos. data.frames, dfMarkColor REMOVED\n")
          )#cat
          remove(dfMarkColorInternal)
        } else if ( length(setdiff(allMarkNames,unique(dfMarkColorInternal$markName) ) )>0) { # nrow not 0
          message(crayon::black("\nColors provided in to dfMarkColor are not enough, internal colors will be used.\n") )
          dfMarkColorInternal<-makedfMarkColor(dfMarkColorInternal,allMarkNames,c(chrColor,cenColor) )
        } else { # nrow not 0
          message(crayon::black("\nCheck OK\n") )
        }
      } # fi # end allmarknames exist
      else { # all Mark Names does not exist
        message(crayon::red("\nError in dfMarkColor Names respect to Marks dataframes, dfMarkColor REMOVED\n")
        )
        remove(dfMarkColorInternal)
      } # else
    } # else column names ok
  } #fi exists dfMarkColor ... continues

  ######################################################################################################
  #   after 1.0.0
  ######################################################################################################

else if (missing(mycolors) ) { # if dfMarkColor not exist and missing mycolors
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColor(idiogramFISH::dfMarkColor,allMarkNames,c(chrColor,cenColor) )
      } # allmarknames
  } else if (!missing(mycolors) ) { # if dfMarkColor not exist , mycolors exist
    if(exists("allMarkNames")){
      dfMarkColorInternal<-makedfMarkColorMycolors(allMarkNames,c(chrColor,cenColor), mycolors )
    }
} # elif
}





  #
  #   CREATION OF CHILD DATAFRAMES MARKS
  #

  ###################
  # REQUIRES STYLE
  ###################

  # substituted by:

  if(exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal")  ){
    for (i in 1:length(listOfdfMarkPosMonocen)){
      listOfdfMarkPosMonocen[[i]]$style<-dfMarkColorInternal$style[match(listOfdfMarkPosMonocen[[i]]$markName, dfMarkColorInternal$markName)]
    } # for
  } # fi

  if(exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal")  ){
    for (i in 1:length(listOfdfMarkPosHolocen)){
      if(class(listOfdfMarkPosHolocen[[i]])=="data.frame" ) {
      listOfdfMarkPosHolocen[[i]]$style<-dfMarkColorInternal$style[match(listOfdfMarkPosHolocen[[i]]$markName, dfMarkColorInternal$markName)]
      } # if data.frame
    } # for
  } # fi

  #################################################################################
  #
  #       MISSING OTUs
  #

  if(!is.na(addMissingOTUAfter[1]) ){
    if (length(missOTUspacings) != length(addMissingOTUAfter) ){
      missOTUspacings<-rep(missOTUspacings, abs(length(addMissingOTUAfter)/length(missOTUspacings)) ) [1:length(addMissingOTUAfter)]
    }
    for (i in 1:length(addMissingOTUAfter)){
    listOfdfChromSize <- append(listOfdfChromSize,
                                rep(NA,missOTUspacings[[i]]),
                                (which(names(listOfdfChromSize)==addMissingOTUAfter[[i]]) )
                                ) # append
    }
  } # fi

# )

  #
  #     reverse
  #

  listOfdfChromSize<-rev(listOfdfChromSize)

  if(revOTUs){
    listOfdfChromSize<-rev(listOfdfChromSize)
  }

  #
  #	change of size based on number of sps
  #

  {
    chrWidth  <- chrWidth/length(listOfdfChromSize)
    chrSpacing<- chrSpacing/length(listOfdfChromSize)
  }  # dfMarkPosSq to listOfdfMarkPosInternalSq


  #######################################################
  #
  #   If Marks missing, rename duplicates of chrNames
  #
  #######################################################

  # mybooleanChrName <- !exists("listOfdfMarkPosHolocen") & !exists("listOfdfMarkPosMonocen") & !exists("listOfdfDataCen")

  # listOfdfChromSize<-fixChrNameDup(listOfdfChromSize, mybooleanChrName)

  for (s in 1:length(listOfdfChromSize) ){
    OTUname<-names(listOfdfChromSize[s])
    if (exists("listOfdfMarkPosHolocen") ){
      OTUlistOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[which(names(listOfdfMarkPosHolocen) %in% OTUname)]
      if(length(OTUlistOfdfMarkPosHolocen)==0){
        remove(OTUlistOfdfMarkPosHolocen)
      }
    }
    if (exists("listOfdfMarkPosMonocen") ){
      OTUlistOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[which(names(listOfdfMarkPosMonocen) %in% OTUname)]
      if(length(OTUlistOfdfMarkPosMonocen)==0){
        remove(OTUlistOfdfMarkPosMonocen)
      }
    }
    if (exists("listOfdfDataCen") ){
      OTUlistOfdfDataCen<-listOfdfDataCen[which(names(listOfdfDataCen) %in% OTUname)]
      if(length(OTUlistOfdfDataCen)==0){
        remove(OTUlistOfdfDataCen)
      }
    }
    mybooleanChrName <- !exists("OTUlistOfdfMarkPosHolocen") & !exists("OTUlistOfdfMarkPosMonocen") & !exists("OTUlistOfdfDataCen")
    dfChromSize<-fixChrNameDupDF(listOfdfChromSize[s], mybooleanChrName)
    listOfdfChromSize[[s]]<-dfChromSize[[1]]
  }

  #####################
  #   total size of chr
  #####################
  #
  #   add column total to dataframes
  #

  for ( i in 1:length(listOfdfChromSize)) {
    if(class(listOfdfChromSize[[i]])=="data.frame") {
    # str(listOfdfChromSize)
      if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){
          listOfdfChromSize[[i]]$chrSize<-listOfdfChromSize[[i]]$shortArmSize+listOfdfChromSize[[i]]$longArmSize
      } # if
    } # if
  } # for

  {
    totalLength<-lapply(listOfdfChromSize, function(x) tryCatch(x$chrSize, error=function(e) NA)  )
    ifelse(class(totalLength)=="matrix",
           totalLength <- base::split(totalLength, col(totalLength) )
           ,NA)
    normalizeToOne<-karHeight/max(unlist(totalLength) , na.rm=TRUE)
  }



  ##############################################################################
  # order by size
  ##############################################################################
  {
    if(orderBySize==TRUE) {
        orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
    } else { # if not want to order by size, set order by name of chro
        orderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$chrName), error=function(e) NA ) )
    } #else

    ##################################################
    #
    #   add column of new chro index to dataframes
    #
    ##################################################

# suppressWarnings(
    for (s in 1:length(listOfdfChromSize)){
      if(class(listOfdfChromSize[[s]])=="data.frame") {
      listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][orderlist[[s]], ]
      listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
      }
    } # end for
# )


    ###################################################
    #     after 1.1.0
    ###################################################

    if("group" %in% colnames(dfChrSizeInternal)){
      message(crayon::blue("group column present - remove column if not using") )
      grouporderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$group), error=function(e) NA ) )

      # suppressWarnings(
      for (s in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[s]])=="data.frame") {
          # if(!is.na(listOfdfChromSize[[s]] )  ) {

        listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][grouporderlist[[s]], ]
        listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])

        } #fi
      } # end for
      # )
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

    if(exists("listOfdfMarkPosHolocen") ){

      for (s in 1:length(listOfdfChromSize)){
        selecteddfChromData<-which(names(listOfdfMarkPosHolocen)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfChromData)>0){
          listOfdfMarkPosHolocen[[selecteddfChromData]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfMarkPosHolocen[[selecteddfChromData]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      }
    } # end if presence of dfMarkPosSq order


    if(exists("listOfdfMarkPosMonocen") ){

      for (s in 1:length(listOfdfChromSize)){
        # s<-1
        selecteddfChromData<-which(names(listOfdfMarkPosMonocen)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfChromData)>0){
          listOfdfMarkPosMonocen[[selecteddfChromData]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfMarkPosMonocen[[selecteddfChromData]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      }
    } # end if presence of dfMarkPosCr order

    ######################################################
    #
    #   important - add new indexer to d.f DataCen
    #
    ######################################################

    if (exists("listOfdfDataCen")){

      for (s in 1:length(listOfdfChromSize)){
        selecteddfDataCen<-which(names(listOfdfDataCen)==names(listOfdfChromSize)[[s]])
        if(length(selecteddfDataCen)>0){
          listOfdfDataCen[[selecteddfDataCen]]$neworder<-listOfdfChromSize[[s]]$neworder[match(
            listOfdfDataCen[[selecteddfDataCen]]$chrName,listOfdfChromSize[[s]]$chrName)]
        }
      } # end for
    }  # end presence of dfCenMarksInternal
  } # ordering chunk

  #######################
  #
  #      main plot
  #
  #######################

  {
    # Monocen
    num_species<-length(listOfdfChromSize)
  }

 # processing for main plot

    # Monocen
  {
    rownumber<-2
    chromosome_ns<-sapply(listOfdfChromSize, function(x) nrow(x) )
    arms_number<-sapply(chromosome_ns, function(x) x*2)
    armRepVector<-lapply(arms_number, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    rownumber<-1
    chromRepVector<-lapply(chromosome_ns, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    x<-list()
    y<-list()
    xm<-list()
    ym<-list()
}
########################################################################################################################################
    for (s in 1:num_species) {

      if(class(listOfdfChromSize[[s]])=="data.frame"){
        #######################################################################################################

      if(attr(listOfdfChromSize[[s]], "cenType")=="monocen"){       # monocen

        #######################################################################################################
      croybot<-list()
      croytop<-list()
      croxleft<-list()
      croxright<-list()
      for (i in 1:length(armRepVector[[s]])){
        croybot[i]<-tryCatch(list(c(karHeight,
                           rep((karHeight-(listOfdfChromSize[[s]][,"longArmSize"]*normalizeToOne)[i]),2),
                           karHeight
                          )#c
                        ), error=function(e) NA ) # list
        croytop[i]<-tryCatch( list(c((karHeight+(centromereSize*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] ),
                           rep((karHeight+(centromereSize*normalizeToOne)),2),
                           (karHeight+(centromereSize*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] )
        ) # c
        ), error=function(e) NA) # list
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

      ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)}) ) + (karHeiSpace*(s-1)  )

      } # fi is monocen
        ###########################################################################################################

      if (attr(listOfdfChromSize[[s]], "cenType")=="holocen" ) {

        ##############################################################
      croytop<-list()
      croxleft<-list()
      croxright<-list()

      for (i in 1:length(chromRepVector[[s]])){
        croytop[i]<-tryCatch( list(c(   (karHeight/2+(listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]   ), # 0+
                                        rep(karHeight/2,2),                                                              # 0,2
                                        (karHeight/2+(listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]  )   # 0+
        ) # c
        ),error=function(e) NA)  # list
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

      # listOfdfChromSize[[s]]<-x[[s]]

      ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + (karHeiSpace*(s-1)  )
    } # holocen if
      } # data.frmae
} # for species
    ###################################################################################################

   if (length(ym)==1){
      karSepar=FALSE
    }

    #
    #	reducing distance among OTUs
    #

    names(ym)<-names(listOfdfChromSize)

    ymCopyC<-ymCopy2<-ymCopy<-ym # important must stay here before modifying ym

# suppressWarnings(
    if(is.na(addMissingOTUAfter[1])){
      if(karSepar){
        for (s in 1:(length(ym)-1)) {
          diffnext<-abs( min(ym[[s+1]] ) - max(ym[[s]]) )
          ym[[s+1]]=ym[[s+1]]-diffnext
          ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
          # message(crayon::red(paste0("ym",amoSepar2 ) ) )
          ym[[s+1]]=ym[[s+1]]+amoSepar2

        }
      }
    }
# )

for (s in 1:length(ym)) {
  if(all(is.null(ym[[s]])) ){
    # y[[s]] <-NA
    x[[s]] <-NA
    ym[[s]]<-NA
    xm[[s]]<-NA
    listOfdfChromSize[[s]]<-NA
  } # for species
} #fi

{
    areNA<-which(is.na(ym))

    listOfdfChromSizenoNA<-listOfdfChromSize
    listOfdfChromSizenoNA[areNA]<-NA
    listOfdfChromSizenoNA<-listOfdfChromSizenoNA[!is.na(listOfdfChromSizenoNA)]

    xmnoNA<-xm
    xmnoNA[areNA]<-NA
    xmnoNA<-xmnoNA[!is.na(xmnoNA)]
    for (i in 1:length(xmnoNA)){
      attr(xmnoNA[[i]],"cenType") <- attr(listOfdfChromSizenoNA[[i]],"cenType")
    }

    ymnoNA<-ym
    ymnoNA[areNA]<-NA
    ymnoNA<-ymnoNA[!is.na(ymnoNA)]


    x[areNA]<-NA
    x<-x[!is.na(x)]

    y<-lapply(1:length(ymnoNA), function(s) base::split(ymnoNA[[s]], row(ymnoNA[[s]] )) )
}
    if(!karIndex){
      xlimLeftMod<-xlimLeftMod*(1/3)
    }

    leftmodifier<-(xlimLeftMod*karIndexPos)+chrWidth

    #####################################################################################################################

    graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-leftmodifier),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                   ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) , ylab = "", xaxt='n',
                   # xlab="", yaxt='n',main = NULL, frame.plot = FALSE, ...)
    xlab="", yaxt='n',main = NULL, frame.plot = FALSE)

    if (Mb){
      opar<-graphics::par(no.readonly = TRUE)
      on.exit(suppressWarnings(par(opar)) )
      par(las=1)
      mtext("Mb", side=2, line=ylabline, at=max(unlist(y), na.rm = TRUE) )
    }

    ######################################################################################################################
    {
    xsizeplot<-(max(unlist(x), na.rm=TRUE)+xlimRightMod )- ( (min(unlist(x), na.rm=TRUE)-(leftmodifier)) )
    ysizeplot<- max(unlist(y), na.rm=TRUE)
    yfactor<-(ysizeplot/xsizeplot)*dotRoundCorr

    # plot types

      if(roundness>20){
        lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrColor),
                                               x=x[[s]],
                                               y=y[[s]]
                                        )#m
        )#l

      } else { # if roundness <20

        n2<-n
        pts_1 <- seq(-pi/2, 0, length.out = n2)
        pts_2 <- seq( 0, pi/2, length.out = n2)
        pts_3 <- seq(pi, pi*1.5, length.out = n2)
        pts_4 <- seq(pi/2, pi, length.out = n2)
        r2 <- chrWidth/(roundness*2)
        yMod<-y
        # str(yMod)

        topBotline_x<-list()
        x2_1<-list()
        x2_2<-list()
        y2<-list()
        topline_y<-list()
        xy_1<-list()
        xy_2<-list()
        newLongx<-list()
        newLongy<-list()
        bottomline_y<-list()

        xy_1<-list()
        xy_2<-list()

        # NEW round up and down
        y2_1<-list()
        y2_2<-list()
        xy_3<-list()
        xy_4<-list()

        for (s in 1:length(yMod) ) {
          if(class(listOfdfChromSizenoNA[[s]])=="data.frame"){
            ########################################################

          if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="monocen" ) {

            ########################################################

            if (centromereSize>0) {

          topBotline_x[[s]]<-list()
          x2_1[[s]]<-list()
          x2_2[[s]]<-list()
          for (c in 1 : length(yMod[[s]]) ) {
            topBotline_x[[s]][[c]]<-c(min(x[[s]][[c]])+r2,max(x[[s]][[c]])-r2 )
            x2_1[[s]][[c]]<-min(x[[s]][[c]])+r2
            x2_2[[s]][[c]]<-max(x[[s]][[c]])-r2
          } # f
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
            xy_1[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_1)*1, y2[[s]][[p]] + (r2 * cos(pts_1)*yfactor) )
            xy_2[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_2)*1, y2[[s]][[p]] + (r2 * cos(pts_2)*yfactor) )
            yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]-r2*yfactor
            newLongx[[s]][[p]]<-c(x[[s]][[p]],xy_1[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_2[[s]][[p]][,1] )
            newLongy[[s]][[p]]<-c(yMod[[s]][[p]],xy_1[[s]][[p]][,2],topline_y[[s]][[p]],xy_2[[s]][[p]][,2] )
          } # for
          for (q in 1: (length(yMod[[s]])/2 ) ) {
            bottomline_y[[s]][[q]]<-rep(min(yMod[[s]][[q]]),2)
            y2[[s]][[q]]<-min(y[[s]][[q]])+r2*yfactor
            xy_1[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_3)*1, y2[[s]][[q]] + (r2 * cos(pts_3)*yfactor) )
            xy_2[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_4)*1, y2[[s]][[q]] + (r2 * cos(pts_4)*yfactor) )
            yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]+r2*yfactor
            newLongx[[s]][[q]]<-c(x[[s]][[q]][1:2],xy_2[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_1[[s]][[q]][,1],x[[s]][[q]][3:4] )
            newLongy[[s]][[q]]<-c(yMod[[s]][[q]][1:2],xy_2[[s]][[q]][,2],bottomline_y[[s]][[q]],xy_1[[s]][[q]][,2],yMod[[s]][[q]][3:4] )

          } # for q

          } # else if cen > 0

            else { # cen ==0
              for (s in 1:length(yMod) ) {

                if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="monocen" ) {

                  topBotline_x[[s]]<-list()
                  x2_1[[s]]<-list()
                  x2_2[[s]]<-list()
                  for (c in 1 : length(yMod[[s]]) ) {
                    topBotline_x[[s]][[c]]<-c(min(x[[s]][[c]])+r2,max(x[[s]][[c]])-r2 )
                    x2_1[[s]][[c]]<-min(x[[s]][[c]])+r2
                    x2_2[[s]][[c]]<-max(x[[s]][[c]])-r2
                  } # f

                  topline_y[[s]]<-list()

                  y2_1[[s]]<-list() # NEW
                  y2_2[[s]]<-list() # NEW

                  xy_1[[s]]<-list()
                  xy_2[[s]]<-list()

                  xy_3[[s]]<-list() # NEW
                  xy_4[[s]]<-list() # NEW

                  newLongx[[s]]<-list()
                  newLongy[[s]]<-list()
                  bottomline_y[[s]]<-list()

                  for (p in ( (length(yMod[[s]])/2)+1 ): length(yMod[[s]]) ) {
                    bottomline_y[[s]][[p]]<-rep(min(yMod[[s]][[p]]),2)

                    topline_y[[s]][[p]]<-rep(max(yMod[[s]][[p]]),2)

                    y2_1[[s]][[p]]<-max(y[[s]][[p]])-r2*yfactor
                    y2_2[[s]][[p]]<-min(y[[s]][[p]])+r2*yfactor

                    xy_1[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_1)*1, y2_1[[s]][[p]] + (r2 * cos(pts_1) *yfactor) )
                    xy_2[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_2)*1, y2_1[[s]][[p]] + (r2 * cos(pts_2) *yfactor) )

                    xy_3[[s]][[p]] <- cbind(x2_1[[s]][[p]] + r2 * sin(pts_3), y2_2[[s]][[p]] + (r2 * cos(pts_3) *yfactor) ) # new
                    xy_4[[s]][[p]] <- cbind(x2_2[[s]][[p]] + r2 * sin(pts_4), y2_2[[s]][[p]] + (r2 * cos(pts_4) *yfactor) ) # new

                    yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==max(yMod[[s]][[p]]))]-r2*yfactor
                    yMod[[s]][[p]][which(yMod[[s]][[p]]==min(yMod[[s]][[p]]))]<-yMod[[s]][[p]][which(yMod[[s]][[p]]==min(yMod[[s]][[p]]))]+r2*yfactor

                    newLongx[[s]][[p]]<-c(x[[s]][[p]][1:2],xy_4[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_3[[s]][[p]][,1],
                                          x[[s]][[p]][3:4],xy_1[[s]][[p]][,1],topBotline_x[[s]][[p]],xy_2[[s]][[p]][,1])

                    newLongy[[s]][[p]]<-c(yMod[[s]][[p]][1:2],xy_4[[s]][[p]][,2],bottomline_y[[s]][[p]],xy_3[[s]][[p]][,2],
                                          yMod[[s]][[p]][3:4],xy_1[[s]][[p]][,2],topline_y[[s]][[p]],xy_2[[s]][[p]][,2])

                  } # for
                  for (q in 1: (length(yMod[[s]])/2 ) ) {
                    bottomline_y[[s]][[q]]<-rep(min(yMod[[s]][[q]]),2)
                    topline_y[[s]][[q]]   <-rep(max(yMod[[s]][[q]]),2)

                    y2_1[[s]][[q]]<-max(y[[s]][[q]])-r2*yfactor# new
                    y2_2[[s]][[q]]<-min(y[[s]][[q]])+r2*yfactor# new

                    xy_1[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_1)*1, y2_1[[s]][[q]] + (r2 * cos(pts_1)*yfactor) )
                    xy_2[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_2)*1, y2_1[[s]][[q]] + (r2 * cos(pts_2)*yfactor) )

                    xy_3[[s]][[q]] <- cbind(x2_1[[s]][[q]] + r2 * sin(pts_3), y2_2[[s]][[q]] + (r2 * cos(pts_3) *yfactor) ) # new
                    xy_4[[s]][[q]] <- cbind(x2_2[[s]][[q]] + r2 * sin(pts_4), y2_2[[s]][[q]] + (r2 * cos(pts_4) *yfactor) ) # new

                    yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==min(yMod[[s]][[q]]))]+r2*yfactor
                    yMod[[s]][[q]][which(yMod[[s]][[q]]==max(yMod[[s]][[q]]))]<-yMod[[s]][[q]][which(yMod[[s]][[q]]==max(yMod[[s]][[q]]))]-r2*yfactor

                    newLongx[[s]][[q]]<-c(x[[s]][[q]][1:2],xy_4[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_3[[s]][[q]][,1],
                                          x[[s]][[q]][3:4],xy_1[[s]][[q]][,1],topBotline_x[[s]][[q]],xy_2[[s]][[q]][,1])

                    newLongy[[s]][[q]]<-c(yMod[[s]][[q]][1:2],xy_4[[s]][[q]][,2],bottomline_y[[s]][[q]],xy_3[[s]][[q]][,2],
                                          yMod[[s]][[q]][3:4],xy_1[[s]][[q]][,2],topline_y[[s]][[q]],xy_2[[s]][[q]][,2])

                  } # for q
                } # monocen
              } # for species
            } # else cen == 0
    } # if monocen
            ###########################################################

          else if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="holocen" ) {

            ###########################################################
            topline_y[[s]]<-list()
            bottomline_y[[s]]<-list()
            topBotline_x[[s]]<-list()
            x2_1[[s]]<-list()
            x2_2[[s]]<-list()
            y2_1[[s]]<-list()
            y2_2[[s]]<-list()
            xy_1[[s]]<-list()
            xy_2[[s]]<-list()
            xy_3[[s]]<-list()
            xy_4[[s]]<-list()
            newLongx[[s]]<-list()
            newLongy[[s]]<-list()

            for (m in 1: length(yMod[[s]]) ) {
              # topline_y<-rep(max(y),2)
              topline_y[[s]][[m]]   <-rep(max(yMod[[s]][[m]]),2)
              bottomline_y[[s]][[m]]<-rep(min(yMod[[s]][[m]]),2)
              topBotline_x[[s]][[m]]<-c(min(x[[s]][[m]])+r2,max(x[[s]][[m]])-r2 )
              yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]<-yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]-r2*yfactor
              yMod[[s]][[m]][which(yMod[[s]][[m]]==min(yMod[[s]][[m]]))]<-yMod[[s]][[m]][which(yMod[[s]][[m]]==min(yMod[[s]][[m]]))]+r2*yfactor

              x2_1[[s]][[m]]<-min(x[[s]][[m]])+r2
              x2_2[[s]][[m]]<-max(x[[s]][[m]])-r2
              y2_1[[s]][[m]]<-max(y[[s]][[m]])-r2*yfactor
              y2_2[[s]][[m]]<-min(y[[s]][[m]])+r2*yfactor
              xy_1[[s]][[m]] <- cbind(x2_1[[s]][[m]] + r2 * sin(pts_1), y2_1[[s]][[m]] + (r2 * cos(pts_1) *yfactor) )
              xy_2[[s]][[m]] <- cbind(x2_2[[s]][[m]] + r2 * sin(pts_2), y2_1[[s]][[m]] + (r2 * cos(pts_2) *yfactor) )
              xy_3[[s]][[m]] <- cbind(x2_1[[s]][[m]] + r2 * sin(pts_3), y2_2[[s]][[m]] + (r2 * cos(pts_3) *yfactor) )
              xy_4[[s]][[m]] <- cbind(x2_2[[s]][[m]] + r2 * sin(pts_4), y2_2[[s]][[m]] + (r2 * cos(pts_4) *yfactor) )
              newLongx[[s]][[m]]<-c(x[[s]][[m]][1:2],xy_4[[s]][[m]][,1],topBotline_x[[s]][[m]],xy_3[[s]][[m]][,1],
                                    x[[s]][[m]][3:4],xy_1[[s]][[m]][,1],topBotline_x[[s]][[m]],xy_2[[s]][[m]][,1])
              newLongy[[s]][[m]]<-c(yMod[[s]][[m]][1:2],xy_4[[s]][[m]][,2],bottomline_y[[s]][[m]],xy_3[[s]][[m]][,2],
                                    yMod[[s]][[m]][3:4],xy_1[[s]][[m]][,2],topline_y[[s]][[m]],xy_2[[s]][[m]][,2])
            } # for


          } # holocen
      } # d.f.
   } # for species
}  # roundness

newLongy<-newLongy[!is.na(newLongy)]
newLongx<-newLongx[!is.na(newLongx)]

  lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                         col=chrColor,
                                                                         lwd=lwd.chr,
                                                                         border=chrColor),
                                         x=newLongx[[s]],
                                         y=newLongy[[s]]
                                  )#m
  )#l


} # plot types

  ####################################################################################################
  #
  #   ruler calculate
  #
  ################################
    # listOfdfChromSize<-rev(listOfdfChromSize)

    {
      monocenVector2<-integer()
      for(i in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[i]])=="data.frame" ){
        if(attr(listOfdfChromSize[[i]],"cenType")=="monocen"){
          monocenVector2<-c(monocenVector2,i)
          attr(listOfdfChromSize[[i]],"position") <- i
          }
        }
      }

      monocenNames2<-names(listOfdfChromSize)[monocenVector2]
      listOfdfChromSizeMonocen<-listOfdfChromSize[monocenVector2]


      holocenVector2<-integer()
      for(i in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[i]])=="data.frame" ){
        if(attr(listOfdfChromSize[[i]],"cenType")=="holocen"){
          holocenVector2<-c(holocenVector2,i)
          attr(listOfdfChromSize[[i]],"position") <- i
          }
        }
      }
      holocenNames2<-names(listOfdfChromSize)[holocenVector2]
      listOfdfChromSizeHolocen<-listOfdfChromSize[holocenVector2]
    }

  if (ruler){
    if (length( which(names(listOfdfChromSize) %in% monocenNames) )>0 ){
    # for (x in monocenVector2){

    maxShortRound<-lapply(1:length(listOfdfChromSizeMonocen), function(x) tryCatch(ceiling(max(listOfdfChromSizeMonocen[[x]]$shortArmSize)), error=function(e) NA ) )
    maxLongRound <-lapply(1:length(listOfdfChromSizeMonocen), function(x) tryCatch(ceiling(max(listOfdfChromSizeMonocen[[x]]$longArmSize)), error=function(e) NA ) )

    fromZerotoMaxShort<-lapply(maxShortRound, function(x) tryCatch(seq(0,x, by=1), error=function(e) NA ) )
    fromZerotoMaxLong<-lapply(maxLongRound, function(x) tryCatch(seq(0,x, by=1), error=function(e) NA ) )

    ycoordLongRound <-lapply(1:length(fromZerotoMaxLong), function(x){
      pos<-as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxLong[[x]]), function(y) (karHeight-(fromZerotoMaxLong[[x]][y]*normalizeToOne) ) + (1*karHeiSpace*(pos-1))
        ) #l
      ) #u
    }
    ) #l
    names(ycoordLongRound)<-(monocenNames2)

    ycoordShortRound  <-lapply(1:length(fromZerotoMaxShort), function(x){
      pos<-as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxShort[[x]]), function(y)
          (karHeight+(centromereSize*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1))
        ) # l
      ) # u
    }
    ) #l

    names(ycoordShortRound)<-(monocenNames2)

    # suppressWarnings(
    if(is.na(addMissingOTUAfter[1])){
      if(karSepar){
        for (s in 1:(length(ymCopy)-1) ) {

          diffnext<-abs(min(ymCopy[[s+1]] ) - max(ymCopy[[s]]) )
          ymCopy[[s+1]] <- ymCopy[[s+1]]-diffnext
          ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
          # message(crayon::red(paste0("ycoordLongRound",amoSepar2 ) ) )
          ymCopy[[s+1]] <- ymCopy[[s+1]]+amoSepar2

          nameYm<-names(ymCopy)[[s+1]]

          if(length(which(names(ycoordLongRound)==nameYm))>0 ){
          ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] <- ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] -diffnext
          ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] <- ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] + amoSepar2

          ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]<- ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]-diffnext
          ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]<- ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]+ amoSepar2

          }
        }
      }
    }

    ycoordLongRound  <-ycoordLongRound[!is.na(ycoordLongRound)]
    ycoordShortRound <-ycoordShortRound[!is.na(ycoordShortRound)]

    ########################################################################################3
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
      graphics::axis(side=2,
                     at=unlist(ycoordShortRound[[x]]),
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
    } # monocen

    ######################################################################################### holocen


    if (length( which(names(listOfdfChromSize) %in% holocenNames) ) > 0 ) {
    maxChrRound<-lapply(1:length(listOfdfChromSizeHolocen), function(x) tryCatch(ceiling(max(listOfdfChromSizeHolocen[[x]]$chrSize)), error=function(e) NA ) )

    fromZerotoMaxChr<-lapply(maxChrRound, function(x) tryCatch(seq(0,x, by=1), error=function(e) NA ) )

    ycoordChrRound  <- lapply(1:length(fromZerotoMaxChr), function(x) {
      pos<-as.numeric(attr(listOfdfChromSizeHolocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxChr[[x]]), function(y)
          (karHeight/2+(fromZerotoMaxChr[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1)) )         # 0+(from
      ) # u
    }
    ) # l
    names(ycoordChrRound)<-holocenNames2

    # suppressWarnings(
      if(is.na(addMissingOTUAfter[1])){
        if(karSepar){
          for (s in 1:(length(ymCopy2)-1) ) {

            diffnext<-abs(min(ymCopy2[[s+1]] ) - max(ymCopy2[[s]]) )
            ymCopy2[[s+1]]=ymCopy2[[s+1]]-diffnext
            ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
            ymCopy2[[s+1]]=ymCopy2[[s+1]]+amoSepar2

            nameYm<-names(ymCopy2)[[s+1]]

            if(length(which(names(ycoordChrRound)==nameYm))>0 ){
            ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] <- ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] -diffnext
            ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] <- ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] + amoSepar2
            } # if
          } # for
        } # redu if
      } # if
    # )

    ycoordChrRound<-ycoordChrRound[!is.na(ycoordChrRound)]

    ####################
    #
    #   add rulers
    #
    ####################

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
    } # end holocen
}   # end rulers if

  #
  #   groups line > 1.1.0
  #

    if("group" %in% colnames(dfChrSizeInternal)){
    for (s in 1:length(xmnoNA)){
      ngroup<-length(table(listOfdfChromSizenoNA[[s]]$group ) )
      for (g in 1: ngroup){
        x0= xmnoNA[[s]][,3][ifelse(length(cumsum(table(listOfdfChromSizenoNA[[s]]$group))[g-1] )==0,
                               1,
                               cumsum(table(listOfdfChromSizenoNA[[s]]$group) )[g-1]+1
        )]
        x1= xmnoNA[[s]][,3][cumsum(table(listOfdfChromSizenoNA[[s]]$group) )[g]  ]+chrWidth
      segments(x0=x0,
               y0=(min(ymnoNA[[s]])-(distTextChr/3)),
               x1=x1,
               y1=(min(ymnoNA[[s]])-(distTextChr/3) )
      ) # seg
      ########################
      #     group name, after 1.1.0
      ########################3
      text( (x0+x1)/2,
            (min(ymnoNA[[s]])-(distTextChr/3)*2 ),
           labels = names( table(listOfdfChromSizenoNA[[s]]$group)[g] ),
           cex=indexIdTextSize
      )# text end
      } # for group
    } # for sp
  } # if

  ###################################################################################################################3
  #  chromosome names
  #################################

  chrNameDistance<-ifelse(exists("grouporderlist"),3,1)
  # original
  {
  if(chrId=="original"){
      for (i in 1:length(xmnoNA)){
        if(attr(xmnoNA[[i]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}
      graphics::text(xmnoNA[[i]][,3][1:(nrow(xmnoNA[[i]])/armFactor)]+chrWidth/2,
                     rep( (min(ymnoNA[[i]])-(distTextChr/3) * chrNameDistance ),(nrow(xmnoNA[[i]])/armFactor) ),
                     # labels = listOfdfChromSize[[i]][,"chrName"][orderlist[[i]]],
                     labels = listOfdfChromSizenoNA[[i]][,"chrName"],

                     cex=indexIdTextSize
      ) # end graphics::text
      } # for
  } # fi
  else if (chrId=="simple"){
    # Simple numbering from 1 to ...
      for (i in 1:length(xmnoNA)){
      if(attr(xmnoNA[[i]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}
      graphics::text(xmnoNA[[i]][,3][1:(nrow(xmnoNA[[i]])/armFactor)]+chrWidth/2,
                     rep( (min(ymnoNA[[i]])-(distTextChr/3)*chrNameDistance ),(nrow(xmnoNA[[i]])/armFactor) ),
                     # rep( (min(ymnoNA[[i]])-.1),(nrow(xmnoNA[[i]])/2) ),
                     labels = 1:(nrow(xmnoNA[[i]])/armFactor),
                     cex=indexIdTextSize
      ) # t
      } # for
  } # elif
}
  #################################
  # horizontal chromosome index
  #################################

  chrIndDistance<-ifelse(exists("grouporderlist"),4,2)

  chrIndbool<-(chrIndex & "AR" %in% colnames(listOfdfChromSizenoNA[[1]]) )
  if(chrIndbool){
    lapply(1:length(xmnoNA), function(s)
      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+(chrWidth/2)),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)*chrIndDistance)) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("CI",listOfdfChromSizenoNA[[s]][,"CI"] ),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
    lapply(1:length(xmnoNA), function(s)
      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth/2),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)*(chrIndDistance+1) )) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("r",listOfdfChromSizenoNA[[s]][,"AR"] ),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  } # fi

  #################################
  # horizontal chromosome morphology categories
  #################################

  chrIndboolGue<-(morpho & "AR" %in% colnames(listOfdfChromSize[[1]]) )

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
    lapply(1:length(xmnoNA), function(s)
      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth/2),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)*distVectorGue[decVector])) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("Guerra",listOfdfChromSizenoNA[[s]][,"Guerra"]),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
    lapply(1:length(xmnoNA), function(s)
      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth/2),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)*(distVectorGue[decVector]+1))) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("Levan",listOfdfChromSizenoNA[[s]][,"Levan"]),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
    ) # end lapply
  } # fi

  #################################
  # vertical karyotype index
  #################################

  if(karIndex){
    for (i in 1:length(listOfdfChromSizenoNA) ) {
      if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
        if(is.character(names(listOfdfChromSizenoNA)[[i]]  ) ){
          message(crayon::green(paste0(names(listOfdfChromSizenoNA)[[i]],":" ) )
          ) # mess
        }

    ind<-asymmetry(listOfdfChromSizenoNA[[i]])

    if(!is.null(ind)){
        graphics::text( c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                        rep( min(ymnoNA[[i]][,1]), 1 ), #2
                        labels = paste("A ",ind$A ),
                        cex=indexIdTextSize,
                        adj=c(1) # justif
        ) # end graphics::text
        graphics::text(c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                       rep( (min(ymnoNA[[i]][,1])-(distTextChr/3) ) , 1 ), # avoid overlap
                       labels = paste("A2",ind$A2 ),
                       cex=indexIdTextSize,
                       adj=c(1) # 0.5 centered
        ) # end graphics::text
    } # null

     } # if monocen
      else if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="holocen"){
        if(is.character(names(listOfdfChromSizenoNA)[[i]]  ) ){
          message(crayon::green(paste0(names(listOfdfChromSizenoNA)[[i]],":" ) )
          ) # mess
        }
        ind<-asymmetryA2(listOfdfChromSizenoNA[[i]])
        if(!is.null(ind)){
        graphics::text(c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                       (max(ymnoNA[[i]]) + min(ymnoNA[[i]]) ) /2 #(distTextChr/3)   #[,1]  was /3
                       ,labels = paste("A2",ind$A2 ),
                       cex=indexIdTextSize,
                       adj= c(1) # 0.5 centered
        ) # end graphics::text
        } # null
      } # holocen
    } # for
  } # fi

  #########################################################################
  # add species names
  #########################################################################

  distVector<-if(exists("grouporderlist") ){
    a<-c((distTextChr/3)*9 ,(distTextChr/3)*7,(distTextChr/3)*7,(distTextChr/3)*5,(distTextChr/3)*4 )
  } else {
    a<-c((distTextChr/3)*7 ,(distTextChr/3)*5,(distTextChr/3)*5,(distTextChr/3)*3,(distTextChr/3)*3 )
  }

  if(addOTUName){
    lapply(1:length(xmnoNA), function(s) {
      if(attr(xmnoNA[[s]],"cenType")=="holocen") {
        decVector<-5
        }
      graphics::text( c( (xmnoNA[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                      ydistance <- (min(ymnoNA[[s]]) - distVector[decVector] ),
                      labels = paste("",names(listOfdfChromSizenoNA)[[s]] ),
                      cex=OTUTextSize,
                      adj=c(0) # justif 0 =left
      ) # end graphics::text
    }
    ) # end lapply
  } # fi

  ##########################################################################################################3
  #
  #   painting Marks monocen
  #
  ############################################################################################################

  if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){

    listOfdfMarkPosSq<-list()
    yMark<-list()
    xMark<-list()
    j<-1

    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="square"),])>0){
        listOfdfMarkPosSq<-c(listOfdfMarkPosSq,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="square"),]))
        names(listOfdfMarkPosSq)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosSq)>0){
    for (sm in 1:length(listOfdfMarkPosSq)) {
      yMark1<-NULL
      xMark1<-NULL
      for (m in 1:nrow(listOfdfMarkPosSq[[sm]]) ){
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
    } #     if(length(listOfdfMarkPosSq)>0){

    else {remove(listOfdfMarkPosSq)}

  } # if presence end painting marks


  # square labels not cen

  if(legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") ) {
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

  ##################################################################################################3
  #
  #   painting Marks square holocen
  #
  ########################################

  if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
    listOfdfMarkPosSq<-list()
    yMark<-list()
    xMark<-list()
    j<-1
    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]
      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="square"),])>0){
        listOfdfMarkPosSq<-c(listOfdfMarkPosSq,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="square"),]) )
        names(listOfdfMarkPosSq)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosSq)>0){
      for (sm in 1:length(listOfdfMarkPosSq)) {

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
    } #     if(length(listOfdfMarkPosSq)>0){

    else {remove(listOfdfMarkPosSq)}

  } # painting marks square fi

  if(legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") ) {
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

  ##########################################################################################################################
  # dot style of marks                        monocen
  ##########################################################################################################################

  if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {
    listOfdfMarkPosCr<-list()

    xfactor<-(xsizeplot/ysizeplot  )/dotRoundCorr

    yMarkCr<-list()
    xMarkCr<-list()
    rad<-list()
    colCr<-list()
    j<-1

    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="dots"),])>0){
      listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="dots"),]) )
      names(listOfdfMarkPosCr)[[j]]<-currName
      j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0){

    for (k in 1:length(listOfdfMarkPosCr) ) {

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

    } # end for k OTU

    #####################
    #   add to plot MarkCrs DOTS
    #####################

    lapply(1:length(xMarkCr), function(w)
      lapply(1:length(xMarkCr[[w]] ), function(u)
        mapply(function(x,y,r,z) {
          pts2=seq(0, 2 * pi, length.out = n*4)
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

    } #     if(length(listOfdfMarkPosCr)>0){

   else {remove(listOfdfMarkPosCr)}
  } # end painting MarkCrs

  # dots
  if(legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCr") ) {
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

  ##############################################################################################################################
  #
  # dot style of marks holocen
  #
  ########################################

  if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ){
    listOfdfMarkPosCr<-list()
    xfactor<-(xsizeplot/ysizeplot  )/dotRoundCorr

    yMarkCr<-list()
    xMarkCr<-list()
    rad<-list()
    colCr<-list()
    j<-1

    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]

      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="dots"),])>0){
        listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="dots"),]) )
        names(listOfdfMarkPosCr)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0){

    for (k in 1:length(listOfdfMarkPosCr) ) {

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
          pts2=seq(0, 2 * pi, length.out = n*4)
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

  } #     if(length(listOfdfMarkPosCr)>0){
  else {remove(listOfdfMarkPosCr)}

  } # end painting MarkCrs

  # dots
  if(legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCr") ) {
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

  ################################
  #
  #   centromeres calculate
  #
  ################################

  if(centromereSize>0){

    CentsList<-lapply(1:length(listOfdfChromSize), function(x) tryCatch(rep(0, nrow(listOfdfChromSize[[x]]) ), error=function(e) NA ) )
    names(CentsList)<-names(listOfdfChromSize)

    ycoordCents <-list()

    for (i in 1:length(CentsList)){

      ycoordCents[[i]] <- t(replicate(length(CentsList[[i]])*2,(c(rep(  karHeight+                                (karHeiSpace*(i-1)),2  ),
                                              rep(  karHeight+(centromereSize*normalizeToOne)+(karHeiSpace*(i-1)),2  )
      )      )      ) # r
      ) #t
    } # for

ycoordCents[areNA]<-NA

# suppressWarnings(
if(is.na(addMissingOTUAfter[1])){
    if(karSepar){
      for (s in 1:(length(ymCopyC)-1) ) {
        diffnext<-abs(min(ymCopyC[[s+1]] ) - max(ymCopyC[[s]]) )
        ymCopyC[[s+1]]=ymCopyC[[s+1]]-diffnext
        ycoordCents[[s+1]] <-ycoordCents[[s+1]] -diffnext

        ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)

        ymCopyC[[s+1]]=ymCopyC[[s+1]]+amoSepar2
        ycoordCents[[s+1]] <-ycoordCents[[s+1]] +amoSepar2
      } # for
    } # redu
}

    ycoordCentsS<- lapply(1:length(ycoordCents), function(j) tryCatch(base::split(ycoordCents[[j]], row(ycoordCents[[j]]) ), error=function(e) NA ) )

    names(ycoordCentsS)<-names(listOfdfChromSize)
    ycoordCentsS<-tryCatch(ycoordCentsS[which(names(ycoordCentsS) %in% monocenNames)] , error=function(e) NA )

    ycoordCentsS<-ycoordCentsS[!is.na(ycoordCentsS)]
    ycoordCents<-ycoordCents[!is.na(ycoordCents)]

    xcoordCents<-lapply(1:length(xmnoNA), function(x) cbind(xmnoNA[[x]][,2:3],xmnoNA[[x]][,2:3] ) )

    xcoordCentsS<- lapply(1:length(xcoordCents), function(j) base::split(xcoordCents[[j]], row(xcoordCents[[j]]) ) )

    names(xcoordCentsS)<-names(listOfdfChromSizenoNA)

    xcoordCentsS<-tryCatch(xcoordCentsS[which(names(xcoordCentsS) %in% monocenNames)], error= function(e) NA )
    xcoordCentsS<-xcoordCentsS[!is.na(xcoordCentsS)]

    cenBorder<-cenColor
    if(chrColor!=cenColor){
      cenBorder<-chrColor
    }

    if(length(xcoordCentsS)>0){
    lapply(1:length(xcoordCentsS), function(w) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                        col=z,
                                                                                        lwd=lwd.chr,
                                                                                        border=z),
                                                      x=xcoordCentsS[[w]],
                                                      y=ycoordCentsS[[w]],
                                                      z=cenBorder
                                                ) #m
    ) #l
    } # if length xcoordCentsS
  } # end centromeres 0

  ###################
  #   centromere with marks
  ###################

  if(centromereSize>0){
  if (exists("listOfdfDataCen") & exists("dfMarkColorInternal") ) {
    names(xcoordCents)<-names(listOfdfChromSizenoNA) #1:length(xcoordCents)

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
  if(legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfDataCen") ) {
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
} # marks centromere > 0

  ##############################################
  #
  #   labels to the right
  #
  ##############################################
  {
  xNoNA<-x
  xNoNA[areNA]<-NA
  xNoNA<-xNoNA[!is.na(xNoNA)]
  yNoNA<-y
  yNoNA[areNA]<-NA
  yNoNA<-yNoNA[!is.na(yNoNA)]
}
  if(legend=="aside" & exists("dfMarkColorInternal") ){

  plotlabelsright(xNoNA,yNoNA, markLabelSpacer,chrWidth,dfMarkColorInternal,allMarkMaxSize,normalizeToOne,
                              markLabelSize,xfactor,legendWidth, legendHeight, n*4)
  }
}  # end of function

#' @export
#' @rdname idiogramFISH-deprecated
#' @inheritParams plotIdiograms
plotIdiogramsHolo <- function() {
  message(crayon::red(paste0(
    "plotIdiogramsHolo() is deprecated. ",
    "Please use plotIdiograms() instead",
    "") ) )
  plotIdiograms()
}
