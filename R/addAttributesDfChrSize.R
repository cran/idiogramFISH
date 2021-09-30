#' addAttributesDfChrSize

#' @description returns all cols. as character
#' @keywords internal
#' @return list of data.frames
#' @import crayon

addAttributesDfChrSize<-function(listOfdfChromSize,threshold,specialOTUNames
                                 ,centromereSize,MbThreshold,cenFactor
                                 ,chrWidth,specialChrWidth,squareness){

for (i in 1:length(listOfdfChromSize)) {

  #
  # remove columns without info. per karyotype
  #

  listOfdfChromSize[[i]][listOfdfChromSize[[i]]==""]<-NA
  listOfdfChromSize[[i]]<-  listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

  # Does the data.frame have short and long info?
  message("\nChecking columns from dfChrSize\n"
  ) # mess

  #################################################################################################
  #
  #   let see if it is monocen
  #

  if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                     colnames(listOfdfChromSize[[i]]) ) )==0 ) {
    message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
     ) # mess
    message(crayon::green(paste("\nOTU ",names(listOfdfChromSize)[[i]],"has all columns with info to have monocen. If not, you have to clean your data"))
    )# message
    attr(listOfdfChromSize[[i]], 'cenType') <- "monocen"
    attr(listOfdfChromSize[[i]], 'indexStatus') <- "missing"

    #
    #   if larger than 100 000 reduce
    #

    myn<-max(pmax(listOfdfChromSize[[i]]$longArmSize+listOfdfChromSize[[i]]$shortArmSize) )
    exp<-floor(log(myn , base = 10 ) )
    divisor2<-10^exp
    threshold2<- threshold/10

    divisor2<-ifelse((myn/divisor2)<threshold2,divisor2<-10^(exp-1),divisor2)

    attr(listOfdfChromSize[[i]],"divisor") <- divisor2

    listOfdfChromSize[[i]]$longArmSize<-listOfdfChromSize[[i]]$longArmSize/divisor2
    listOfdfChromSize[[i]]$shortArmSize<-listOfdfChromSize[[i]]$shortArmSize/divisor2

    if(divisor2 >= MbThreshold){
      attr(listOfdfChromSize[[i]],"units") <- "Mb"
      attr(listOfdfChromSize[[i]],"ytitle") <- "Mb"
    } else {
      attr(listOfdfChromSize[[i]],"units") <- "notMb"
      attr(listOfdfChromSize[[i]],"ytitle") <- "notMb"
    }
    if(attr(listOfdfChromSize[i],"name") %in% specialOTUNames){
      attr(listOfdfChromSize[[i]],"ytitle") <- "cM"
    }

    if(!is.na(centromereSize)){
      centromereSize2<-centromereSize
    } else {
      centromereSize2<-divisor2
    }

    if(attr(listOfdfChromSize[[i]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }
    r2 <- chrWidth2/(squareness*2)

    attr(listOfdfChromSize[[i]],"r2") <- r2

    attr(listOfdfChromSize[[i]],"centromere") <- (centromereSize2/divisor2)*cenFactor
    listOfdfChromSize[[i]]$centromereSize     <- (centromereSize2/divisor2)*cenFactor

  } # if monocen success

  ##############################################################################################3
  #   let see if it is holocen
  #

  else if(length( setdiff(c("chrName", "chrSize"),
                          colnames(listOfdfChromSize[[i]]) ) )==0 ){
    message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
     ) # mess
    message(crayon::green(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," has all columns with info to have holocen. If not, you have to clean your data")))
    ) # message
    attr(listOfdfChromSize[[i]], 'cenType') <- "holocen"
    attr(listOfdfChromSize[[i]], 'indexStatus') <- "missing"

    #
    #   if larger than 100 000 reduce
    #

    myn<-max(listOfdfChromSize[[i]]$chrSize)
    exp<-floor(log(myn , base = 10 ) )

    divisor2   <- 10^exp
    threshold2 <- threshold/10

    ifelse( (myn/divisor2)<threshold2, divisor2<-10^(exp-1) ,divisor2)

    attr(listOfdfChromSize[[i]],"divisor") <- divisor2
    listOfdfChromSize[[i]]$chrSize <- listOfdfChromSize[[i]]$chrSize/divisor2

    if(divisor2 >= MbThreshold){
      attr(listOfdfChromSize[[i]],"units") <- "Mb"
      attr(listOfdfChromSize[[i]],"ytitle") <- "Mb"
    } else {
      attr(listOfdfChromSize[[i]],"units") <- "notMb"
      attr(listOfdfChromSize[[i]],"ytitle") <- "notMb"
    }
    if(attr(listOfdfChromSize[i],"name") %in% specialOTUNames){
      attr(listOfdfChromSize[[i]],"ytitle") <- "cM"
      # attr(listOfdfChromSize[[i]],"units") <- "cM"
    }

    if(!is.na(centromereSize)){
      centromereSize2<-centromereSize
    } else {
      centromereSize2<-divisor2
    }

    if(attr(listOfdfChromSize[[i]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }
    r2 <- chrWidth2/(squareness*2)

    attr(listOfdfChromSize[[i]],"r2") <- r2

    attr(listOfdfChromSize[[i]],"centromere") <- (centromereSize2/divisor2)*cenFactor
    listOfdfChromSize[[i]]$centromereSize     <- (centromereSize2/divisor2)*cenFactor

  } # if holocen success

  #
  # let see if it is not monocen
  #

  else if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                          colnames(listOfdfChromSize[[i]]) ) )>0 ){
    message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
    ) # mess
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
    message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
    ) # mess
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
# rebuild with centromereSize
#

listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeCharCols(x) )
return(listOfdfChromSize)
}
##############
addChrSizeColumn<-function(listOfdfChromSize){
  for ( i in 1:length(listOfdfChromSize)) {
    if(class(listOfdfChromSize[[i]])=="data.frame") {
      if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){
        listOfdfChromSize[[i]]<-makeNumCols(listOfdfChromSize[[i]])
        listOfdfChromSize[[i]]$chrSize<-listOfdfChromSize[[i]]$shortArmSize+listOfdfChromSize[[i]]$longArmSize
      } # if
    } # if
  } # for
  return(listOfdfChromSize)
}

################### ordering function
addNeworderColumn<-function(listOfdfChromSize,orderlist){
  for (s in 1:length(listOfdfChromSize)){
    if(class(listOfdfChromSize[[s]])=="data.frame") {
      listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][orderlist[[s]], ] # important THIS orders
      listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
    }
  } # end for
  return(listOfdfChromSize)
}
###################
addAttributesDfChrSizeSimple<-function(listOfdfChromSize,centromereSize=NA,threshold=35){
  for (i in 1:length(listOfdfChromSize)) {

    #
    # remove columns without info. per karyotype
    #

    listOfdfChromSize[[i]][listOfdfChromSize[[i]]==""]<-NA
    listOfdfChromSize[[i]]<-  listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

    # Does the data.frame have short and long info?
    message("\nChecking columns from dfChrSize\n" )  # mess

    #################################################################################################
    #
    #   let see if it is monocen
    #

    if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                       colnames(listOfdfChromSize[[i]]) ) )==0 ) {
      message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
      message(crayon::green(paste("\nOTU ",names(listOfdfChromSize)[[i]],"has all columns with info to have monocen. If not, you have to clean your data"))
      )# message
      attr(listOfdfChromSize[[i]],'cenType') <- "monocen"

      #
      #   if larger than 100 000 reduce
      #

      myn<-max(pmax(listOfdfChromSize[[i]]$longArmSize+listOfdfChromSize[[i]]$shortArmSize) )
      exp<-floor(log(myn , base = 10 ) )
      divisor2<-10^exp
      threshold2<- threshold/10

      divisor2<-ifelse((myn/divisor2)<threshold2,divisor2<-10^(exp-1),divisor2)

      attr(listOfdfChromSize[[i]],"divisor") <- divisor2

      # listOfdfChromSize[[i]]$longArmSize <- listOfdfChromSize[[i]]$longArmSize /divisor2
      # listOfdfChromSize[[i]]$shortArmSize<- listOfdfChromSize[[i]]$shortArmSize/divisor2

      if(!is.na(centromereSize)){
        centromereSize2<-centromereSize
      } else {
        centromereSize2<-divisor2
      }

      attr(listOfdfChromSize[[i]],"centromere") <- centromereSize2 #/divisor2
      listOfdfChromSize[[i]]$centromereSize     <- centromereSize2 #/divisor2

    } # if monocen success

    ##############################################################################################3
    #   let see if it is holocen
    #

    else if(length( setdiff(c("chrName", "chrSize"),
                            colnames(listOfdfChromSize[[i]]) ) )==0 ){
      message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
      message(crayon::green(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," has all columns with info to have holocen. If not, you have to clean your data")))
      ) # message
      attr(listOfdfChromSize[[i]], 'cenType') <- "holocen"

      #
      #   if larger than 100 000 reduce
      #

      myn<-max(listOfdfChromSize[[i]]$chrSize)
      exp<-floor(log(myn , base = 10 ) )

      divisor2   <- 10^exp
      threshold2 <- threshold/10

      ifelse( (myn/divisor2)<threshold2, divisor2<-10^(exp-1) ,divisor2)

      attr(listOfdfChromSize[[i]],"divisor") <- divisor2

      # listOfdfChromSize[[i]]$chrSize <- listOfdfChromSize[[i]]$chrSize/divisor2

      if(!is.na(centromereSize)){
        centromereSize2<-centromereSize
      } else {
        centromereSize2<-divisor2
      }

      attr(listOfdfChromSize[[i]],"centromere") <- centromereSize2#/divisor2
      listOfdfChromSize[[i]]$centromereSize     <- centromereSize2#/divisor2

    } # if holocen success

    #
    # let see if it is not monocen
    #

    else if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                            colnames(listOfdfChromSize[[i]]) ) )>0 ){
      message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
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
      message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
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
  # rebuild with centromereSize
  #

  listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeCharCols(x) )
  return(listOfdfChromSize)
}

