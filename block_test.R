library(Resonance)
library(R3)
library(Resonate)

file = 'd:/YandexDisk/eyelinesOnline/data/test-2/06.r2e'
blocks <- blockLevelRead(file)

eegBlocks <- Filter(function(b){
  if(!inherits(b, "DataBlock")) return(F)
  if(attr(b, 'stream')!=0) return(F)
  TRUE
}, blocks)

clickBlocks <- Filter(function(b){
  if(!inherits(b, "DataBlock")) return(F)
  if(attr(b, 'stream')!=1) return(F)
  TRUE
}, blocks)



########################

code <- "

require(Resonance)
library(Resonate)
library(eyelinesOnline)

W = c(0.0529078707683507, 0.044014761636564, 0.167599364672095, 
      0.0759448946162227, -0.0173479141391115, -0.114834810452548, 
      0.0513220104026001, -0.0961470319916284, -0.0865414985420686, 
      -0.142950898827934, 0.0697052246066697, 0.0754620613146662, 0.00273139238217478, 
      -0.0208809883935714, 0.0750806209252997, 0.0936616469175329, 
      0.136220872639952, -0.0632560480635951, 0.0127720670137731, -0.0913476275594759, 
      -0.0205313449273005, -0.0640963501272562, -0.10101766339769, 
      0.110485192870993, -0.0779084845068587, 0.139891161668672, -0.174011216755437, 
      0.118871532742326, 0.0360007421696571, 0.175207450800796, 0.0451555001333072, 
      -0.0217888216107314, 0.0270010893217829, 0.188506698385018, -0.111229652335719, 
      0.0801768625019763, -0.205033918836484, 0.0629055068739538, -0.0479604848214012, 
      0.0953377617749893, 0.017178615836179, 0.00352329781697166, 0.0133886945916007, 
      0.0361169910133169, -0.0183143078044107, 0.0150496424340076, 
      -0.0323979812749566, 0.00281554628397186, -0.0151464054062193, 
      0.0052810020043959)

th = -1.08677534936626

ufeats = structure(c(1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 
                     3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 
                     5, 5, 5, 5, 5, 5, 5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 
                     4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 
                     4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), .Dim = c(50L, 
                                                                                    2L))

channels = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 11L, 12L, 14L, 15L, 16L)
low = FALSE
high = 5
A1 = 13
A2 = 14

fixDur = 500
sRate = 500
t = 250
bsln_start = 1
bsln_end = 50

process = function(){
  RM <- diag(nrow=33)[channels,]
  #   FS <- pipeline(
  #     input(1),
  #     signalPreparation(, low=low, high=high, notch=50),
  #     pipe.spatial(, RM),
  #     pipe.references(, c(A1,A2))
  #   )
  
  FS1 <- signalPreparation(input(1), low=low, high=high, notch=50)
  FS2 <- pipe.spatial(FS1, RM)
  FS <- pipe.references(FS2, c(A1,A2))
  
  RA1 <- pipe.decimate(FS, 1, 20 , coef_10000_to_500)
  ev <- input(2)
  RA2 <- cross.windowizeByEvents(RA1, ev, t/20, shift=-t/20)
  RA3 <- pipe.medianWindow(RA2, 1, 12)
  RA4 <- pipe.trof.classifier(RA3, W, th, ufeats )
   createOutput(RA4,'RES')
}
"

########################

A <- matrix(ncol=33,nrow=0)
SI(A) <- SI.channels(channels = 33, samplingRate = 500)
SI(A, 'online') <- T
B <- list()
SI(B) <- SI.event()
SI(B, 'online') <- T

onPrepare(list(A,B), code)


nextEeg <- function(b){
  onDataBlock.double(id = 1, vector = t(b), samples = nrow(b), timestamp = attr(b, 'created'))
}

nextClick <- function(b){
  onDataBlock.message(id = 2, msg = as.character(b), timestamp = attr(b, 'created'))
}

nextBlock <- function(b){
  if(!inherits(b, "DataBlock")) return()
  if(attr(b, 'stream')==0) nextEeg(b) else nextClick(b)
}

lapply(blocks, nextBlock)

Q <- popQueue()
