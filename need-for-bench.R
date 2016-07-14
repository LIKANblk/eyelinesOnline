library(R3)

file = 'd:/YandexDisk/eyelinesOnline/data/test-online/02.r2e'
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

clickResultBlocks <- Filter(function(b){
  if(!inherits(b, "DataBlock")) return(F)
  if(attr(b, 'stream')!=2) return(F)
  TRUE
}, blocks)

Csi <- SI.channels(33, 500)
Esi <- SI.event()

CB <- lapply(eegBlocks, function(x){
  DB.channels(Csi, attr(x, 'created'), t(x))
})
EB <- lapply(clickBlocks, function(x){
  DB.event(Esi, attr(x, 'created'), x)
})


blocks <- c(CB, EB)

TS <- sapply(blocks, function(x){ if(is.list(x)) attr(x[[1]],'TS') else attr(x, 'TS')[[1]] })

blocks <- blocks[order(TS)]


code <- '
library(Resonance)
library(Resonate)
library(eyelinesOnline)

res <-
structure(list(W = c(-0.0145912580010955, -0.0218431115862439, 
0.00811860102061057, 0.00296249347413369, -0.0160761470166285, 
-0.012080373764671, 0.00779016631327707, -0.0118930295346983, 
-0.0468298223573775, -0.0316396592899351, -0.00392468080881925, 
-0.0173475016355338, 0.0134384433755938, 0.0275604469387581, 
0.0205333024909606, 0.0084753383881487, -0.0278870411659109, 
-0.025098459053856, 0.0403762151218967, 0.0455423229735104, -0.00406593916245558, 
-0.0360135147226669, -0.0145510825218674, -0.00249432786920403, 
0.0167350287036172, 0.00819255422799398, 0.0178927784561407, 
-0.00120214629463942, -0.00651913912513807, 0.0138468940180663, 
0.033391353174522, -0.000249220895542099, -0.004935815502425, 
-0.00628477400011414, 0.0364848169882675, 0.0296534360439206, 
-0.0246370969684378, -0.0560572786915447, -0.0219670991187337, 
-0.0125324651217246, -0.0343587103622223, -0.0175380750299552, 
-0.00636548573590714, -0.0155463079244046, -0.0126517236681903, 
-0.0126077732539475, -0.0542273652075579, -0.035503826488368, 
-0.0219593370443067, -0.0415280218674936, 0.00603464320747684, 
0.0337353826673536, 0.0250506210917132, 0.00720948692091724, 
-0.00578647770812884, -0.0401232992234501, -0.0130489031752702, 
0.00359398438769488, 0.0320784638765312, 0.0286583200659954, 
0.0253621999590477, -0.00725390154544452, -0.0291851255846834, 
0.00936581591769781, 0.0322186241510076, -0.0326327036420045, 
-0.0250441268889624, 0.00122772745141489, 0.013101178711455, 
0.0119594565597651, 0.00806590854542885, 0.0191380514574297, 
0.0628112520072373, -0.00545358608717081, -0.046924193486049, 
-0.0571595807436326, -0.0323157495043292, 0.0193295301664884, 
0.0105670415828354, 0.0184469638567265, 0.0387272878098484, 0.0327975940769429, 
0.0142725127636561, 0.0104656673928623, 0.0183632209101947, 0.033567690634317, 
0.00633105205217056, 0.02620235395285, 0.020149095350451, -0.0431380025072455, 
-0.0449579199567068, -0.0396253857458479, -0.0225909356138415, 
0.00833558840779396, -0.0194451758206338, -0.0240925816893648, 
0.0762028349282925, 0.0347257819906443, 0.00449263014017374, 
-0.00873600542126557, -0.00206785358052475, 0.026369849814055, 
0.0428629531657052, 0.0640787991168536), th = -1.39393381531449, 
channels = c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15), 
low = FALSE, high = 30, A1 = 16, A2 = 17, sRate = 500, bsln_start = 200, 
bsln_end = 300, times_seq = c(300, 320, 340, 360, 380, 400, 
420, 440), decimation_window = 50), .Names = c("W", "th", 
"channels", "low", "high", "A1", "A2", "sRate", "bsln_start", 
"bsln_end", "times_seq", "decimation_window"))



online_epoch_start <- min(res$bsln_start, res$times_seq[1])
online_epoch_end <- max(res$bsln_end, (res$times_seq + res$decimation_window))
  
refs <- c(res$A1, res$A2)

online_epoch_size <- online_epoch_end - online_epoch_start
online_epoch_shift <- online_epoch_start - 500

times_seq <- res$times_seq - online_epoch_start

bsln_start <- res$bsln_start - online_epoch_start
bsln_end <- res$bsln_end - online_epoch_start

process = function(){

FS <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=refs, channels=res$channels)
ev <- input(2)
RA2 <- cross.windowizeByEvents(FS, ev, online_epoch_size/1000*SI(FS)$samplingRate, shift=online_epoch_shift/1000*SI(FS)$samplingRate)
createOutput(RA2, "tmp")
RA3 <- pipe.medianWindow(RA2, (bsln_start)/1000* SI(RA2)$samplingRate, (bsln_end)/1000* SI(RA2)$samplingRate)
RA4 <- pipe.trof.classifier2(RA3, res$W, res$th, times_seq/1000, res$decimation_window/1000)
createOutput(RA4,"RES")

}
'

#system.time({A <<- run.online(list(Csi, Esi), blocks, code)})
B <- run.offline(list(Csi, Esi), blocks, code)

#all.equal(A$RES,B$RES, check.attributes = F)

p <- 1
compl <- c()

#for(i in clickBlocks){
#  if(i==clickResultBlocks[[p]]){
#    compl <<- c(compl, attr(i, 'TS'))
#    p <<- p+1
#  }
#}



