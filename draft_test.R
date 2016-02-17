
require(Resonance)
require(eyelinesOnline)
require(hybridEyeEEG)


W = c(-0.0663228945,-0.0427098774,0.0058597209,-0.0302715475,0.0889063848,0.1579459345,
      0.1629850989,-0.3223800968,0.0033425359,-0.0119923844,0.0698503035,
      -0.0092900247,0.0203553263,-0.0015004250,0.0400019265,-0.1534510324,
      0.0025231526,0.1017149258,-0.0738177491,0.0160926572,-0.0412207782,
      -0.0017815983,0.0028627214,-0.0367072145,-0.0707234100,-0.0177138198,
      -0.1762642502,0.1922943230,0.0027695796,0.0020734295,-0.0941617591,
      -0.0223116277,0.0619919794,0.0004420746,-0.0185049808,0.1078008015,
      0.0063200628,-0.0432141481,0.1037532851,0.0277384356,-0.0158571999,
      -0.0058407737,0.0201757776,0.0079140847,0.0018720494,0.0181232155,
      0.0213857267,-0.0183996406,0.0284015086,0.0106828039)

th = -0.9997981

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
bsln_end = 2



file = 'd:/YandexDisk/eyelinesOnline/data/test-3/06.r2e'
signal <- R3::extractChannel(file,0)
sync_marks <- which( diff(signal[,ncol(signal)])>0 )
signal <- signal[(sync_marks[3]+1):nrow(signal), ]

actions <- extract.actions('d:/YandexDisk/eyelinesOnline/data/test-3/24261882')

msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*1E6)
msgballChosen_t <- ceiling(actions[which(actions$Type=="msgballChosen"),1]*1E6)
msgBallMoved_t <- ceiling(actions[which(actions$Type=="msgBallMoved"),1]*1E6)
eventsT_t = c(msgbuttonPressed_t, msgballChosen_t, msgBallMoved_t)
msgev <- rep("GFY", length(eventsT_t))

# msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*1E6)
# msgev <- rep("GFY", length(msgbuttonPressed_t))

input1 <- source.channels(signal, samplingRate=500)
# input2 <- source.events(msgev, msgbuttonPressed_t)
input2 <- source.events(msgev, eventsT_t)

input <- function(x){
  if(x==1){ input1 }else{ input2 }
}

#eval(applyClassifier)


RM <- diag(nrow=33)[channels,]
FS <- pipeline(
  input(1),
  signalPreparation(, low=low, high=high, notch=50),
  pipe.spatial(, RM),
  pipe.references(, c(A1,A2))
)
#createOutput('niceEEG', FS)


RA1 <- pipe.decimate(FS, 1, 20 , coef_10000_to_500)
ev <- input(2)
RA2 <- cross.windowizeByEvents(RA1, ev, t/20, shift=-t/20)
RA3 <- pipe.medianWindow(RA2, 1, 12)
RA4 <- pipe.trof.classifier(RA3, W, th, ufeats )

number_of_clicks <- sum(sapply(RA4, function(x) is.null(x)))
print(number_of_clicks)
#createOutput(RES,'RES')