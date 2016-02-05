
require(Resonance)
require(R.matlab)
require(eyelinesOnline)
require(hybridEyeEEG)

W <-
  c(0.0529885015760123, -0.0303038658294656, 0.130144062030636, 
    -0.046543062182492, -0.061873339405163, -0.133033691528948, -0.0668262694466023, 
    0.15563718901255, -0.149780117019469, 0.424941826847294, 0.359877730365836, 
    0.240676795519499, 0.195832491730619, 0.188277733378972, 0.103053820169166, 
    0.214069370458148, 0.225921217918586, 0.137527835931826, 0.110483539482145, 
    -0.0869852591703793, -0.0981437131398338, 0.129364759673009, 
    -0.00421648394984042, 0.0148184367777002, 0.133941924721536, 
    0.241671596641089, 0.049429136520045, -0.0808120473607399, 0.042729237880128, 
    -0.263674027622109, -0.0496578141599844, 0.161955108879483, 0.140067684369396, 
    -0.00660260483687816, 0.136793291121678, 0.19940589342018, 0.0185707670197368, 
    0.0603053515841009, -0.0313431702892174, 0.10668874287846, 0.0179181652025508, 
    0.0468946806879992, 0.0488304404465852, 0.00902698605461979, 
    0.0319207663137009, 0.0459302332546252, 0.0154811281966146, 0.0306642870393552, 
    -0.00114871024320051, 0.0451199452046678)
th <-
  -1.02335153117645
ufeats <-
  structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 
              2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 
              4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10), .Dim = c(50L, 2L))
channels <-
  c(4L, 5L, 6L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 
    18L)
low <-
  FALSE
high <-
  5
A1 <-
  13
A2 <-
  14
fixDur <-
  500
sRate <-
  500
t <-
  250
bsln_start <-
  -100
bsln_end <-
  -50



file = '/home/mayenok/Yandex.Disk/EVG/eyelines/data/eff-02/eff-02-S1R04.r2e.raw.mat'
signal <- readMat(file)$raw
sync_marks <- which( diff(signal[,ncol(signal)])>0 )
signal <- signal[(sync_marks[3]+1):nrow(signal), ]

actions <- extract.actions('/home/mayenok/Yandex.Disk/EVG/eyelines/data/eff-02/23819717')
msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*1E6)
msgev <- rep(T, length(msgbuttonPressed_t))

input1 <- source.channels(signal, samplingRate=500)
input2 <- source.events(msgev, msgbuttonPressed_t)

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
RA2 <- cross.windowizeByEvents(RA1, input(2), t/20, shift=-t/20)
RA3 <- pipe.medianWindow(RA2, bsln_start/20, bsln_end/20)
RA4 <- pipe.trof.classifier(RA3, W, th, ufeats )

