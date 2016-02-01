blocks <- Resonance:::blockLevelRead('d:/MEGA/EVG/eyelines/data/test-05/07.r2e')

#load('d:/Urii/Resonance-Rproj-yoba/DData')
require(Resonance)
require(bit64)

streams <- list()

for(h in 1:4){
  if(inherits( blocks[[h]], 'StreamDescription')){
    streams[ blocks[[h]]$name ] <- blocks[[h]]$id
  }
}


RAW_EEG_BLOCKS <- Filter(
  function(b){
    if(!inherits(b, "DataBlock")) return(F)
    if(attr(b, "stream")!=streams['EEG']) return(F)
    T
  },
  blocks)

RAW_FIX_BLOCKS <- Filter(
  function(b){
    if(!inherits(b, "DataBlock")) return(F)
    if(attr(b, "stream")!=streams['click']) return(F)
    T
  },
  blocks)

RAW_EEG_BLOCKS <- lapply(RAW_EEG_BLOCKS, function(x){ attr(x, "timestamp") <- attr(x, 'created'); x })
RAW_FIX_BLOCKS <- lapply(RAW_FIX_BLOCKS, function(x){ attr(x, "timestamp") <- attr(x, 'created'); x })

input1 <- source.dataBlocks(RAW_EEG_BLOCKS, "channels", channels=33, samplingRate=500)
input2 <- source.dataBlocks(RAW_FIX_BLOCKS, "message")


W <-
  c(0.0351977553721355, -0.0133378992250019, -0.0564032811195422, 
    -0.0620037855439785, -0.0694325470967451, -0.0129404226066531, 
    0.0271472372458826, 0.0402477885695595, 0.0216063780334808, 0.0849121378743835, 
    -0.0133630434478199, -0.027919373009705, -0.0470151029451429, 
    0.0627636700307195, 0.0230030307652385, 0.0229436500960828, -0.00160310924945226, 
    0.0230220808897092, 0.0286590465723958, -0.0411486529724925, 
    0.0194772854974848, 0.004020831882805, -0.0477676875873448, -0.0159394818943059, 
    0.000562907284410535, -0.0075837318873371, 0.0339303179748029, 
    -0.00183250994111677, 0.00910132474665004, -0.0291444133512135, 
    -0.0379356572920787, 0.0246069107663956, -0.0168252212829022, 
    -0.0956303819762801, 0.00917869268272246, 0.131629527458099, 
    -0.0202860064090163, 0.0703428693710693, -0.0199801778985829, 
    0.0347419575029294, -0.0627354409735489, 0.00123317422648313, 
    -0.0104970452329866, -0.0628269007469635, -0.0134867273489953, 
    0.062271093217163, -0.0401673977017644, 0.00994231613606207, 
    -0.0633916993014883, -0.0251464846800132, -0.00896097612041404, 
    -0.000848878745324495, -0.00179648642521119, -0.00796097603935707, 
    -0.00384477432574438, 0.00272608782437372, -0.00516174390789887, 
    -0.00258728279644592, -0.0114764354075964, -0.00719360784126077
  )
th <-
  -0.951263290727177
ufeats <-
  structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 
              2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 
              4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
              6, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
              10), .Dim = c(60L, 2L))
channels <-
  c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16, 17)
low <-
  1
high <-
  30
A1 <-
  11
A2 <-
  12
fixDur <-
  500
sRate <-
  500
t <-
  250
pipe.trof.classifier <-
  function(input, W, th, ufeats)
  {
    bp <- block.processor("message")
    
    input$connect(function(db){
      X <- rep(0, dim(ufeats)[[1]])
      for (i in 1:dim(ufeats)[[1]])
      {
        ts <- ufeats[i, 1]
        ch <- ufeats[i, 2]
        X[i] <- db[ts, ch]
      }
      Q = X %*% W
      
      if( T || Q < th){
        bp$emit(DataBlock(attr(db,'windowizationEvent'), db))
      }
      
      
    })
    
    bp
  }
RM <- diag(nrow=33)[channels,]
FS <- pipeline(
  input1,
  signalPreparation(, low=low, high=high, notch=50),
  pipe.spatial(, RM),
  pipe.references(, c(A1,A2))
)
p5 <-
  pipeline(FS
           ,pipe.decimate(, 1, 20 , coef_10000_to_500)
           ,cross.windowizeByEvents(, input2, t/20, -t/20)
           ,pipe.trof.classifier(, W, th, ufeats )
  )

p5$connect(function(data){
  cat("====> ", data, typeof(data), "\n")
})

pumpTogether(input1, input2)
