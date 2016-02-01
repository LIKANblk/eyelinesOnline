RAW_EEG_BLOCKS <- lapply(
  Filter(
    function(b){
      inherits(b, "DataBlock") && attr(b, "stream")==0
    } ,
    RAW_RECORD),
  function(b){
    class(b) <- "matrix"
    DataBlock(b, attr(b, "created")*fix)
  }
)

RAW_FIX_BLOCKS <- lapply(
  Filter(
    function(b){
      inherits(b, "DataBlock") && attr(b, "stream")==1
    } , 
    RAW_RECORD),
  function(b){
    DataBlock(T, attr(b, "created")*fix)
  }
)

ets <- unlist(lapply(RAW_EEG_BLOCKS, function(b) attr(b, "timestamp")))
oldClass(ets) <- "integer64"
fts <- unlist(lapply(RAW_FIX_BLOCKS, function(b) attr(b, "timestamp")))
oldClass(fts) <- "integer64"
