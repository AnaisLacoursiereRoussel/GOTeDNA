# remove NOTE about no visible binding for global variable during R CMD check --
utils::globalVariables(
  c(
    ".", "protocol_ID", "concentration", "controlType", "detected",
    "eventID", "id", "kingdom", "phylum", "class", "order",
    "family", "genus", "species", "month", "newP_agg", "newP_yr",
    "year","Pscaled_agg", "Pscaled_yr", "fill", "scaleP", "x", "y", "P", "PRED",
    "freq_det", "n", "nd", "primer", "prob", "station", "detect", "diff_y1",
    "nondetect", "id.yr", "p", "prev_", "next_", "success", "total", "perc",
    "protocolVersion", "protocol.v", "materialSampleID", "unit", "decimalLatitude",
    "decimalLongitude"
  )
)
