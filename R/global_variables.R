# remove NOTE about no visible binding for global variable during R CMD check --
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".", "GOTeDNA_ID", "concentration", "controlType", "detected", "ecodistrict", "eventID",
      "id", "kingdom", "month", "newP_agg", "newP_yr","scientificName", "target_gene",
      "target_subfragment", "year", "Pscaled_agg", "fill", "scaleP", "x", "y"
    )
  )
}
