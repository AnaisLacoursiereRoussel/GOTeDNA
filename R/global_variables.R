# remove NOTE about no visible binding for global variable during R CMD check --
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("CLASS", "DATA_QPCR_FILES", "DATE", "DETECTED", "ECOREGION", "EVENT_DATE",
      "EVENT_ID", "FAMILY", "FREQ_DETECTION", "GENUS", "IQR", "KINGDOM", "MEAN",
      "METADATA_QPCR_FILES", "MONTH", "OCCURRENCE_STATUS", "ORDER", "PHYLUM",
      "PROJECT_ID", "Q25", "Q75", "SAMPLING_EFFORT", "SAMPLING_SITE",
      "SCIENTIFIC_NAME", "SPP", "YEAR", "eventDate", "where", ".", "SITE",
      "SAMPLE_ID"
    )
  )
}
