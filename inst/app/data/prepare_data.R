newprob <- calc_det_prob(
    data = D_mb_ex,
    ecodistrict.select = "Scotian Shelf"
)
saveRDS(newprob, "inst/app/data/newprob.rds")

Pscaled_month <- scale_prob_by_month(
    D_mb_ex, "Scotian Shelf",
    newprob$newP_agg
)
saveRDS(Pscaled_month, "inst/app/data/Pscaled_month.rds")
