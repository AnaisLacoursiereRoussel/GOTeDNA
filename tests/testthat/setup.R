newprob <- calc_det_prob(D_mb_ex, "Scotian Shelf")
Pscaled_month <- scale_prob_by_month(D_mb_ex, "Scotian Shelf", newprob$newP_agg)
Pscaled_year <- scale_prob_by_year(D_mb_ex, "Scotian Shelf", newprob$newP_yr)
