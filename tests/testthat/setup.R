newprob <- calc_det_prob(D_mb_ex, "Scotian Shelf")
Pscaled_month <- scale_prob_by_month(D_mb_ex, "Scotian Shelf", newprob$newP_agg)
