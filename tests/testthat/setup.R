data("D_mb_ex")
newprob <- calc_det_prob(D_mb_ex, "Scotian Shelf")
scaledprobs <- scale_newprob(D_mb_ex, "Scotian Shelf", newprob)
