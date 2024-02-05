data("D_mb_ex")
newprob <- calc_det_prob(D_mb_ex)
scaledprobs <- scale_newprob(D_mb_ex, newprob)
