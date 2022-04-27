library(metafor)

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
# dat$sei <- sqrt(dat$vi)

### fit a mixed-effects model with two moderators (absolute latitude and publication year)
xx <- rma(yi, vi, mods=cbind(ablat, year), data=dat, measure="RR")
preds <- predict(xx, newmods = cbind(0:60, 1970), transf = exp)
wi <- 1/sqrt(dat$vi)
size <- 0.5 + 3 * (wi - min(wi))/(max(wi) - min(wi))
plot(dat$ablat, exp(dat$yi), pch = 21, cex = size,
     xlab = "Absolute Latitude", ylab = "Relative Risk",
     las = 1, bty = "l", log = "y", xlim=c(0,60))
abline(h = 1, lty = "dotted")
lines(0:60, preds$pred)
lines(0:60, preds$ci.lb, lty = "dashed")
lines(0:60, preds$ci.ub, lty = "dashed")
