
png("fig/cosine_illu0.png", width = 4.5, height = 3, units = "in", res = 400)
par(mar = c(1,1,1,1), mgp = c(0,0,0))
plot(0, 0, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, 
     xlab = "Fairness", ylab = "Authority", type = "p", pch = 16, cex = .1)
lines(c(0, 0), c(0, 1), lwd = 3)
lines(c(0, 1), c(0, 0), lwd = 3)
dev.off()

png("fig/cosine_illu1.png", width = 4.5, height = 3, units = "in", res = 400)
par(mar = c(1,1,1,1), mgp = c(0,0,0))
plot(0, 0, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, 
     xlab = "Fairness", ylab = "Authority", type = "p", pch = 16, cex = .1)
lines(c(0, 0), c(0, 1), lwd = 3)
lines(c(0, 1), c(0, 0), lwd = 3)
arrows(y0 = 0, y1 = .2, x0 = 0, x1 = .8, code = 2, length = 0.1, lwd = 2)
text(.8, .25, "Opening Statement", cex = .5)
dev.off()

png("fig/cosine_illu2.png", width = 4.5, height = 3, units = "in", res = 400)
par(mar = c(1,1,1,1), mgp = c(0,0,0))
plot(0, 0, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, 
     xlab = "Fairness", ylab = "Authority", type = "p", pch = 16, cex = .1)
lines(c(0, 0), c(0, 1), lwd = 3)
lines(c(0, 1), c(0, 0), lwd = 3)
arrows(y0 = 0, y1 = .2, x0 = 0, x1 = .8, code = 2, length = 0.1, lwd = 2)
text(.8, .25, "Opening Statement", cex = .5)
arrows(y0 = 0, y1 = .9, x0 = 0, x1 = .15, code = 2, length = 0.1, lwd = 2, lty = "dotted", col = "darkred")
text(.15, .95, "Counterargument A", cex = .5, col = "darkred")
dev.off()

png("fig/cosine_illu3.png", width = 4.5, height = 3, units = "in", res = 400)
par(mar = c(1,1,1,1), mgp = c(0,0,0))
plot(0, 0, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, 
     xlab = "Fairness", ylab = "Authority", type = "p", pch = 16, cex = .1)
lines(c(0, 0), c(0, 1), lwd = 3)
lines(c(0, 1), c(0, 0), lwd = 3)
arrows(y0 = 0, y1 = .2, x0 = 0, x1 = .8, code = 2, length = 0.1, lwd = 2)
text(.8, .25, "Opening Statement", cex = .5)
arrows(y0 = 0, y1 = .9, x0 = 0, x1 = .15, code = 2, length = 0.1, lwd = 2, lty = "dotted", col = "darkred")
text(.15, .95, "Counterargument A", cex = .5, col = "darkred")
arrows(y0 = 0, y1 = .25, x0 = 0, x1 = .5, code = 2, length = 0.1, lwd = 2, lty = "dotted", col = "darkblue")
text(.5, .3, "Counterargument B", cex = .5, col = "darkblue")
dev.off()
