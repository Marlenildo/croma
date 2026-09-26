# Gera a logo do aplicativo (www/img/logo_app.png) a partir de cores CIELCH reais:
# anel de matiz (h° 0-360, L* 68, C* 48), eixos a*/b* e um ponto de amostra.
# Uso: Rscript scripts/gerar_logo_app.R

library(colorspace)

desenhar_logo <- function() {
  par(mar = c(0, 0, 0, 0), bg = "transparent")
  plot.new(); plot.window(c(-1, 1), c(-1, 1), asp = 1)
  passos <- 180
  angulos <- seq(0, 2 * pi, length.out = passos + 1)
  for (i in seq_len(passos)) {
    h <- (angulos[i] + angulos[i + 1]) / 2 * 180 / pi
    cor <- hex(polarLAB(L = 68, C = 48, H = h), fixup = TRUE)
    t <- seq(angulos[i], angulos[i + 1] + .01, length.out = 6)
    polygon(c(.98 * cos(t), rev(.64 * cos(t))), c(.98 * sin(t), rev(.64 * sin(t))), col = cor, border = cor, lwd = .6)
  }
  t <- seq(0, 2 * pi, length.out = 300)
  polygon(.6 * cos(t), .6 * sin(t), col = "#173B5B", border = NA)
  segments(c(-.44, 0), c(0, -.44), c(.44, 0), c(0, .44), col = adjustcolor("#FFFFFF", .35), lwd = 5)
  for (r in c(.2, .38)) lines(r * cos(t), r * sin(t), col = adjustcolor("#FFFFFF", .18), lwd = 3)
  ang <- 62 * pi / 180; r <- .3
  segments(0, 0, r * cos(ang), r * sin(ang), col = "#FFFFFF", lwd = 7)
  points(r * cos(ang), r * sin(ang), pch = 21, bg = hex(polarLAB(68, 48, 62)), col = "#FFFFFF", cex = 7.5, lwd = 6)
  points(0, 0, pch = 16, col = "#FFFFFF", cex = 2.6)
}

png("www/img/logo_app.png", width = 512, height = 512, bg = "transparent", type = "quartz")
desenhar_logo(); dev.off()
png("www/img/favicon.png", width = 64, height = 64, bg = "transparent", type = "quartz")
desenhar_logo(); dev.off()
