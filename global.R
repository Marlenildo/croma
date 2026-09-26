# Funções e dependências compartilhadas pela aplicação

library(shiny)
library(DT)
library(colorspace)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Versão lida do DESCRIPTION (fonte única; atualize lá e no CHANGELOG.md).
VERSAO_APP <- tryCatch(unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1]), error = function(e) "dev")

CORES_APP <- list(
  navy = "#173B5B", blue = "#2A5C92", ink = "#263B4D", muted = "#627589",
  line = "#D9E3EB", canvas = "#F4F7FA", soft = "#EAF2FA", green = "#4D965D"
)

numero_seguro <- function(x, padrao = 0) {
  valor <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  if (length(valor) == 0 || is.na(valor) || !is.finite(valor)) padrao else valor
}

fmt <- function(x, digitos = 2) {
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}

fmt_dp <- function(x, dp, digitos = 2) paste(fmt(x, digitos), "±", fmt(dp, digitos))

# ------------------------------------------------------------
# Conversões de cor
# ------------------------------------------------------------

lab_para_lch <- function(a, b) {
  list(C = sqrt(a^2 + b^2), h = (atan2(b, a) * 180 / pi) %% 360)
}

lch_para_lab <- function(C, h) {
  rad <- h * pi / 180
  list(a = C * cos(rad), b = C * sin(rad))
}

lab_para_hex <- function(L, a, b) {
  if (any(is.na(c(L, a, b))) || any(!is.finite(c(L, a, b)))) return("#FFFFFF")
  tryCatch({
    resultado <- colorspace::hex(colorspace::LAB(L = L, A = a, B = b), fixup = TRUE)
    if (is.na(resultado)) "#FFFFFF" else resultado
  }, error = function(e) "#FFFFFF")
}

fora_gamut <- function(L, a, b) {
  tryCatch(is.na(colorspace::hex(colorspace::LAB(L = L, A = a, B = b), fixup = FALSE)),
           error = function(e) rep(TRUE, length(L)))
}

cor_texto <- function(hex) {
  rgb <- col2rgb(hex)
  luminancia <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(luminancia > 150, "#111111", "#FFFFFF")
}

# Nome aproximado da tonalidade a partir de h°, C* e L*.
nome_tom <- function(L, C, h) {
  vapply(seq_along(L), function(i) {
    if (C[i] < 8) {
      base <- if (L[i] > 85) "Branco" else if (L[i] < 20) "Preto" else "Cinza"
      return(if (C[i] >= 4) paste(base, "(leve tom)") else base)
    }
    faixas <- c(20, 50, 75, 105, 125, 165, 210, 250, 315, 345)
    nomes <- c("Rosa-avermelhado", "Vermelho", "Laranja", "Amarelo", "Amarelo-esverdeado",
               "Verde", "Verde-azulado", "Ciano", "Azul", "Púrpura", "Rosa-avermelhado")
    tom <- nomes[findInterval(h[i], faixas) + 1]
    if (L[i] < 35) paste(tom, "escuro") else if (L[i] > 75) paste(tom, "claro") else tom
  }, character(1))
}

# Padroniza a tabela de amostras: Id, Nome, Grupo, L, a, b, C, h, Hex, Fora, Tom (+ colunas extras).
completar_dados <- function(dados) {
  base <- c("Id", "Nome", "Grupo", "L", "a", "b", "C", "h", "Hex", "Fora", "Tom")
  if (is.null(dados) || !nrow(dados)) {
    return(data.frame(Id = integer(), Nome = character(), Grupo = character(), L = numeric(), a = numeric(),
                      b = numeric(), C = numeric(), h = numeric(), Hex = character(), Fora = logical(),
                      Tom = character(), stringsAsFactors = FALSE))
  }
  if (is.null(dados$Grupo)) dados$Grupo <- NA_character_
  dados$Id <- seq_len(nrow(dados))
  dados$Fora <- fora_gamut(dados$L, dados$a, dados$b)
  dados$Tom <- nome_tom(dados$L, dados$C, dados$h)
  dados[, c(base, setdiff(names(dados), base)), drop = FALSE]
}

# ------------------------------------------------------------
# Diferença de cor
# ------------------------------------------------------------

de00 <- function(lab, ref) {
  lab <- as.matrix(lab); ref <- matrix(as.numeric(ref), nrow = 1)
  if (requireNamespace("farver", quietly = TRUE)) {
    as.vector(farver::compare_colour(lab, ref, from_space = "lab", method = "cie2000"))
  } else sqrt(rowSums(sweep(lab, 2, ref)^2))
}

classificar_delta <- function(de) {
  classe <- as.character(cut(de, c(-Inf, 1, 2, 3.5, 5, Inf), right = FALSE,
    labels = c("Imperceptível", "Muito pequena", "Pequena", "Perceptível", "Grande")))
  classe[is.na(de)] <- "–"
  classe
}

tabela_delta <- function(dados, ref) {
  n <- nrow(dados)
  if (!n || is.na(ref) || ref < 1 || ref > n) return(NULL)
  dL <- dados$L - dados$L[ref]; da <- dados$a - dados$a[ref]; db <- dados$b - dados$b[ref]
  dC <- dados$C - dados$C[ref]
  de76 <- sqrt(dL^2 + da^2 + db^2)
  dh <- ((dados$h - dados$h[ref] + 180) %% 360) - 180
  dH <- sign(dh) * sqrt(pmax(0, de76^2 - dL^2 - dC^2))
  d00 <- de00(dados[, c("L", "a", "b")], unlist(dados[ref, c("L", "a", "b")]))
  classe <- classificar_delta(d00)
  classe[ref] <- "Referência"
  data.frame(Id = dados$Id, Nome = dados$Nome, Hex = dados$Hex, dL = dL, da = da, db = db, dC = dC,
             dH = dH, dE76 = de76, dE00 = d00, Classe = classe, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
# Agrupamento de repetições
# ------------------------------------------------------------

# Remove o número da repetição do final do nome:
# "Manga A 1", "Manga A-2", "Manga A_R3", "Manga A (4)", "Manga A rep 5" -> "Manga A"
nome_grupo_auto <- function(nome) {
  nome <- trimws(as.character(nome))
  g <- trimws(sub("(?i)[\\s_\\-\\.]*[\\(\\[]?\\s*(r|rep|repeti[cç][aã]o|repetition)?\\s*\\.?\\s*\\d+\\s*[\\)\\]]?$",
                  "", nome, perl = TRUE))
  ifelse(nzchar(g), g, nome)
}

chave_grupo <- function(dados, criterio = "auto") {
  nomes <- trimws(as.character(dados$Nome))
  if (identical(criterio, "nome")) return(nomes)
  grupo <- trimws(as.character(dados$Grupo %||% rep(NA_character_, nrow(dados))))
  ifelse(!is.na(grupo) & nzchar(grupo), grupo, nome_grupo_auto(nomes))
}

agrupar_dados <- function(dados, criterio = "auto", estatistica = "media") {
  ind <- completar_dados(dados)
  if (!nrow(ind)) return(list(grupos = ind, repeticoes = ind))
  chave <- chave_grupo(ind, criterio)
  niveis <- unique(chave)
  ind$GrupoId <- match(chave, niveis); ind$GrupoNome <- chave
  f <- if (identical(estatistica, "mediana")) stats::median else mean
  ind$dEcentro <- NA_real_; ind$Limite <- NA_real_
  linhas <- lapply(seq_along(niveis), function(g) {
    sel <- which(ind$GrupoId == g); m <- ind[sel, ]
    L <- f(m$L); a <- f(m$a); b <- f(m$b); lch <- lab_para_lch(a, b)
    dp <- function(x) if (length(x) > 1) stats::sd(x) else 0
    dist <- de00(m[, c("L", "a", "b")], c(L, a, b))
    ind$dEcentro[sel] <<- dist
    ind$Limite[sel] <<- max(1.5, 2.5 * stats::median(dist))
    data.frame(Nome = niveis[g], Grupo = niveis[g], L = L, a = a, b = b, C = lch$C, h = lch$h,
               Hex = lab_para_hex(L, a, b), n = nrow(m), dpL = dp(m$L), dpa = dp(m$a), dpb = dp(m$b),
               dpC = dp(m$C), dEintra = mean(dist), stringsAsFactors = FALSE)
  })
  grupos <- completar_dados(do.call(rbind, linhas))
  # Repetição possivelmente atípica: distância ao centro > 2,5x a mediana do grupo (e > 1,5).
  ind$Atipica <- ind$dEcentro > ind$Limite & grupos$n[ind$GrupoId] >= 3
  list(grupos = grupos, repeticoes = ind)
}

resumo_cores <- function(dados) {
  a <- mean(dados$a); b <- mean(dados$b); L <- mean(dados$L); lch <- lab_para_lch(a, b)
  list(L = L, a = a, b = b, C = lch$C, h = lch$h, Hex = lab_para_hex(L, a, b),
       L_min = min(dados$L), L_max = max(dados$L), L_dp = if (nrow(dados) > 1) sd(dados$L) else 0,
       C_media = mean(dados$C), C_min = min(dados$C), C_max = max(dados$C),
       fora = sum(dados$Fora), n = nrow(dados),
       n_rep = if (!is.null(dados$n)) sum(dados$n) else nrow(dados),
       dEintra = if (!is.null(dados$dEintra)) mean(dados$dEintra[dados$n > 1]) else NA_real_)
}

# ------------------------------------------------------------
# Gráficos (base R: usados no app e no PDF)
# ------------------------------------------------------------

fundo_ab <- function(lim, L_fundo, n = 150) {
  eixo <- seq(-lim, lim, length.out = n)
  grade <- expand.grid(a = eixo, b = rev(eixo))
  cores <- colorspace::hex(colorspace::LAB(L = L_fundo, A = grade$a, B = grade$b), fixup = FALSE)
  cores <- ifelse(is.na(cores), "#FFFFFF00", grDevices::adjustcolor(cores, alpha.f = .5))
  matrix(cores, nrow = n, byrow = TRUE)
}

par_grafico <- function(escala) {
  par(mar = c(3.6, 3.8, 1.8, 1), mgp = c(2.3, .6, 0), las = 1, tcl = -.25,
      cex.axis = .78 * escala, cex.lab = .85 * escala, col.axis = CORES_APP$muted,
      col.lab = CORES_APP$ink, fg = "#B9C9D7")
}

# Desenha repetições (pontos pequenos ligados ao centro do grupo) e as amostras/grupos.
desenhar_pontos <- function(x, y, dados, ref, escala, rep_x = NULL, rep_y = NULL, repeticoes = NULL) {
  if (!is.null(repeticoes) && nrow(repeticoes)) {
    g <- repeticoes$GrupoId
    segments(x[g], y[g], rep_x, rep_y, col = adjustcolor(dados$Hex[g], .9), lwd = 1.3)
    segments(x[g], y[g], rep_x, rep_y, col = adjustcolor("#263B4D", .3), lwd = .6)
    points(rep_x, rep_y, pch = 21, bg = repeticoes$Hex, col = adjustcolor("#263B4D", .7), cex = 1.05 * escala, lwd = .7)
    atip <- which(repeticoes$Atipica %in% TRUE)
    if (length(atip)) points(rep_x[atip], rep_y[atip], pch = 4, col = "#B94B4B", cex = 1.3 * escala, lwd = 1.6)
  }
  if (!is.na(ref) && ref <= nrow(dados)) points(x[ref], y[ref], pch = 1, cex = 3.6 * escala, lwd = 2, col = CORES_APP$ink)
  points(x, y, pch = 21, bg = dados$Hex, col = "#FFFFFF", cex = 2.5 * escala, lwd = 1.4)
  points(x, y, pch = 1, col = adjustcolor("#263B4D", .6), cex = 2.7 * escala, lwd = .8)
  text(x, y, dados$Id, cex = .6 * escala, font = 2, col = cor_texto(dados$Hex))
}

grafico_plano_ab <- function(dados, ref = NA, escala = 1, repeticoes = NULL) {
  valores <- c(dados$a, dados$b, repeticoes$a, repeticoes$b)
  lim <- max(60, ceiling(max(abs(valores), 0) * 1.15 / 20) * 20)
  L_fundo <- if (nrow(dados)) min(80, max(40, mean(dados$L))) else 60
  op <- par_grafico(escala); on.exit(par(op))
  plot(NA, xlim = c(-lim, lim), ylim = c(-lim, lim), asp = 1, xaxs = "i", yaxs = "i",
       xlab = "a*   (- verde  |  + vermelho)", ylab = "b*   (- azul  |  + amarelo)")
  rasterImage(fundo_ab(lim, L_fundo), -lim, -lim, lim, lim, interpolate = TRUE)
  t <- seq(0, 2 * pi, length.out = 200)
  for (r in seq(20, lim * 1.4, by = 20)) lines(r * cos(t), r * sin(t), col = adjustcolor("#263B4D", .18), lty = 3)
  for (ang in seq(0, 330, by = 30)) segments(0, 0, 2 * lim * cos(ang * pi / 180), 2 * lim * sin(ang * pi / 180), col = adjustcolor("#263B4D", .08))
  abline(h = 0, v = 0, col = adjustcolor("#263B4D", .45))
  for (ang in c(0, 90, 180, 270)) {
    r <- lim * .9
    text(r * cos(ang * pi / 180), r * sin(ang * pi / 180), paste0(ang, "°"),
         cex = .7 * escala, col = CORES_APP$ink, font = 2)
  }
  mtext(sprintf("Fundo: plano cromático em L* = %s  |  círculos a cada 20 unidades de C*", fmt(L_fundo, 0)),
        side = 3, adj = 0, line = .4, cex = .62 * escala, col = CORES_APP$muted)
  if (nrow(dados)) {
    segments(0, 0, dados$a, dados$b, col = adjustcolor("#263B4D", .25))
    desenhar_pontos(dados$a, dados$b, dados, ref, escala, repeticoes$a, repeticoes$b, repeticoes)
  }
  box(col = "#B9C9D7")
}

grafico_LC <- function(dados, ref = NA, escala = 1, repeticoes = NULL) {
  lim_c <- max(60, ceiling(max(c(dados$C, repeticoes$C), 0) * 1.15 / 10) * 10)
  op <- par_grafico(escala); on.exit(par(op))
  plot(NA, xlim = c(0, lim_c), ylim = c(0, 100), xaxs = "i", yaxs = "i",
       xlab = "C*   (cromaticidade / saturação)", ylab = "L*   (luminosidade)")
  faixas <- seq(0, 100, length.out = 51)
  cinzas <- grDevices::adjustcolor(vapply(faixas, function(L) lab_para_hex(L, 0, 0), character(1)), alpha.f = .22)
  rasterImage(matrix(rev(cinzas), ncol = 1), 0, 0, lim_c * .035, 100, interpolate = TRUE)
  abline(h = seq(10, 90, 10), v = seq(10, lim_c, 10), col = adjustcolor("#263B4D", .08))
  mtext("Barra à esquerda: escala de cinza (C* = 0) na mesma luminosidade",
        side = 3, adj = 0, line = .4, cex = .62 * escala, col = CORES_APP$muted)
  if (nrow(dados)) desenhar_pontos(dados$C, dados$L, dados, ref, escala, repeticoes$C, repeticoes$L, repeticoes)
  box(col = "#B9C9D7")
}

# ------------------------------------------------------------
# Relatório em PDF
# ------------------------------------------------------------

texto_pdf <- function(x) {
  x <- gsub("[−–—]", "-", as.character(x))
  y <- iconv(x, "UTF-8", "latin1//TRANSLIT", sub = "?")
  y[is.na(y)] <- x[is.na(y)]
  iconv(y, "latin1", "UTF-8")
}

encurtar <- function(x, n) {
  x <- texto_pdf(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "."), x)
}

ler_imagem <- function(caminho) {
  if (!file.exists(caminho) || !requireNamespace("png", quietly = TRUE)) return(NULL)
  tryCatch(png::readPNG(caminho), error = function(e) NULL)
}

# Tabela genérica: cada coluna tem x, adj, cab (texto ou expressão) e valor(i).
# Colunas com tipo = "cor" desenham um quadrado com a cor da linha.
desenhar_tabela_pdf <- function(idx, colunas, hex, destaque = integer(), y = .835, passo = .048) {
  rect(.045, y - .025, .955, y + .025, col = CORES_APP$navy, border = NA)
  for (col in colunas) {
    if (is.null(col$cab)) next
    rotulo <- if (is.character(col$cab)) texto_pdf(col$cab) else col$cab
    text(col$x, y, rotulo, adj = c(col$adj %||% 0, .5), cex = .66, font = 2, col = "#FFFFFF")
  }
  y <- y - .06
  for (k in seq_along(idx)) {
    i <- idx[k]
    rect(.045, y - .024, .955, y + .024, col = if (k %% 2) "#FFFFFF" else CORES_APP$canvas, border = NA)
    if (i %in% destaque) rect(.045, y - .024, .049, y + .024, col = CORES_APP$green, border = NA)
    segments(.045, y - .024, .955, y - .024, col = "#E7EEF4")
    for (col in colunas) {
      if (identical(col$tipo, "cor")) {
        rect(col$x, y - .017, col$x + .038, y + .017, col = hex[i], border = "#C9D6E0")
      } else {
        text(col$x, y, texto_pdf(col$valor(i)), adj = c(col$adj %||% 0, .5), cex = col$cex %||% .74,
             font = col$font %||% 1, col = if (is.function(col$cor)) col$cor(i) else col$cor %||% CORES_APP$ink,
             family = col$familia %||% "")
      }
    }
    y <- y - passo
  }
}

gerar_relatorio_pdf <- function(arquivo, titulo, descricao, dados, origem, responsavel = "",
                                referencia = 1, secoes = c("graficos", "paleta", "tabela", "delta", "repeticoes"),
                                repeticoes = NULL, estatistica = "media", rep_graficos = TRUE) {
  dados <- completar_dados(dados)
  if (!nrow(dados)) stop("Não há amostras para exportar no relatório.")
  agrupado <- !is.null(repeticoes) && nrow(repeticoes) > 0 && !is.null(dados$n)
  titulo <- trimws(titulo %||% "")
  if (!nzchar(titulo)) titulo <- "Relatório de cores CIELAB / CIELCH"
  descricao <- trimws(descricao %||% ""); responsavel <- trimws(responsavel %||% "")
  referencia <- suppressWarnings(as.integer(referencia))
  usar_delta <- "delta" %in% secoes && nrow(dados) > 1 && !is.na(referencia) && referencia <= nrow(dados)
  delta <- if (usar_delta) tabela_delta(dados, referencia) else NULL
  resumo <- resumo_cores(dados)
  nome_estat <- if (identical(estatistica, "mediana")) "mediana" else "média"
  unidade <- if (agrupado) "grupo" else "amostra"
  data_hora <- format(Sys.time(), "%d/%m/%Y às %H:%M")
  n <- nrow(dados)

  paginar <- function(ids, por) split(ids, ceiling(seq_along(ids) / por))
  paginas_tabela <- if ("tabela" %in% secoes) paginar(dados$Id, 13L) else list()
  paginas_paleta <- if ("paleta" %in% secoes) paginar(dados$Id, 18L) else list()
  paginas_rep <- if (agrupado && "repeticoes" %in% secoes) paginar(seq_len(nrow(repeticoes)), 13L) else list()
  total <- 1L + ("graficos" %in% secoes) + length(paginas_paleta) + length(paginas_tabela) + length(paginas_rep)
  pagina <- 0L

  logo_app <- ler_imagem("www/img/logo_app.png")
  logo_autor <- ler_imagem("www/img/logo_marlenildo.png")

  grDevices::pdf(arquivo, width = 11.69, height = 8.27, paper = "a4r", encoding = "ISOLatin1",
                 title = texto_pdf(titulo), family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)

  nova_pagina <- function(secao) {
    pagina <<- pagina + 1L
    par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), new = FALSE)
    plot.new(); plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
    # Cabeçalho: logo do aplicativo
    rect(0, .885, 1, 1, col = CORES_APP$navy, border = NA)
    rect(0, .881, 1, .885, col = CORES_APP$green, border = NA)
    if (!is.null(logo_app)) rasterImage(logo_app, .045, .903, .045 + .082 * 8.27 / 11.69, .985, interpolate = TRUE)
    text(.115, .958, encurtar(titulo, 72), adj = c(0, .5), cex = 1.35, font = 2, col = "#FFFFFF")
    sub <- if (agrupado) paste0("Croma · Visualizador CIELAB / CIELCH  |  ", n, " grupos (", resumo$n_rep, " repetições, ", nome_estat, ")")
           else paste("Croma · Visualizador CIELAB / CIELCH  |", n, if (n == 1) "amostra" else "amostras")
    text(.115, .923, texto_pdf(sub), adj = c(0, .5), cex = .75, col = "#B9D1E7")
    text(.955, .958, texto_pdf(toupper(secao)), adj = c(1, .5), cex = .72, font = 2, col = "#FFFFFF")
    text(.955, .923, texto_pdf(paste("Página", pagina, "de", total)), adj = c(1, .5), cex = .68, col = "#B9D1E7")
    # Rodapé: logo do autor
    segments(.045, .06, .955, .06, col = CORES_APP$line)
    text(.045, .034, "Desenvolvido por", adj = c(0, .5), cex = .62, col = "#587086")
    if (!is.null(logo_autor)) {
      alt <- .052; larg <- alt * (ncol(logo_autor) / nrow(logo_autor)) * 8.27 / 11.69
      rasterImage(logo_autor, .11, .033 - alt / 2, .11 + larg, .033 + alt / 2, interpolate = TRUE)
    } else text(.105, .034, "Marlenildo", adj = c(0, .5), cex = .66, font = 2, col = "#587086")
    text(.5, .034, texto_pdf("Cores em sRGB (D65, observador 2°). Valores fora do gamut foram ajustados para exibição."),
         adj = c(.5, .5), cex = .56, col = "#8A9AAA")
    text(.955, .034, texto_pdf(paste0("Gerado em ", data_hora, "  |  Croma v", VERSAO_APP)), adj = c(1, .5), cex = .64, col = "#587086")
  }

  rotulo <- function(x, y, txt) text(x, y, texto_pdf(txt), adj = c(0, .5), cex = .66, font = 2, col = CORES_APP$blue)
  nota <- function(txt) text(.045, .08, texto_pdf(txt), adj = c(0, .5), cex = .56, col = CORES_APP$muted, font = 3)
  cores_rep <- function(g) if (agrupado) repeticoes$Hex[repeticoes$GrupoId == g] else character()

  # ---------------- Página 1: resumo ----------------
  nova_pagina("Resumo")
  rotulo(.045, .835, "IDENTIFICAÇÃO")
  info <- list(c("Origem dos dados", origem))
  info <- c(info, if (agrupado) list(c("Agrupamento", paste0(n, " grupos · ", resumo$n_rep, " repetições · valor do grupo = ", nome_estat)))
                  else list(c("Número de amostras", n)))
  if (nzchar(responsavel)) info <- c(info, list(c("Responsável", responsavel)))
  if (usar_delta) info <- c(info, list(c(paste(if (agrupado) "Grupo" else "Amostra", "de referência"), paste0("#", referencia, " ", dados$Nome[referencia]))))
  info <- c(info, list(c("Emissão", data_hora)))
  y <- .795
  for (item in info) {
    text(.045, y, texto_pdf(item[1]), adj = c(0, .5), cex = .78, col = CORES_APP$muted)
    text(.2, y, encurtar(item[2], 58), adj = c(0, .5), cex = .82, font = 2, col = CORES_APP$ink)
    segments(.045, y - .02, .52, y - .02, col = "#EEF3F7")
    y <- y - .045
  }
  y <- y - .02
  rotulo(.045, y, "DESCRIÇÃO DO EXPERIMENTO / OBSERVAÇÕES")
  linhas <- if (nzchar(descricao)) unlist(lapply(strsplit(descricao, "\n")[[1]], function(p) if (nzchar(trimws(p))) strwrap(p, 88) else "")) else "Nenhuma descrição informada."
  max_linhas <- max(1, floor((y - .33) / .03))
  if (length(linhas) > max_linhas) linhas <- c(linhas[seq_len(max_linhas - 1)], paste0(linhas[max_linhas], " [...]"))
  for (linha in linhas) {
    y <- y - .03
    text(.045, y, texto_pdf(linha), adj = c(0, .5), cex = .8, col = if (nzchar(descricao)) "#3E5467" else "#9AAAB8", font = if (nzchar(descricao)) 1 else 3)
  }

  rotulo(.57, .835, if (agrupado) "COR MÉDIA DOS GRUPOS" else "COR MÉDIA DAS AMOSTRAS")
  rect(.57, .58, .74, .81, col = resumo$Hex, border = "#C9D6E0")
  text(.655, .6, resumo$Hex, cex = .8, font = 2, col = cor_texto(resumo$Hex))
  valores_media <- c(paste("L*", fmt(resumo$L)), paste("a*", fmt(resumo$a)), paste("b*", fmt(resumo$b)),
                     paste("C*", fmt(resumo$C)), paste("h°", fmt(resumo$h)))
  for (i in seq_along(valores_media)) text(.76, .8 - (i - 1) * .044, texto_pdf(valores_media[i]), adj = c(0, .5), cex = .85, font = 2, col = CORES_APP$ink)
  text(.76, .8 - 5 * .044 + .005, encurtar(nome_tom(resumo$L, resumo$C, resumo$h), 26), adj = c(0, .5), cex = .72, col = CORES_APP$muted, font = 3)

  kpis <- list(
    c(paste("L* médio ± dp entre", if (agrupado) "grupos" else "amostras"), fmt_dp(resumo$L, resumo$L_dp), paste("faixa", fmt(resumo$L_min, 1), "a", fmt(resumo$L_max, 1))),
    c("C* médio", fmt(resumo$C_media), paste("faixa", fmt(resumo$C_min, 1), "a", fmt(resumo$C_max, 1))),
    c("h° da cor média", paste0(fmt(resumo$h, 1), "°"), nome_tom(resumo$L, resumo$C, resumo$h)),
    if (agrupado && !is.na(resumo$dEintra)) c("Homogeneidade das repetições", paste("dE00", fmt(resumo$dEintra)), "distância média ao centro do grupo")
    else c("Fora do gamut sRGB", resumo$fora, if (resumo$fora) "cores ajustadas" else "todas exibidas fielmente")
  )
  for (i in seq_along(kpis)) {
    cx <- .57 + ((i - 1) %% 2) * .197; cy <- .5 - ((i - 1) %/% 2) * .115
    rect(cx, cy - .095, cx + .188, cy, col = CORES_APP$canvas, border = CORES_APP$line)
    rect(cx, cy - .095, cx + .004, cy, col = CORES_APP$blue, border = NA)
    text(cx + .014, cy - .02, encurtar(kpis[[i]][1], 38), adj = c(0, .5), cex = .62, col = CORES_APP$muted)
    text(cx + .014, cy - .05, texto_pdf(kpis[[i]][2]), adj = c(0, .5), cex = 1.15, font = 2, col = CORES_APP$navy)
    text(cx + .014, cy - .078, encurtar(kpis[[i]][3], 36), adj = c(0, .5), cex = .6, col = CORES_APP$muted)
  }

  rotulo(.045, .265, if (agrupado) "ESPECTRO DOS GRUPOS (faixa inferior: cada repetição)" else "ESPECTRO DAS AMOSTRAS (na ordem de entrada)")
  largura <- .91 / n
  for (i in seq_len(n)) {
    x0 <- .045 + (i - 1) * largura
    rect(x0, .13, x0 + largura, .235, col = dados$Hex[i], border = NA)
    reps <- cores_rep(i)
    if (length(reps)) {
      lr <- largura / length(reps)
      for (k in seq_along(reps)) rect(x0 + (k - 1) * lr, .13, x0 + k * lr, .158, col = reps[k], border = NA)
    }
    if (n <= 60) rect(x0, .13, x0 + largura, .235, border = "#FFFFFF", lwd = 1.2)
    if (n <= 40) text(x0 + largura / 2, .114, dados$Id[i], cex = .55, col = CORES_APP$muted)
  }
  rect(.045, .13, .955, .235, border = CORES_APP$line)

  # ---------------- Gráficos ----------------
  if ("graficos" %in% secoes) {
    nova_pagina("Gráficos")
    ref_graf <- if (usar_delta) referencia else NA
    reps_graf <- if (agrupado && rep_graficos) repeticoes else NULL
    rotulo(.045, .845, "PLANO CROMÁTICO a* x b*")
    text(.045, .818, "Distância ao centro = C*; ângulo = h°", adj = c(0, .5), cex = .62, col = CORES_APP$muted)
    rotulo(.43, .845, "LUMINOSIDADE x CROMATICIDADE (L* x C*)")
    text(.43, .818, texto_pdf("Posição entre escura/clara e neutra/saturada"), adj = c(0, .5), cex = .62, col = CORES_APP$muted)
    rotulo(.785, .845, "LEGENDA")
    legenda_max <- 22
    y <- .81
    for (i in head(seq_len(n), legenda_max)) {
      rect(.785, y - .011, .805, y + .011, col = dados$Hex[i], border = "#C9D6E0")
      text(.795, y, dados$Id[i], cex = .48, font = 2, col = cor_texto(dados$Hex[i]))
      rot <- if (agrupado) paste0(encurtar(dados$Nome[i], 20), " (n=", dados$n[i], ")") else encurtar(dados$Nome[i], 26)
      text(.812, y, texto_pdf(rot), adj = c(0, .5), cex = .66, col = CORES_APP$ink)
      y <- y - .029
    }
    if (n > legenda_max) text(.785, y, texto_pdf(paste("... e mais", n - legenda_max, "(ver tabela)")), adj = c(0, .5), cex = .62, col = CORES_APP$muted, font = 3)
    obs <- c(if (usar_delta) paste0("Anel escuro: referência #", referencia),
             if (!is.null(reps_graf)) "Pontos pequenos: repetições",
             if (!is.null(reps_graf) && any(repeticoes$Atipica)) "x vermelho: repetição atípica")
    for (k in seq_along(obs)) text(.785, .135 - (k - 1) * .022, texto_pdf(obs[k]), adj = c(0, .5), cex = .58, col = CORES_APP$muted)
    par(fig = c(.035, .415, .085, .8), new = TRUE)
    grafico_plano_ab(dados, ref_graf, escala = .95, repeticoes = reps_graf)
    par(fig = c(.42, .77, .085, .8), new = TRUE)
    grafico_LC(dados, ref_graf, escala = .95, repeticoes = reps_graf)
  }

  # ---------------- Paleta ----------------
  for (idx in paginas_paleta) {
    nova_pagina("Paleta de cores")
    ncol <- 6; gap <- .012; w <- (.91 - (ncol - 1) * gap) / ncol; h <- .245
    for (k in seq_along(idx)) {
      i <- idx[k]
      cx <- .045 + ((k - 1) %% ncol) * (w + gap); topo <- .855 - ((k - 1) %/% ncol) * (h + .018)
      base <- topo - h * .48
      rect(cx, topo - h, cx + w, topo, col = "#FFFFFF", border = CORES_APP$line)
      rect(cx, base, cx + w, topo, col = dados$Hex[i], border = NA)
      reps <- cores_rep(i)
      if (length(reps)) {
        lr <- w / length(reps)
        for (r in seq_along(reps)) rect(cx + (r - 1) * lr, base, cx + r * lr, base + .02, col = reps[r], border = "#FFFFFF", lwd = .6)
      }
      rect(cx + .006, topo - .032, cx + .032, topo - .008, col = adjustcolor("#FFFFFF", .85), border = NA)
      text(cx + .019, topo - .02, dados$Id[i], cex = .62, font = 2, col = CORES_APP$navy)
      text(cx + w - .006, base + (if (length(reps)) .034 else .016), dados$Hex[i], adj = c(1, .5), cex = .66, font = 2, col = cor_texto(dados$Hex[i]))
      if (dados$Fora[i]) text(cx + w - .006, topo - .02, "ajustada", adj = c(1, .5), cex = .52, font = 3, col = cor_texto(dados$Hex[i]))
      text(cx + .008, base - .022, encurtar(dados$Nome[i], 22), adj = c(0, .5), cex = .74, font = 2, col = CORES_APP$ink)
      if (agrupado) {
        text(cx + .008, base - .042, encurtar(paste0("n = ", dados$n[i], " · ", dados$Tom[i]), 30), adj = c(0, .5), cex = .56, font = 3, col = CORES_APP$muted)
        linhas_val <- c(paste0("L* ", fmt_dp(dados$L[i], dados$dpL[i], 1), "   a* ", fmt_dp(dados$a[i], dados$dpa[i], 1)),
                        paste0("b* ", fmt_dp(dados$b[i], dados$dpb[i], 1), "   C* ", fmt_dp(dados$C[i], dados$dpC[i], 1)),
                        paste0("h° ", fmt(dados$h[i], 1), "°   dE00 intra ", fmt(dados$dEintra[i])))
        cex_val <- .53
      } else {
        text(cx + .008, base - .042, encurtar(dados$Tom[i], 28), adj = c(0, .5), cex = .58, font = 3, col = CORES_APP$muted)
        linhas_val <- c(paste0("L* ", fmt(dados$L[i]), "   a* ", fmt(dados$a[i])),
                        paste0("b* ", fmt(dados$b[i]), "   C* ", fmt(dados$C[i])),
                        paste0("h° ", fmt(dados$h[i]), "°"))
        cex_val <- .6
      }
      for (l in seq_along(linhas_val)) text(cx + .008, base - .065 - (l - 1) * .02, texto_pdf(linhas_val[l]), adj = c(0, .5), cex = cex_val, col = CORES_APP$ink)
    }
    if (agrupado) nota(paste0("Valores do grupo = ", nome_estat, " de L*, a* e b* das repetições (C* e h° calculados a partir de a* e b*); ± desvio padrão. Faixa inferior do quadrado: cor de cada repetição."))
  }

  # ---------------- Tabela principal ----------------
  num <- function(v) function(i) fmt(v[i])
  cor_classe <- function(i) if (usar_delta && i == referencia) CORES_APP$green else CORES_APP$muted
  for (idx in paginas_tabela) {
    if (agrupado) {
      nova_pagina(if (usar_delta) "Grupos e diferença de cor" else "Tabela de grupos")
      colunas <- list(
        list(x = .055, cab = "#", valor = function(i) dados$Id[i], font = 2, cor = CORES_APP$muted),
        list(x = .075, tipo = "cor", cab = "COR"),
        list(x = .125, cab = "GRUPO", valor = function(i) encurtar(dados$Nome[i], 24), font = 2),
        list(x = .3, adj = 1, cab = "n", valor = function(i) dados$n[i]),
        list(x = .39, adj = 1, cab = "L* ± dp", valor = function(i) fmt_dp(dados$L[i], dados$dpL[i]), cex = .7),
        list(x = .48, adj = 1, cab = "a* ± dp", valor = function(i) fmt_dp(dados$a[i], dados$dpa[i]), cex = .7),
        list(x = .57, adj = 1, cab = "b* ± dp", valor = function(i) fmt_dp(dados$b[i], dados$dpb[i]), cex = .7),
        list(x = .66, adj = 1, cab = "C* ± dp", valor = function(i) fmt_dp(dados$C[i], dados$dpC[i]), cex = .7),
        list(x = .72, adj = 1, cab = "h°", valor = num(dados$h), cex = .7),
        list(x = .795, adj = 1, cab = expression(bold(Delta * E[0 * 0] ~ intra)), valor = num(dados$dEintra), cex = .7, cor = CORES_APP$muted)
      )
    } else {
      nova_pagina(if (usar_delta) "Coordenadas e diferença de cor" else "Tabela de coordenadas")
      colunas <- list(
        list(x = .055, cab = "#", valor = function(i) dados$Id[i], font = 2, cor = CORES_APP$muted),
        list(x = .075, tipo = "cor", cab = "COR  /  HEX"),
        list(x = .12, valor = function(i) dados$Hex[i], cex = .64, familia = "Courier"),
        list(x = .2, cab = "AMOSTRA", valor = function(i) encurtar(dados$Nome[i], 30), font = 2),
        list(x = .45, adj = 1, cab = "L*", valor = num(dados$L)),
        list(x = .515, adj = 1, cab = "a*", valor = num(dados$a)),
        list(x = .58, adj = 1, cab = "b*", valor = num(dados$b)),
        list(x = .645, adj = 1, cab = "C*", valor = num(dados$C)),
        list(x = .71, adj = 1, cab = "h°", valor = num(dados$h)),
        list(x = .73, cab = "TONALIDADE", valor = function(i) encurtar(paste0(dados$Tom[i], if (dados$Fora[i]) " *" else ""), 22), cex = .66, cor = CORES_APP$muted)
      )
    }
    if (usar_delta) colunas <- c(colunas, list(
      list(x = .868, adj = 1, cab = expression(bold(Delta * E[0 * 0])), font = 2, valor = function(i) if (i == referencia) "-" else fmt(delta$dE00[i])),
      list(x = .878, cab = "DIFERENÇA", valor = function(i) delta$Classe[i], cex = .64, cor = cor_classe)
    ))
    desenhar_tabela_pdf(idx, colunas, dados$Hex, destaque = if (usar_delta) referencia else integer())
    notas <- if (agrupado) paste0("Valores = ", nome_estat, " das repetições ± desvio padrão. dE00 intra = distância média (CIEDE2000) das repetições ao centro do grupo.")
             else "* cor fora do gamut sRGB, ajustada para exibição."
    if (usar_delta) notas <- paste(notas, "dE00 vs. referência: <1 imperceptível; 1-2 muito pequena; 2-3,5 pequena; 3,5-5 perceptível; >5 grande.")
    nota(notas)
  }

  # ---------------- Repetições individuais ----------------
  for (idx in paginas_rep) {
    nova_pagina("Repetições (dados individuais)")
    r <- repeticoes
    colunas <- list(
      list(x = .055, cab = "#", valor = function(i) i, font = 2, cor = CORES_APP$muted),
      list(x = .075, tipo = "cor", cab = "COR  /  HEX"),
      list(x = .12, valor = function(i) r$Hex[i], cex = .64, familia = "Courier"),
      list(x = .2, cab = "AMOSTRA", valor = function(i) encurtar(r$Nome[i], 24), font = 2),
      list(x = .36, cab = "GRUPO", valor = function(i) paste0(r$GrupoId[i], " · ", encurtar(r$GrupoNome[i], 18)), cex = .68, cor = CORES_APP$muted),
      list(x = .56, adj = 1, cab = "L*", valor = num(r$L)),
      list(x = .625, adj = 1, cab = "a*", valor = num(r$a)),
      list(x = .69, adj = 1, cab = "b*", valor = num(r$b)),
      list(x = .755, adj = 1, cab = "C*", valor = num(r$C)),
      list(x = .82, adj = 1, cab = "h°", valor = num(r$h)),
      list(x = .885, adj = 1, cab = expression(bold(Delta * E[0 * 0] ~ centro)), valor = num(r$dEcentro), font = 2),
      list(x = .895, valor = function(i) if (isTRUE(r$Atipica[i])) "atípica" else "", cex = .6, font = 3, cor = "#B94B4B")
    )
    desenhar_tabela_pdf(idx, colunas, r$Hex)
    nota(paste0("dE00 centro = diferença CIEDE2000 entre a repetição e a ", nome_estat, " do seu grupo. 'atípica' = mais de 2,5 vezes a distância mediana do grupo (e > 1,5): verifique a leitura."))
  }
  invisible(arquivo)
}
