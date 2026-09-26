server <- function(input, output, session) {
  # Estado exclusivo da aba "Editar dados".
  modo_atual <- reactiveVal("LAB")
  ids_amostras <- reactiveVal(1L)
  proximo_id <- reactiveVal(2L)
  dados_amostras <- reactiveVal(list("1" = list(nome = "Amostra 1", L = 50, x = 0, y = 0)))
  linha_pendente_exclusao <- reactiveVal(NULL)

  # Estado exclusivo da aba "Importar do Excel".
  dados_importados <- reactiveVal(data.frame(Nome = character(), Grupo = character(), L = numeric(), a = numeric(), b = numeric(), C = numeric(), h = numeric(), Hex = character()))

  ler_linha <- function(id, modo) {
    padrao <- dados_amostras()[[as.character(id)]] %||% list(nome = paste("Amostra", id), L = 50, x = 0, y = 0)
    list(
      nome = input[[paste0("nome_", id)]] %||% padrao$nome,
      L = min(100, max(0, numero_seguro(input[[paste0("L_", id)]], padrao$L))),
      x = numero_seguro(input[[paste0(if (modo == "LAB") "a_" else "C_", id)]], padrao$x),
      y = numero_seguro(input[[paste0(if (modo == "LAB") "b_" else "H_", id)]], padrao$y)
    )
  }

  salvar_valores <- function(modo = modo_atual()) {
    dados <- dados_amostras()
    for (id in ids_amostras()) dados[[as.character(id)]] <- ler_linha(id, modo)
    dados_amostras(dados)
  }

  obter_lab <- function(id) {
    linha <- ler_linha(id, modo_atual())
    if (modo_atual() == "LAB") return(list(nome = linha$nome, L = linha$L, a = linha$x, b = linha$y))
    convertido <- lch_para_lab(max(0, linha$x), linha$y %% 360)
    list(nome = linha$nome, L = linha$L, a = convertido$a, b = convertido$b)
  }

  criar_linha_ui <- function(id, dados, modo) {
    campos <- if (modo == "LAB") {
      tagList(
        column(2, numericInput(paste0("a_", id), NULL, round(dados$x, 2), step = .1)),
        column(2, numericInput(paste0("b_", id), NULL, round(dados$y, 2), step = .1))
      )
    } else {
      tagList(
        column(2, numericInput(paste0("C_", id), NULL, round(max(0, dados$x), 2), min = 0, step = .1)),
        column(2, numericInput(paste0("H_", id), NULL, round(dados$y %% 360, 2), min = 0, max = 360, step = .1))
      )
    }
    div(id = paste0("linha_", id), class = "linha-amostra", fluidRow(
      column(3, textInput(paste0("nome_", id), NULL, dados$nome, placeholder = "Nome da amostra")),
      column(2, numericInput(paste0("L_", id), NULL, round(dados$L, 2), min = 0, max = 100, step = .1)),
      campos,
      column(2, uiOutput(paste0("cor_", id))),
      column(1, actionButton(paste0("remover_", id), label = NULL, icon = icon("trash"), class = "btn btn-remover", title = "Apagar esta amostra"))
    ))
  }

  registrar_amostra <- function(id) {
    force(id)
    output[[paste0("cor_", id)]] <- renderUI({
      lab <- obter_lab(id)
      hex <- lab_para_hex(lab$L, lab$a, lab$b)
      tagList(
        div(class = "cor-amostra", style = paste0("background-color:", hex, ";color:", cor_texto(hex), ";"), hex),
        if (fora_gamut(lab$L, lab$a, lab$b)) div(class = "gamut-aviso", "ajustada para sRGB")
      )
    })
    observeEvent(input[[paste0("remover_", id)]], {
      linha_pendente_exclusao(id)
      showModal(modalDialog(
        title = tagList(icon("trash"), " Confirmar exclusão"),
        "Deseja apagar esta amostra? Esta ação não pode ser desfeita.",
        easyClose = TRUE,
        footer = tagList(modalButton("Cancelar"), actionButton("confirmar_exclusao", "Apagar amostra", icon = icon("trash"), class = "btn-danger"))
      ))
    }, ignoreInit = TRUE)
  }
  registrar_amostra(1L)

  output$explicacao_sistema <- renderUI({
    if (modo_atual() == "LAB") HTML("Edite <b>L*</b>, <b>a*</b> e <b>b*</b>.") else HTML("Edite <b>L*</b>, <b>C*</b> e <b>h°</b>.")
  })
  output$numero_amostras <- renderText(paste(length(ids_amostras()), if (length(ids_amostras()) == 1) "amostra" else "amostras"))
  output$cabecalho_ui <- renderUI({
    nomes <- if (modo_atual() == "LAB") {
      c("Nome da amostra", "L* (0–100)", "a* (≈ −128 a +127)", "b* (≈ −128 a +127)", "Cor", "")
    } else {
      c("Nome da amostra", "L* (0–100)", "C* (≥ 0)", "h° (0–360)", "Cor", "")
    }
    div(class = "cabecalho-amostras", fluidRow(column(3, nomes[1]), column(2, nomes[2]), column(2, nomes[3]), column(2, nomes[4]), column(2, nomes[5]), column(1, nomes[6])))
  })
  output$amostras_ui <- renderUI({
    ids <- ids_amostras(); dados <- dados_amostras()
    if (!length(ids)) return(div(class = "nenhuma-amostra", "Nenhuma amostra adicionada."))
    tagList(lapply(ids, function(id) criar_linha_ui(id, dados[[as.character(id)]], modo_atual())))
  })

  observeEvent(input$adicionar, {
    salvar_valores()
    id <- proximo_id(); dados <- dados_amostras()
    dados[[as.character(id)]] <- list(nome = paste("Amostra", length(ids_amostras()) + 1), L = 50, x = 0, y = 0)
    dados_amostras(dados); registrar_amostra(id)
    ids_amostras(c(ids_amostras(), id)); proximo_id(id + 1L)
  })
  observeEvent(input$confirmar_exclusao, {
    id <- linha_pendente_exclusao()
    if (!is.null(id)) {
      salvar_valores(); dados <- dados_amostras(); dados[[as.character(id)]] <- NULL
      dados_amostras(dados); ids_amostras(ids_amostras()[ids_amostras() != id]); linha_pendente_exclusao(NULL)
    }
    removeModal()
  })
  observeEvent(input$sistema, {
    novo <- input$sistema; antigo <- modo_atual()
    if (identical(novo, antigo)) return()
    salvar_valores(antigo); dados <- dados_amostras()
    for (id in ids_amostras()) {
      chave <- as.character(id); linha <- dados[[chave]]
      if (antigo == "LAB" && novo == "LCH") { valor <- lab_para_lch(linha$x, linha$y); linha$x <- valor$C; linha$y <- valor$h }
      if (antigo == "LCH" && novo == "LAB") { valor <- lch_para_lab(max(0, linha$x), linha$y %% 360); linha$x <- valor$a; linha$y <- valor$b }
      dados[[chave]] <- linha
    }
    dados_amostras(dados); modo_atual(novo)
  }, ignoreInit = TRUE)

  output$tabela_importacao <- renderDT({
    dados <- dados_importados()
    visualizacao <- dados[, c("Nome", "L", "a", "b", "C", "h"), drop = FALSE]
    visualizacao$Cor <- vapply(dados$Hex, function(hex) paste0("<div class='cor-preview' style='background-color:", hex, ";color:", cor_texto(hex), ";'>", hex, "</div>"), character(1))
    datatable(visualizacao, rownames = FALSE, escape = FALSE,
      editable = list(target = "cell", disable = list(columns = c(6))),
      colnames = c("Nome da amostra", "L* (0–100)", "a* (≈ −128 a +127)", "b* (≈ −128 a +127)", "C* (≥ 0)", "h° (0–360)", "Cor"),
      options = list(
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "Todas")),
        lengthChange = TRUE,
        ordering = FALSE,
        scrollX = TRUE,
        language = list(lengthMenu = "Exibir _MENU_ linhas", search = "Buscar:", info = "Mostrando _START_ a _END_ de _TOTAL_ amostras", infoEmpty = "Nenhuma amostra importada", paginate = list(previous = "Anterior", "next" = "Próxima")),
        columnDefs = list(list(width = "150px", targets = 6))
      ))
  }, server = FALSE)

  observeEvent(input$tabela_importacao_cell_edit, {
    info <- input$tabela_importacao_cell_edit
    dados <- dados_importados()
    linha <- info$row; coluna <- info$col + 1
    if (linha < 1 || linha > nrow(dados)) return()
    if (coluna == 1) {
      dados$Nome[linha] <- as.character(info$value)
    } else {
      valor <- numero_seguro(info$value, NA_real_)
      if (is.na(valor)) { showNotification("Digite um valor numérico válido.", type = "error"); return() }
      if (coluna == 2) {
        if (valor < 0 || valor > 100) { showNotification("L* deve estar entre 0 e 100.", type = "error"); return() }
        dados$L[linha] <- valor
      }
      if (coluna == 3) { dados$a[linha] <- valor; lch <- lab_para_lch(dados$a[linha], dados$b[linha]); dados$C[linha] <- lch$C; dados$h[linha] <- lch$h }
      if (coluna == 4) { dados$b[linha] <- valor; lch <- lab_para_lch(dados$a[linha], dados$b[linha]); dados$C[linha] <- lch$C; dados$h[linha] <- lch$h }
      if (coluna == 5) {
        if (valor < 0) { showNotification("C* não pode ser negativo.", type = "error"); return() }
        dados$C[linha] <- valor; lab <- lch_para_lab(dados$C[linha], dados$h[linha]); dados$a[linha] <- lab$a; dados$b[linha] <- lab$b
      }
      if (coluna == 6) { dados$h[linha] <- valor %% 360; lab <- lch_para_lab(dados$C[linha], dados$h[linha]); dados$a[linha] <- lab$a; dados$b[linha] <- lab$b }
      dados[linha, c("a", "b", "C", "h")] <- round(dados[linha, c("a", "b", "C", "h")], 3)
      dados$Hex[linha] <- lab_para_hex(dados$L[linha], dados$a[linha], dados$b[linha])
    }
    dados_importados(dados)
  })

  observeEvent(input$importar, {
    texto <- input$dados_colados
    if (is.null(texto) || !nzchar(trimws(texto))) { showNotification("Cole primeiro os dados copiados do Excel.", type = "warning"); return() }
    resultado <- tryCatch({
      linhas <- unlist(strsplit(gsub("\r", "", texto, fixed = TRUE), "\n", fixed = TRUE)); linhas <- linhas[nzchar(trimws(linhas))]
      if (isTRUE(input$tem_cabecalho)) { if (length(linhas) < 2) stop("Não há linhas de dados abaixo do cabeçalho."); linhas <- linhas[-1] }
      partes <- lapply(linhas, function(x) trimws(unlist(strsplit(x, "\t", fixed = TRUE))))
      if (any(lengths(partes) < 3)) stop("Cada linha precisa ter Nome + três coordenadas ou apenas três coordenadas.")
      novas <- lapply(seq_along(partes), function(i) {
        x <- partes[[i]]
        # 5+ colunas: Grupo | Nome | coordenadas; 4 colunas: Nome | coordenadas; 3 colunas: só coordenadas
        grupo <- if (length(x) >= 5 && nzchar(x[1])) x[1] else NA_character_
        if (length(x) >= 5) x <- x[-1]
        tem_nome <- length(x) >= 4; inicio <- if (tem_nome) 2 else 1
        nome <- if (tem_nome && nzchar(x[1])) x[1] else paste("Amostra", i)
        L <- numero_seguro(x[inicio], NA_real_); x1 <- numero_seguro(x[inicio + 1], NA_real_); x2 <- numero_seguro(x[inicio + 2], NA_real_)
        if (anyNA(c(L, x1, x2))) stop(paste("Existe valor não numérico na linha", i, "."))
        if (L < 0 || L > 100) stop(paste("L* deve estar entre 0 e 100. Erro na linha", i))
        if (input$formato_colado == "LAB") { a <- x1; b <- x2; lch <- lab_para_lch(a, b); C <- lch$C; h <- lch$h } else {
          if (x1 < 0) stop(paste("C* não pode ser negativo. Erro na linha", i)); C <- x1; h <- x2 %% 360; lab <- lch_para_lab(C, h); a <- lab$a; b <- lab$b
        }
        data.frame(Nome = nome, Grupo = grupo, L = round(L, 3), a = round(a, 3), b = round(b, 3), C = round(C, 3), h = round(h, 3), Hex = lab_para_hex(L, a, b), stringsAsFactors = FALSE)
      })
      do.call(rbind, novas)
    }, error = function(e) { showNotification(e$message, type = "error", duration = 8); NULL })
    if (is.null(resultado)) return()
    dados_importados(if (input$acao_importacao == "adicionar") rbind(dados_importados(), resultado) else resultado)
    updateRadioButtons(session, "fonte_dados", selected = "importar")
    grupos <- agrupar_dados(dados_importados(), "auto")$grupos
    if (nrow(grupos) < nrow(dados_importados())) {
      updateCheckboxInput(session, "agrupar", value = TRUE)
      showNotification(paste0(nrow(resultado), " amostra(s) importada(s). Repetições detectadas: agrupadas em ", nrow(grupos), " grupos."), type = "message", duration = 7)
    } else showNotification(paste(nrow(resultado), "amostra(s) importada(s) com sucesso."), type = "message")
  })

  dados_editados_para_relatorio <- function() {
    ids <- ids_amostras()
    if (!length(ids)) return(dados_importados()[0, ])
    linhas <- lapply(ids, function(id) {
      lab <- obter_lab(id)
      lch <- lab_para_lch(lab$a, lab$b)
      data.frame(Nome = lab$nome, Grupo = NA_character_, L = round(lab$L, 3), a = round(lab$a, 3), b = round(lab$b, 3), C = round(lch$C, 3), h = round(lch$h, 3), Hex = lab_para_hex(lab$L, lab$a, lab$b), stringsAsFactors = FALSE)
    })
    do.call(rbind, linhas)
  }

  # ------------------------------------------------------------
  # Visualização (compartilhada com o relatório PDF)
  # ------------------------------------------------------------
  dados_individuais <- debounce(reactive({
    base <- if (identical(input$fonte_dados, "importar")) dados_importados() else dados_editados_para_relatorio()
    completar_dados(base)
  }), 350)

  agrupado <- reactive(isTRUE(input$agrupar))
  agrupamento <- reactive(agrupar_dados(dados_individuais(), input$criterio_grupo %||% "auto", input$estatistica %||% "media"))
  dados_ativos <- reactive(if (agrupado()) agrupamento()$grupos else dados_individuais())
  repeticoes_graficos <- reactive(if (agrupado() && isTRUE(input$mostrar_rep)) agrupamento()$repeticoes else NULL)
  unidade <- reactive(if (agrupado()) "grupo" else "amostra")

  referencia_ativa <- reactive({
    ref <- suppressWarnings(as.integer(input$referencia %||% NA))[1]
    n <- nrow(dados_ativos())
    if (!n) NA_integer_ else if (is.na(ref) || ref > n) 1L else ref
  })

  observeEvent(input$modo_entrada, {
    fonte <- if (identical(input$modo_entrada, "Importar dados")) "importar" else "editar"
    if (fonte == "editar" || nrow(dados_importados()) > 0) updateRadioButtons(session, "fonte_dados", selected = fonte)
  }, ignoreInit = TRUE)

  observe({
    dados <- dados_ativos()
    escolhas <- if (nrow(dados)) setNames(as.character(dados$Id), paste0("#", dados$Id, " · ", dados$Nome)) else c("Nenhuma amostra" = "")
    atual <- isolate(input$referencia)
    selecionada <- if (!is.null(atual) && atual %in% escolhas) atual else escolhas[1]
    updateSelectInput(session, "referencia", choices = escolhas, selected = selecionada)
  })

  output$info_grupos <- renderUI({
    ind <- dados_individuais()
    if (!nrow(ind)) return(NULL)
    g <- agrupamento()$grupos
    if (!agrupado()) {
      if (nrow(g) < nrow(ind)) return(div(class = "aviso-grupos", icon("lightbulb"), sprintf(" Parece haver repetições: os nomes formam %d grupos. Marque \"Agrupar repetições\" para comparar os grupos.", nrow(g))))
      return(NULL)
    }
    rotulos <- paste0(g$Nome, " (", g$n, ")")
    extra <- if (length(rotulos) > 12) paste0(" … e mais ", length(rotulos) - 12) else ""
    atip <- sum(agrupamento()$repeticoes$Atipica, na.rm = TRUE)
    div(class = "aviso-grupos aviso-ok",
      tags$b(sprintf("%d grupos formados a partir de %d repetições: ", nrow(g), nrow(ind))),
      paste(head(rotulos, 12), collapse = " · "), extra,
      if (atip) span(class = "alerta-atipica", icon("triangle-exclamation"), sprintf(" %d repetição(ões) possivelmente atípica(s) — veja a aba Repetições.", atip))
    )
  })

  output$resumo_visual <- renderUI({
    dados <- dados_ativos()
    if (!nrow(dados)) return(NULL)
    r <- resumo_cores(dados)
    kpi <- function(rotulo, valor, detalhe) div(class = "kpi", div(class = "kpi-rotulo", rotulo), div(class = "kpi-valor", valor), div(class = "kpi-detalhe", detalhe))
    div(class = "resumo-visual",
      div(class = "kpi kpi-media",
        div(class = "kpi-amostra", style = paste0("background:", r$Hex, ";color:", cor_texto(r$Hex), ";"), r$Hex),
        div(div(class = "kpi-rotulo", if (agrupado()) "Cor média dos grupos" else "Cor média"), div(class = "kpi-valor", nome_tom(r$L, r$C, r$h)),
            div(class = "kpi-detalhe", sprintf("L* %s · a* %s · b* %s", fmt(r$L), fmt(r$a), fmt(r$b))))
      ),
      if (agrupado()) kpi("Grupos", r$n, paste(r$n_rep, "repetições ·", if (identical(input$estatistica, "mediana")) "mediana" else "média"))
      else kpi("Amostras", r$n, if (r$fora) paste(r$fora, "fora do gamut sRGB") else "todas dentro do gamut sRGB"),
      kpi(paste("L* médio ± dp"), paste(fmt(r$L, 1), "±", fmt(r$L_dp, 1)), paste("faixa", fmt(r$L_min, 1), "a", fmt(r$L_max, 1))),
      kpi("C* médio", fmt(r$C_media, 1), paste("faixa", fmt(r$C_min, 1), "a", fmt(r$C_max, 1))),
      if (agrupado() && !is.na(r$dEintra)) kpi(HTML("Homogeneidade (ΔE<sub>00</sub>)"), fmt(r$dEintra), "distância média das repetições ao centro")
      else kpi("h° da cor média", paste0(fmt(r$h, 1), "°"), "ângulo de matiz")
    )
  })

  output$paleta_cores <- renderUI({
    dados <- dados_ativos()
    if (!nrow(dados)) return(div(class = "nenhuma-amostra", icon("palette"), " Nenhuma amostra para visualizar. Adicione ou importe dados acima."))
    ref <- referencia_ativa(); grp <- agrupado()
    reps <- if (grp) agrupamento()$repeticoes else NULL
    val <- function(sigla, x, dp = NULL) span(HTML(paste0("<b>", sigla, "</b> ", fmt(x, if (grp) 1 else 2), if (!is.null(dp)) paste0(" <small>±", fmt(dp, 1), "</small>") else "")))
    div(class = "grade-paleta", lapply(seq_len(nrow(dados)), function(i) {
      membros <- if (grp) reps[reps$GrupoId == i, ] else NULL
      div(class = paste("cartao-cor", if (identical(i, ref)) "cartao-ref"),
        div(class = "cartao-amostra", style = paste0("background:", dados$Hex[i], ";color:", cor_texto(dados$Hex[i]), ";"),
          span(class = "cartao-id", dados$Id[i]),
          if (dados$Fora[i]) span(class = "cartao-aviso", title = "Fora do gamut sRGB: cor ajustada para exibição", "ajustada"),
          span(class = "cartao-hex", dados$Hex[i]),
          if (grp) div(class = "faixa-rep", lapply(seq_len(nrow(membros)), function(k)
            span(class = paste("rep", if (isTRUE(membros$Atipica[k])) "rep-atipica"), style = paste0("background:", membros$Hex[k]),
                 title = sprintf("%s — ΔE00 ao centro: %s", membros$Nome[k], fmt(membros$dEcentro[k])))))
        ),
        div(class = "cartao-corpo",
          div(class = "cartao-nome", title = dados$Nome[i], dados$Nome[i]),
          div(class = "cartao-tom", if (grp) paste0("n = ", dados$n[i], " · "), dados$Tom[i], if (identical(i, ref)) span(class = "selo-ref", "referência")),
          div(class = "cartao-valores",
            val("L*", dados$L[i], if (grp) dados$dpL[i]), val("a*", dados$a[i], if (grp) dados$dpa[i]),
            val("b*", dados$b[i], if (grp) dados$dpb[i]), val("C*", dados$C[i], if (grp) dados$dpC[i]),
            span(HTML(paste0("<b>h°</b> ", fmt(dados$h[i], if (grp) 1 else 2), "°"))),
            if (grp) span(HTML(paste0("<b>ΔE<sub>00</sub></b> ", fmt(dados$dEintra[i]))), title = "Homogeneidade: distância média das repetições ao centro do grupo")
          )
        )
      )
    }))
  })

  output$grafico_ab <- renderPlot({
    dados <- dados_ativos()
    validate(need(nrow(dados) > 0, "Adicione ou importe amostras para ver o gráfico."))
    grafico_plano_ab(dados, referencia_ativa(), repeticoes = repeticoes_graficos())
  }, res = 96)

  output$grafico_lc <- renderPlot({
    dados <- dados_ativos()
    validate(need(nrow(dados) > 0, "Adicione ou importe amostras para ver o gráfico."))
    grafico_LC(dados, referencia_ativa(), repeticoes = repeticoes_graficos())
  }, res = 96)

  idioma_dt <- list(info = "Mostrando _START_ a _END_ de _TOTAL_ linhas", infoEmpty = "Nenhum dado", paginate = list(previous = "Anterior", "next" = "Próxima"))
  cor_html <- function(hex) sprintf("<div class='cor-preview cor-mini' style='background:%s;color:%s;'>%s</div>", hex, cor_texto(hex), hex)

  output$tabela_delta <- renderDT({
    dados <- dados_ativos()
    validate(need(nrow(dados) > 1, if (agrupado()) "São necessários pelo menos dois grupos para calcular a diferença de cor." else "São necessárias pelo menos duas amostras para calcular a diferença de cor."))
    d <- tabela_delta(dados, referencia_ativa())
    visual <- data.frame("#" = d$Id, Cor = cor_html(d$Hex), check.names = FALSE, stringsAsFactors = FALSE)
    visual[[if (agrupado()) "Grupo" else "Amostra"]] <- d$Nome
    if (agrupado()) visual$n <- dados$n
    visual <- cbind(visual, data.frame("ΔL*" = round(d$dL, 2), "Δa*" = round(d$da, 2), "Δb*" = round(d$db, 2), "ΔC*" = round(d$dC, 2), "ΔH*" = round(d$dH, 2),
      "ΔE*ab" = round(d$dE76, 2), "ΔE00" = round(d$dE00, 2), "Diferença" = d$Classe, check.names = FALSE, stringsAsFactors = FALSE))
    if (agrupado()) visual[["ΔE00 intra"]] <- round(dados$dEintra, 2)
    datatable(visual, rownames = FALSE, escape = -2, selection = "none",
      options = list(pageLength = 25, ordering = TRUE, scrollX = TRUE, dom = "tip", language = idioma_dt)) |>
      formatStyle("ΔE00", fontWeight = "bold") |>
      formatStyle("Diferença", color = styleEqual(c("Referência", "Imperceptível", "Muito pequena", "Pequena", "Perceptível", "Grande"),
                                                  c("#347b46", "#347b46", "#5d8a3a", "#926800", "#b96a2a", "#b94b4b")), fontWeight = "bold")
  }, server = FALSE)

  output$tabela_repeticoes <- renderDT({
    validate(need(agrupado(), "Marque \"Agrupar repetições\" para ver os detalhes de cada repetição."))
    r <- agrupamento()$repeticoes
    validate(need(nrow(r) > 0, "Nenhuma amostra."))
    visual <- data.frame(Grupo = paste0(r$GrupoId, " · ", r$GrupoNome), Cor = cor_html(r$Hex), Amostra = r$Nome,
      "L*" = round(r$L, 2), "a*" = round(r$a, 2), "b*" = round(r$b, 2), "C*" = round(r$C, 2), "h°" = round(r$h, 2),
      "ΔE00 ao centro" = round(r$dEcentro, 2), Situação = ifelse(r$Atipica, "Possivelmente atípica", "OK"),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(visual, rownames = FALSE, escape = -2, selection = "none",
      options = list(pageLength = 25, ordering = TRUE, scrollX = TRUE, dom = "tip", language = idioma_dt)) |>
      formatStyle("ΔE00 ao centro", fontWeight = "bold") |>
      formatStyle("Situação", color = styleEqual(c("OK", "Possivelmente atípica"), c("#347b46", "#b94b4b")), fontWeight = "bold")
  }, server = FALSE)

  output$info_relatorio <- renderText({
    dados <- dados_ativos()
    if (!nrow(dados)) return("Nenhuma amostra disponível para o relatório.")
    fonte <- if (identical(input$fonte_dados, "importar")) "dados importados" else "amostras editadas"
    ref <- referencia_ativa()
    qtd <- if (agrupado()) paste(nrow(dados), "grupos de", sum(dados$n), "repetições") else paste(nrow(dados), if (nrow(dados) == 1) "amostra" else "amostras")
    paste0("O PDF incluirá ", qtd, " (", fonte, ")",
           if (nrow(dados) > 1 && "delta" %in% input$secoes_relatorio) paste0(", com ΔE em relação a #", ref, " ", dados$Nome[ref]) else "", ".")
  })

  output$baixar_pdf <- downloadHandler(
    filename = function() {
      base <- gsub("[^A-Za-z0-9]+", "_", iconv(trimws(input$titulo_relatorio %||% ""), "UTF-8", "ASCII//TRANSLIT", sub = ""))
      base <- gsub("^_+|_+$", "", tolower(base))
      if (!nzchar(base)) base <- "relatorio_cielab_cielch"
      paste0(base, "_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf")
    },
    content = function(arquivo) {
      dados <- dados_ativos()
      if (!nrow(dados)) {
        showNotification("Não há amostras para exportar. Adicione ou importe dados.", type = "error")
        stop("Sem dados para o relatório.")
      }
      origem <- if (identical(input$fonte_dados, "importar")) "Dados importados" else "Amostras editadas"
      gerar_relatorio_pdf(arquivo, input$titulo_relatorio, input$descricao_relatorio, dados, origem,
                          responsavel = input$responsavel_relatorio, referencia = referencia_ativa(),
                          secoes = input$secoes_relatorio %||% character(),
                          repeticoes = if (agrupado()) agrupamento()$repeticoes else NULL,
                          estatistica = input$estatistica %||% "media", rep_graficos = isTRUE(input$mostrar_rep))
    },
    contentType = "application/pdf"
  )
}
