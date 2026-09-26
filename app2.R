# ============================================================
# VISUALIZADOR DE CORES — CIELAB / CIELCH
# Múltiplas amostras
# ============================================================

# Instala os pacotes caso necessário
pacotes <- c("shiny", "colorspace")

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(colorspace)


# ============================================================
# FUNÇÕES AUXILIARES
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


numero_seguro <- function(x, padrao = 0) {

  valor <- suppressWarnings(as.numeric(x))

  if (
    length(valor) == 0 ||
    is.na(valor) ||
    !is.finite(valor)
  ) {
    return(padrao)
  }

  valor
}


# ------------------------------------------------------------
# Converte LAB para HEX
# ------------------------------------------------------------

lab_para_hex <- function(L, a, b) {

  tryCatch({

    cor <- colorspace::LAB(
      L = L,
      A = a,
      B = b
    )

    resultado <- colorspace::hex(
      cor,
      fixup = TRUE
    )

    if (is.na(resultado)) {
      "#FFFFFF"
    } else {
      resultado
    }

  }, error = function(e) {

    "#FFFFFF"

  })
}


# ------------------------------------------------------------
# Verifica se está fora do gamut sRGB
# ------------------------------------------------------------

fora_gamut <- function(L, a, b) {

  tryCatch({

    cor <- colorspace::LAB(
      L = L,
      A = a,
      B = b
    )

    original <- colorspace::hex(
      cor,
      fixup = FALSE
    )

    is.na(original)

  }, error = function(e) {

    TRUE

  })
}


# ------------------------------------------------------------
# Escolhe texto preto ou branco sobre a cor
# ------------------------------------------------------------

cor_texto <- function(hex) {

  rgb <- col2rgb(hex)

  luminancia <-
    0.2126 * rgb[1, 1] +
    0.7152 * rgb[2, 1] +
    0.0722 * rgb[3, 1]

  if (luminancia > 150) {
    "#000000"
  } else {
    "#FFFFFF"
  }
}


# ============================================================
# INTERFACE
# ============================================================

ui <- fluidPage(

  tags$head(

    tags$style(
      HTML("

      body {
        background: #f4f6f5;
        font-family: Arial, Helvetica, sans-serif;
      }

      .container-fluid {
        max-width: 1250px;
        margin: auto;
        padding-bottom: 50px;
      }

      .titulo {
        font-size: 30px;
        font-weight: 700;
        margin-top: 28px;
        margin-bottom: 5px;
      }

      .subtitulo {
        color: #666;
        font-size: 15px;
        margin-bottom: 25px;
      }

      .painel {
        background: white;
        border-radius: 14px;
        padding: 24px;
        box-shadow: 0 2px 12px rgba(0,0,0,.07);
        margin-bottom: 22px;
      }

      .cabecalho-amostras {
        font-size: 13px;
        font-weight: 700;
        color: #555;
        padding: 0 10px 8px 10px;
        border-bottom: 1px solid #ddd;
        margin-bottom: 10px;
      }

      .linha-amostra {
        padding: 12px 10px 3px 10px;
        border-bottom: 1px solid #eeeeee;
      }

      .linha-amostra:last-child {
        border-bottom: none;
      }

      .form-group {
        margin-bottom: 10px;
      }

      .cor-amostra {
        height: 46px;
        width: 100%;
        min-width: 90px;
        border-radius: 7px;
        border: 1px solid rgba(0,0,0,.15);
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 12px;
        letter-spacing: .3px;
      }

      .gamut-aviso {
        font-size: 10px;
        text-align: center;
        color: #8a6100;
        margin-top: 3px;
      }

      .btn-adicionar {
        margin-top: 18px;
      }

      .btn-remover {
        width: 100%;
        height: 46px;
        margin-top: 0;
      }

      .comparacao-container {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        margin-top: 15px;
      }

      .cartao-cor {
        width: 150px;
        background: white;
        border: 1px solid #ddd;
        border-radius: 11px;
        overflow: hidden;
      }

      .cartao-cor-visual {
        height: 105px;
        width: 100%;
      }

      .cartao-cor-info {
        padding: 10px;
      }

      .cartao-cor-nome {
        font-weight: 700;
        font-size: 13px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .cartao-cor-hex {
        color: #666;
        font-size: 12px;
        margin-top: 2px;
      }

      .nenhuma-amostra {
        color: #777;
        text-align: center;
        padding: 35px;
      }

      .explicacao {
        color: #666;
        font-size: 13px;
        line-height: 1.5;
      }

      .radio-inline {
        margin-right: 25px;
      }

      @media (max-width: 767px) {

        .cabecalho-amostras {
          display: none;
        }

        .linha-amostra {
          background: #fafafa;
          border: 1px solid #ddd;
          border-radius: 10px;
          margin-bottom: 12px;
          padding: 15px;
        }

        .cartao-cor {
          width: calc(50% - 7px);
        }

      }

      ")
    )
  ),


  # ==========================================================
  # TÍTULO
  # ==========================================================

  div(
    class = "titulo",
    "Visualizador CIELAB / CIELCH"
  ),

  div(
    class = "subtitulo",
    paste(
      "Adicione quantas amostras desejar e compare",
      "visualmente as cores obtidas."
    )
  ),


  # ==========================================================
  # SELEÇÃO DO SISTEMA
  # ==========================================================

  div(

    class = "painel",

    h4("Sistema de cor"),

    radioButtons(
      inputId = "sistema",
      label = NULL,
      choices = c(
        "CIELAB — L* a* b*" = "LAB",
        "CIELCH — L* C* h°" = "LCH"
      ),
      selected = "LAB",
      inline = TRUE
    ),

    div(
      class = "explicacao",
      uiOutput("explicacao_sistema")
    )
  ),


  # ==========================================================
  # AMOSTRAS
  # ==========================================================

  div(

    class = "painel",

    h4("Amostras"),

    br(),

    uiOutput("cabecalho_ui"),

    uiOutput("amostras_ui"),

    actionButton(
      inputId = "adicionar",
      label = "＋ Adicionar amostra",
      class = "btn btn-success btn-adicionar"
    )
  ),


  # ==========================================================
  # COMPARAÇÃO
  # ==========================================================

  div(

    class = "painel",

    h4("Comparação das cores"),

    div(
      class = "explicacao",
      paste(
        "As cores de todas as amostras são apresentadas",
        "lado a lado para facilitar a comparação."
      )
    ),

    uiOutput("comparacao_ui")
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # ESTADO
  # ==========================================================

  modo_atual <- reactiveVal("LAB")

  ids_amostras <- reactiveVal(c(1L))

  proximo_id <- reactiveVal(2L)

  dados_amostras <- reactiveVal(

    list(

      "1" = list(
        nome = "Amostra 1",
        L = 60,
        x = 25,
        y = 40
      )

    )

  )


  # ==========================================================
  # LER UMA LINHA DA INTERFACE
  # ==========================================================

  ler_linha <- function(id, modo) {

    dados <- dados_amostras()

    padrao <- dados[[as.character(id)]]

    if (is.null(padrao)) {

      padrao <- list(
        nome = paste("Amostra", id),
        L = 60,
        x = 0,
        y = 0
      )
    }


    nome <-
      input[[paste0("nome_", id)]] %||%
      padrao$nome


    L <- numero_seguro(
      input[[paste0("L_", id)]],
      padrao$L
    )


    if (modo == "LAB") {

      x <- numero_seguro(
        input[[paste0("a_", id)]],
        padrao$x
      )

      y <- numero_seguro(
        input[[paste0("b_", id)]],
        padrao$y
      )

    } else {

      x <- numero_seguro(
        input[[paste0("C_", id)]],
        padrao$x
      )

      y <- numero_seguro(
        input[[paste0("H_", id)]],
        padrao$y
      )
    }


    list(
      nome = nome,
      L = L,
      x = x,
      y = y
    )
  }


  # ==========================================================
  # SALVAR VALORES ATUAIS DA TELA
  # ==========================================================

  salvar_valores <- function(modo = modo_atual()) {

    ids <- ids_amostras()

    dados <- dados_amostras()

    for (id in ids) {

      dados[[as.character(id)]] <-
        ler_linha(id, modo)
    }

    dados_amostras(dados)
  }


  # ==========================================================
  # CONVERTER LINHA PARA LAB
  # ==========================================================

  obter_lab <- function(id) {

    modo <- modo_atual()

    linha <- ler_linha(
      id = id,
      modo = modo
    )

    L <- linha$L


    if (modo == "LAB") {

      a <- linha$x
      b <- linha$y

    } else {

      C <- linha$x
      H <- linha$y

      rad <- H * pi / 180

      a <- C * cos(rad)
      b <- C * sin(rad)
    }


    list(
      nome = linha$nome,
      L = L,
      a = a,
      b = b
    )
  }


  # ==========================================================
  # CRIAR INTERFACE DE UMA AMOSTRA
  # ==========================================================

  criar_linha_ui <- function(id, dados, modo) {

    if (modo == "LAB") {

      campos <- tagList(

        column(
          width = 2,

          numericInput(
            inputId = paste0("a_", id),
            label = NULL,
            value = round(dados$x, 2),
            step = 0.1
          )
        ),

        column(
          width = 2,

          numericInput(
            inputId = paste0("b_", id),
            label = NULL,
            value = round(dados$y, 2),
            step = 0.1
          )
        )
      )

    } else {

      campos <- tagList(

        column(
          width = 2,

          numericInput(
            inputId = paste0("C_", id),
            label = NULL,
            value = round(dados$x, 2),
            min = 0,
            step = 0.1
          )
        ),

        column(
          width = 2,

          numericInput(
            inputId = paste0("H_", id),
            label = NULL,
            value = round(dados$y, 2),
            min = 0,
            max = 360,
            step = 0.1
          )
        )
      )
    }


    div(

      id = paste0("linha_", id),

      class = "linha-amostra",

      fluidRow(

        column(
          width = 3,

          textInput(
            inputId = paste0("nome_", id),
            label = NULL,
            value = dados$nome,
            placeholder = "Nome da amostra"
          )
        ),


        column(
          width = 2,

          numericInput(
            inputId = paste0("L_", id),
            label = NULL,
            value = round(dados$L, 2),
            min = 0,
            max = 100,
            step = 0.1
          )
        ),


        campos,


        column(
          width = 2,

          uiOutput(
            outputId = paste0("cor_", id)
          )
        ),


        column(
          width = 1,

          actionButton(
            inputId = paste0("remover_", id),
            label = "−",
            class = "btn btn-danger btn-remover",
            title = "Remover amostra"
          )
        )
      )
    )
  }


  # ==========================================================
  # REGISTRAR UMA NOVA AMOSTRA
  # ==========================================================

  registrar_amostra <- function(id) {

    force(id)


    # --------------------------------------------------------
    # COR DA LINHA
    # --------------------------------------------------------

    output[[paste0("cor_", id)]] <- renderUI({

      lab <- obter_lab(id)

      hex <- lab_para_hex(
        lab$L,
        lab$a,
        lab$b
      )

      texto <- cor_texto(hex)

      gamut <- fora_gamut(
        lab$L,
        lab$a,
        lab$b
      )


      tagList(

        div(

          class = "cor-amostra",

          style = paste0(
            "background-color:", hex, ";",
            "color:", texto, ";"
          ),

          hex
        ),

        if (gamut) {

          div(
            class = "gamut-aviso",
            "ajustada para sRGB"
          )

        }
      )
    })


    # --------------------------------------------------------
    # BOTÃO REMOVER
    # --------------------------------------------------------

    observeEvent(

      input[[paste0("remover_", id)]],

      {

        salvar_valores()

        ids <- ids_amostras()

        dados <- dados_amostras()


        ids <- ids[ids != id]

        dados[[as.character(id)]] <- NULL


        dados_amostras(dados)

        ids_amostras(ids)
      },

      ignoreInit = TRUE
    )
  }


  # ==========================================================
  # REGISTRAR PRIMEIRA AMOSTRA
  # ==========================================================

  registrar_amostra(1L)


  # ==========================================================
  # CABEÇALHO
  # ==========================================================

  output$cabecalho_ui <- renderUI({

    modo <- modo_atual()


    if (modo == "LAB") {

      x_nome <- "a*"
      y_nome <- "b*"

    } else {

      x_nome <- "C*"
      y_nome <- "h°"
    }


    div(

      class = "cabecalho-amostras",

      fluidRow(

        column(
          3,
          "Nome da amostra"
        ),

        column(
          2,
          "L*"
        ),

        column(
          2,
          x_nome
        ),

        column(
          2,
          y_nome
        ),

        column(
          2,
          "Cor"
        ),

        column(
          1,
          ""
        )
      )
    )
  })


  # ==========================================================
  # MOSTRAR AS AMOSTRAS
  # ==========================================================

  output$amostras_ui <- renderUI({

    ids <- ids_amostras()

    modo <- modo_atual()

    dados <- dados_amostras()


    if (length(ids) == 0) {

      return(

        div(
          class = "nenhuma-amostra",
          "Nenhuma amostra adicionada."
        )

      )
    }


    tagList(

      lapply(

        ids,

        function(id) {

          linha <- dados[[as.character(id)]]

          criar_linha_ui(
            id = id,
            dados = linha,
            modo = modo
          )
        }
      )
    )
  })


  # ==========================================================
  # ADICIONAR NOVA AMOSTRA
  # ==========================================================

  observeEvent(input$adicionar, {

    salvar_valores()


    id <- proximo_id()

    modo <- modo_atual()

    dados <- dados_amostras()


    numero_amostra <-
      length(ids_amostras()) + 1


    # Valores iniciais

    if (modo == "LAB") {

      nova <- list(
        nome = paste("Amostra", numero_amostra),
        L = 60,
        x = 0,
        y = 0
      )

    } else {

      nova <- list(
        nome = paste("Amostra", numero_amostra),
        L = 60,
        x = 0,
        y = 0
      )
    }


    dados[[as.character(id)]] <- nova

    dados_amostras(dados)


    registrar_amostra(id)


    ids_amostras(
      c(
        ids_amostras(),
        id
      )
    )


    proximo_id(
      id + 1L
    )
  })


  # ==========================================================
  # TROCAR LAB <-> LCH
  # ==========================================================

  observeEvent(

    input$sistema,

    {

      novo_modo <- input$sistema

      antigo_modo <- modo_atual()


      if (identical(
        novo_modo,
        antigo_modo
      )) {

        return()
      }


      # Salvar valores atuais antes da conversão

      salvar_valores(antigo_modo)


      dados <- dados_amostras()

      ids <- ids_amostras()


      for (id in ids) {

        chave <- as.character(id)

        linha <- dados[[chave]]


        # ----------------------------------------------------
        # LAB -> LCH
        # ----------------------------------------------------

        if (
          antigo_modo == "LAB" &&
          novo_modo == "LCH"
        ) {

          a <- linha$x
          b <- linha$y

          C <- sqrt(
            a^2 + b^2
          )

          H <- atan2(
            b,
            a
          ) * 180 / pi


          if (H < 0) {
            H <- H + 360
          }


          linha$x <- C

          linha$y <- H
        }


        # ----------------------------------------------------
        # LCH -> LAB
        # ----------------------------------------------------

        if (
          antigo_modo == "LCH" &&
          novo_modo == "LAB"
        ) {

          C <- linha$x
          H <- linha$y

          rad <- H * pi / 180


          a <- C * cos(rad)

          b <- C * sin(rad)


          linha$x <- a

          linha$y <- b
        }


        dados[[chave]] <- linha
      }


      dados_amostras(dados)

      modo_atual(novo_modo)
    },

    ignoreInit = TRUE
  )


  # ==========================================================
  # EXPLICAÇÃO DO SISTEMA
  # ==========================================================

  output$explicacao_sistema <- renderUI({

    modo <- modo_atual()


    if (modo == "LAB") {

      HTML(
        paste0(
          "<b>L*</b> = luminosidade (0–100)<br>",
          "<b>a*</b> = verde (−) ↔ vermelho (+)<br>",
          "<b>b*</b> = azul (−) ↔ amarelo (+)"
        )
      )

    } else {

      HTML(
        paste0(
          "<b>L*</b> = luminosidade (0–100)<br>",
          "<b>C*</b> = croma ou intensidade da cor<br>",
          "<b>h°</b> = ângulo de matiz: ",
          "0° vermelho, 90° amarelo, ",
          "180° verde e 270° azul"
        )
      )
    }
  })


  # ==========================================================
  # COMPARAÇÃO LADO A LADO
  # ==========================================================

  output$comparacao_ui <- renderUI({

    ids <- ids_amostras()


    if (length(ids) == 0) {

      return(

        div(
          class = "nenhuma-amostra",
          "Adicione uma amostra para visualizar a comparação."
        )

      )
    }


    cartoes <- lapply(

      ids,

      function(id) {

        lab <- obter_lab(id)

        hex <- lab_para_hex(
          lab$L,
          lab$a,
          lab$b
        )


        nome <- lab$nome


        if (
          is.null(nome) ||
          nome == ""
        ) {

          nome <- paste(
            "Amostra",
            id
          )
        }


        div(

          class = "cartao-cor",

          div(

            class = "cartao-cor-visual",

            style = paste0(
              "background-color:",
              hex,
              ";"
            )
          ),

          div(

            class = "cartao-cor-info",

            div(
              class = "cartao-cor-nome",
              title = nome,
              nome
            ),

            div(
              class = "cartao-cor-hex",
              hex
            )
          )
        )
      }
    )


    div(
      class = "comparacao-container",
      cartoes
    )
  })
}


# ============================================================
# EXECUTAR
# ============================================================

shinyApp(
  ui = ui,
  server = server
)