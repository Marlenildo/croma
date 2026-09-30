ui <- fluidPage(
  tags$head(
    tags$script(async = NA, src = "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-3130340973057636", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$meta(name = "author", content = "Marlenildo"),
    tags$meta(name = "description", content = "Croma: visualizador CIELAB / CIELCH. Organize, converta e documente suas amostras de cor com precisão."),
    tags$link(rel = "icon", type = "image/png", href = "img/favicon.png"),
    tags$title("Croma · Visualizador CIELAB / CIELCH")
  ),
  div(class = "cabecalho-app",
    tags$img(src = "img/logo_app.png", class = "logo-app", alt = "Logo do Croma"),
    div(class = "titulo-area",
      div(class = "titulo", "Croma"),
      div(class = "descricao-app", "Visualizador CIELAB / CIELCH"),
      div(class = "subtitulo", "Organize, converta e documente suas amostras de cor com precisão.")
    )
  ),

  div(class = "painel painel-informacao",
    div(class = "titulo-legenda", icon("info-circle"), " Guia rápido das coordenadas"),
    div(class = "grade-coordenadas",
      div(class = "coord-item", tags$span("L*", class = "coord-sigla"), tags$span("0 a 100", class = "coord-faixa"), tags$span("Luminosidade", class = "coord-descricao")),
      div(class = "coord-item", tags$span("a*", class = "coord-sigla"), tags$span("≈ −128 a +127", class = "coord-faixa"), tags$span("Verde → vermelho", class = "coord-descricao")),
      div(class = "coord-item", tags$span("b*", class = "coord-sigla"), tags$span("≈ −128 a +127", class = "coord-faixa"), tags$span("Azul → amarelo", class = "coord-descricao")),
      div(class = "coord-item", tags$span("C*", class = "coord-sigla"), tags$span("≥ 0", class = "coord-faixa"), tags$span("Cromaticidade", class = "coord-descricao")),
      div(class = "coord-item", tags$span("h°", class = "coord-sigla"), tags$span("0 a 360°", class = "coord-faixa"), tags$span("Ângulo de matiz", class = "coord-descricao"))
    )
  ),

  div(class = "painel",
    tabsetPanel(id = "modo_entrada",
      tabPanel("Editar dados",
        br(),
        fluidRow(
          column(12,
            h4("Sistema de cor"),
            radioButtons("sistema", NULL,
            choices = c("CIELAB — L* (0–100), a* e b* (≈ −128 a +127)" = "LAB", "CIELCH — L* (0–100), C* (≥ 0), h° (0–360)" = "LCH"),
              selected = "LAB", inline = TRUE
            ),
            div(class = "explicacao", uiOutput("explicacao_sistema"))
          )
        ),
        tags$hr(),
        fluidRow(
          column(8, h4("Amostras")),
          column(4, div(class = "contador-amostras", textOutput("numero_amostras")))
        ),
        uiOutput("cabecalho_ui"),
        uiOutput("amostras_ui"),
        actionButton("adicionar", "Adicionar linha", icon = icon("plus-circle"), class = "btn-adicionar"),
        br(), br(),
        div(class = "explicacao", "Preencha os campos de cada linha. Para remover uma amostra, use o botão à direita.")
      ),

      tabPanel("Importar dados",
        br(),
        fluidRow(
          column(4, radioButtons("formato_colado", "Formato dos dados colados:",
            choices = c("CIELAB — Nome | L* (0–100) | a*, b* (≈ −128 a +127)" = "LAB", "CIELCH — Nome | L* (0–100) | C* (≥ 0) | h° (0–360)" = "LCH"), selected = "LAB")),
          column(4, checkboxInput("tem_cabecalho", "A primeira linha contém cabeçalho", TRUE)),
          column(4, radioButtons("acao_importacao", "Na tabela de importação:",
            choices = c("Substituir resultados" = "substituir", "Adicionar aos resultados" = "adicionar"), selected = "substituir"))
        ),
        div(class = "paste-box", textAreaInput("dados_colados", "Cole os dados aqui (Excel, planilha ou outra fonte):", width = "100%", rows = 8,
          placeholder = "Amostra\tL*\ta*\tb*\nManga A R1\t62,4\t18,3\t34,7\nManga A R2\t61,8\t17,9\t35,2\nManga B R1\t67,8\t14,1\t41,2\n\nOu com coluna de grupo:\nGrupo\tAmostra\tL*\ta*\tb*\nManga A\tR1\t62,4\t18,3\t34,7")),
        actionButton("importar", "Importar dados", icon = icon("paste"), class = "btn-adicionar"),
        br(), br(),
        div(class = "explicacao", HTML("Copie e cole diretamente os dados, de uma planilha, Excel ou outra fonte. Use o formato do exemplo: uma amostra por linha e valores separados por tabulação. Números com vírgula decimal, como <b>62,4</b>, são aceitos.<br><b>Repetições:</b> use nomes como <i>Manga A R1, Manga A R2…</i> (o número final é removido para formar o grupo) ou cole uma primeira coluna <b>Grupo</b> (5 colunas: Grupo | Amostra | L* | a*/C* | b*/h°).")),
        tags$hr(),
        h4("Dados importados"),
        DTOutput("tabela_importacao")
      )
    )
  ),

  div(class = "painel painel-visual",
    div(class = "cabecalho-secao",
      div(h4(icon("eye"), " Visualização das cores")),
      div(class = "tag-secao tag-verde", "ATUALIZA EM TEMPO REAL")
    ),
    fluidRow(
      column(5, radioButtons("fonte_dados", "Amostras exibidas", inline = TRUE,
        choices = c("Amostras editadas" = "editar", "Dados importados" = "importar"), selected = "editar")),
      column(4, selectInput("referencia", "Referência para diferença de cor (ΔE)", choices = c("Amostra 1" = "1"), width = "100%")),
      column(3, div(class = "explicacao nota-visual", "A referência aparece com um anel escuro nos gráficos."))
    ),
    div(class = "caixa-grupos",
      fluidRow(
        column(3, checkboxInput("agrupar", tags$b("Agrupar repetições"), FALSE),
          div(class = "explicacao", "Junta as repetições da mesma amostra e compara os grupos.")),
        column(3, conditionalPanel("input.agrupar",
          selectInput("criterio_grupo", "Como identificar o grupo", width = "100%",
            choices = c("Nome sem o nº da repetição (automático)" = "auto", "Nome exatamente igual" = "nome")))),
        column(3, conditionalPanel("input.agrupar",
          radioButtons("estatistica", "Valor do grupo", inline = TRUE, choices = c("Média" = "media", "Mediana" = "mediana")))),
        column(3, conditionalPanel("input.agrupar",
          checkboxInput("mostrar_rep", "Mostrar repetições nos gráficos", TRUE)))
      ),
      uiOutput("info_grupos")
    ),
    uiOutput("resumo_visual"),
    tabsetPanel(id = "aba_visual",
      tabPanel(tagList(icon("table-cells"), " Paleta"), br(), uiOutput("paleta_cores")),
      tabPanel(tagList(icon("chart-area"), " Gráficos"), br(),
        fluidRow(
          column(6, div(class = "titulo-grafico", "Plano cromático a* × b*"),
            div(class = "explicacao", "Distância ao centro = C* (saturação) · ângulo = h° (matiz)."),
            plotOutput("grafico_ab", height = "460px")),
          column(6, div(class = "titulo-grafico", "Luminosidade × cromaticidade (L* × C*)"),
            div(class = "explicacao", "Mostra se a amostra é clara/escura e neutra/saturada."),
            plotOutput("grafico_lc", height = "460px"))
        )
      ),
      tabPanel(tagList(icon("not-equal"), " Diferença de cor (ΔE)"), br(),
        div(class = "explicacao", HTML("ΔE<sub>00</sub> (CIEDE2000) em relação à referência: <b>&lt;1</b> imperceptível · <b>1–2</b> muito pequena · <b>2–3,5</b> pequena · <b>3,5–5</b> perceptível · <b>&gt;5</b> grande.")),
        DTOutput("tabela_delta")
      ),
      tabPanel(tagList(icon("layer-group"), " Repetições"), br(),
        div(class = "explicacao", HTML("ΔE<sub>00</sub> de cada repetição até o valor do seu grupo. <b>Possivelmente atípica</b> = mais de 2,5× a distância mediana do grupo (e &gt; 1,5): vale conferir a leitura.")),
        DTOutput("tabela_repeticoes")
      )
    )
  ),

  div(class = "painel painel-relatorio",
    div(class = "cabecalho-secao", div(h4(icon("file-pdf"), " Relatório em PDF")), div(class = "tag-secao", "PRONTO PARA IMPRESSÃO")),
    div(class = "explicacao", "Um clique gera o PDF completo (A4 paisagem) com resumo, cor média, gráficos, paleta, tabela de coordenadas e ΔE. Usa as amostras e a referência escolhidas na visualização."),
    br(),
    fluidRow(
      column(6, textInput("titulo_relatorio", "Título do relatório", value = "Relatório de cores CIELAB / CIELCH", width = "100%")),
      column(6, textInput("responsavel_relatorio", "Responsável (opcional)", placeholder = "Nome do avaliador ou laboratório", width = "100%"))
    ),
    textAreaInput("descricao_relatorio", "Descrição do experimento / observações", width = "100%", rows = 3, placeholder = "Ex.: avaliação de cor de frutos após sete dias de armazenamento..."),
    fluidRow(
      column(8, checkboxGroupInput("secoes_relatorio", "Incluir no PDF", inline = TRUE,
        choices = c("Gráficos" = "graficos", "Paleta de cores" = "paleta", "Tabela de coordenadas" = "tabela", "Diferença de cor (ΔE)" = "delta", "Repetições (se agrupado)" = "repeticoes"),
        selected = c("graficos", "paleta", "tabela", "delta", "repeticoes"))),
      column(4, div(class = "area-botao-pdf", downloadButton("baixar_pdf", "Baixar relatório PDF", icon = icon("download"), class = "btn-pdf")))
    ),
    div(class = "explicacao", textOutput("info_relatorio"))
  ),

  div(class = "rodape-app",
    span("Desenvolvido por"),
    tags$img(src = "img/logo_marlenildo.png", class = "logo-rodape", alt = "Marlenildo.online"),
    span(class = "versao-app",
      tags$a(href = "https://github.com/Marlenildo/croma/blob/main/CHANGELOG.md", target = "_blank", rel = "noopener",
             title = "Ver novidades desta versão", paste0("Croma v", VERSAO_APP)))
  )
)
