# Changelog

Todas as mudanças relevantes do Croma são registradas aqui.

O formato segue o [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/)
e o projeto usa [Versionamento Semântico](https://semver.org/lang/pt-BR/):
`MAIOR.MENOR.CORREÇÃO`.

## [Não lançado]

### Adicionado

- `manifest.json` para publicação no Posit Connect Cloud (gerado com
  `rsconnect::writeManifest()`).
- Script do Google AdSense e `ads.txt` para monetização do app.

### Alterado

- Assinatura do autor no rodapé (e nos relatórios) passa a usar a nova logo
  Marlenildo.online, a mesma do site, sem o slogan "Soluções em Curso".

## [1.0.0] - 2026-09-26

Primeira versão pública, com o nome **Croma**.

### Adicionado

- Identidade visual: nome **Croma**, logo própria (anel de matiz CIELCH com o
  plano a\*b\*), favicon e logo do autor no rodapé do app e do PDF.
- Seção **Visualização das cores**, atualizada em tempo real:
  - paleta com cartões de cor, valores L\*, a\*, b\*, C\*, h° e nome aproximado
    da tonalidade;
  - plano cromático a\* × b\* com fundo colorido e círculos de C\*;
  - gráfico L\* × C\* (luminosidade × cromaticidade);
  - resumo com cor média, L\* médio ± dp, C\* médio e h° da cor média;
  - aviso de cores fora do gamut sRGB.
- **Agrupamento de repetições**:
  - grupo identificado pelo nome sem o número da repetição (`Manga A R1` →
    `Manga A`), por nome idêntico ou por uma coluna `Grupo` na importação;
  - valor do grupo por média ou mediana, com desvio padrão;
  - homogeneidade das repetições (ΔE₀₀ médio até o centro do grupo);
  - sinalização de repetições possivelmente atípicas;
  - aba **Repetições** e repetições exibidas nos gráficos.
- **Diferença de cor** em relação a uma referência: ΔL\*, Δa\*, Δb\*, ΔC\*,
  ΔH\*, ΔE\*ab e ΔE₀₀ (CIEDE2000), com classificação da diferença.
- **Relatório em PDF** reformulado (A4 paisagem): resumo, gráficos, paleta,
  tabela de coordenadas ou de grupos, ΔE e repetições; seções selecionáveis;
  campo de responsável; nome do arquivo baseado no título.
- Detecção automática de repetições ao importar dados.
- Versão do app exibida no rodapé do app e do PDF.
- `README.md`, `CHANGELOG.md`, `LICENSE` (MIT), `DESCRIPTION` e
  `CITATION.cff`.

### Alterado

- Fonte dos dados da visualização e do PDF acompanha a aba ativa
  (amostras editadas ou dados importados).

### Corrigido

- Texto das opções (botões de rádio e caixas de seleção) ficava cortado pelo
  próprio controle.
- Erro na referência de ΔE quando a sessão iniciava sem referência escolhida.

## [0.1.0] - 2026-09-26

Versão inicial (antes do nome Croma).

### Adicionado

- Edição de amostras linha a linha em CIELAB ou CIELCH, com conversão
  automática entre os sistemas.
- Importação de dados colados do Excel/planilha (vírgula decimal aceita) e
  edição na tabela importada.
- Pré-visualização da cor de cada amostra.
- Relatório em PDF com tabela de coordenadas e cores.

[Não lançado]: https://github.com/Marlenildo/croma/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/Marlenildo/croma/releases/tag/v1.0.0
