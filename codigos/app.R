# app_renomear.R
library(shiny)
library(shinyFiles)
library(dplyr)
library(tibble)

# Defina o caminho para a pasta Downloads do usuário
downloads_path <- "C:/Users/arauj/Downloads"

ui <- fluidPage(
  titlePanel("Renomear Arquivos - Adicionar Prefixo e Sufixo"),
  sidebarLayout(
    sidebarPanel(
      # Botão para selecionar a pasta
      shinyDirButton("pasta", "Selecionar Pasta", "Escolha uma pasta"),
      br(), br(),
      
      # Botões para selecionar todos / limpar seleção
      fluidRow(
        column(6, actionButton("select_all", "Selecionar todos", 
                               icon = icon("check-square"), width = "100%")),
        column(6, actionButton("clear_all", "Limpar seleção", 
                               icon = icon("square"), width = "100%"))
      ),
      br(),
      
      # Checkboxes para selecionar os arquivos da pasta
      uiOutput("file_checkboxes"),
      br(),
      
      # Campos para prefixo e sufixo
      textInput("prefixo", "Prefixo:", value = ""),
      textInput("sufixo", "Sufixo:", value = ""),
      
      # Botão para renomear
      actionButton("rename_btn", "Renomear Arquivos Selecionados", 
                   icon = icon("refresh"), class = "btn-primary"),
      br(), br(),
      
      # Status da operação
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Pré-visualização dos novos nomes:"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  
  # Definir as raízes que o usuário pode acessar
  roots <- c(
    Downloads = downloads_path,
    Home = path.expand("~"),
    WD = getwd()
  )
  
  # Configurar seleção de pasta
  shinyDirChoose(input, "pasta", roots = roots, session = session)
  
  # Reativo: caminho da pasta selecionada
  pasta_selecionada <- reactive({
    req(input$pasta)
    parseDirPath(roots, input$pasta)
  })
  
  # Reativo: lista de arquivos na pasta (apenas arquivos, não subpastas)
  arquivos <- reactive({
    path <- pasta_selecionada()
    if (is.null(path) || length(path) == 0 || !dir.exists(path)) {
      return(NULL)
    }
    files <- list.files(path, full.names = TRUE)
    files <- files[!file.info(files)$isdir]  # filtra apenas arquivos
    if (length(files) == 0) {
      return(NULL)
    }
    data.frame(
      caminho = files,
      nome = basename(files),
      stringsAsFactors = FALSE
    )
  })
  
  # Renderizar checkboxes para selecionar arquivos
  output$file_checkboxes <- renderUI({
    df <- arquivos()
    if (is.null(df)) {
      return(p("Nenhum arquivo encontrado na pasta selecionada."))
    }
    checkboxGroupInput("arquivos_selecionados", "Arquivos disponíveis:",
                       choices = setNames(df$caminho, df$nome))
  })
  
  # Observador para o botão "Selecionar todos"
  observeEvent(input$select_all, {
    df <- arquivos()
    if (!is.null(df)) {
      updateCheckboxGroupInput(session, "arquivos_selecionados",
                               selected = df$caminho)
    }
  })
  
  # Observador para o botão "Limpar seleção"
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "arquivos_selecionados",
                             selected = character(0))
  })
  
  # Reativo: dados para pré-visualização (com base nos arquivos selecionados)
  preview_data <- reactive({
    req(input$arquivos_selecionados)
    caminhos <- input$arquivos_selecionados
    if (length(caminhos) == 0) return(NULL)
    
    nomes_originais <- basename(caminhos)
    extensoes <- tools::file_ext(nomes_originais)
    nomes_base <- tools::file_path_sans_ext(nomes_originais)
    
    novos_nomes <- ifelse(extensoes == "",
                          paste0(input$prefixo, nomes_base, input$sufixo),
                          paste0(input$prefixo, nomes_base, input$sufixo, ".", extensoes))
    
    data.frame(
      Original = nomes_originais,
      Novo = novos_nomes,
      stringsAsFactors = FALSE
    )
  })
  
  # Renderizar tabela de pré-visualização
  output$preview <- renderTable({
    preview_data()
  })
  
  # Evento de renomear
  observeEvent(input$rename_btn, {
    req(input$arquivos_selecionados)
    df_prev <- preview_data()
    caminhos_originais <- input$arquivos_selecionados
    novos_caminhos <- file.path(dirname(caminhos_originais[1]), df_prev$Novo)
    
    # Verificar se algum novo nome já existe (evitar sobrescrita)
    existentes <- file.exists(novos_caminhos)
    if (any(existentes)) {
      conflitos <- df_prev$Novo[existentes]
      output$status <- renderPrint(paste(
        "Erro: O(s) seguinte(s) arquivo(s) já existe(m):",
        paste(conflitos, collapse = ", ")
      ))
      return()
    }
    
    # Executar renomeação
    sucessos <- 0
    erros <- c()
    for (i in seq_along(caminhos_originais)) {
      tryCatch({
        file.rename(caminhos_originais[i], novos_caminhos[i])
        sucessos <- sucessos + 1
      }, error = function(e) {
        erros <<- c(erros, basename(caminhos_originais[i]))
      })
    }
    
    # Mensagem de status
    msg <- paste(sucessos, "arquivo(s) renomeado(s) com sucesso.")
    if (length(erros) > 0) {
      msg <- paste0(msg, " Erro ao renomear: ", paste(erros, collapse = ", "))
    }
    output$status <- renderPrint(msg)
  })
}

=======
# app_renomear.R
library(shiny)
library(shinyFiles)
library(dplyr)
library(tibble)

# Defina o caminho para a pasta Downloads do usuário
downloads_path <- "C:/Users/arauj/Downloads"

ui <- fluidPage(
  titlePanel("Renomear Arquivos - Adicionar Prefixo e Sufixo"),
  sidebarLayout(
    sidebarPanel(
      # Botão para selecionar a pasta
      shinyDirButton("pasta", "Selecionar Pasta", "Escolha uma pasta"),
      br(), br(),
      
      # Botões para selecionar todos / limpar seleção
      fluidRow(
        column(6, actionButton("select_all", "Selecionar todos", 
                               icon = icon("check-square"), width = "100%")),
        column(6, actionButton("clear_all", "Limpar seleção", 
                               icon = icon("square"), width = "100%"))
      ),
      br(),
      
      # Checkboxes para selecionar os arquivos da pasta
      uiOutput("file_checkboxes"),
      br(),
      
      # Campos para prefixo e sufixo
      textInput("prefixo", "Prefixo:", value = ""),
      textInput("sufixo", "Sufixo:", value = ""),
      
      # Botão para renomear
      actionButton("rename_btn", "Renomear Arquivos Selecionados", 
                   icon = icon("refresh"), class = "btn-primary"),
      br(), br(),
      
      # Status da operação
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Pré-visualização dos novos nomes:"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  
  # Definir as raízes que o usuário pode acessar
  roots <- c(
    Downloads = downloads_path,
    Home = path.expand("~"),
    WD = getwd()
  )
  
  # Configurar seleção de pasta
  shinyDirChoose(input, "pasta", roots = roots, session = session)
  
  # Reativo: caminho da pasta selecionada
  pasta_selecionada <- reactive({
    req(input$pasta)
    parseDirPath(roots, input$pasta)
  })
  
  # Reativo: lista de arquivos na pasta (apenas arquivos, não subpastas)
  arquivos <- reactive({
    path <- pasta_selecionada()
    if (is.null(path) || length(path) == 0 || !dir.exists(path)) {
      return(NULL)
    }
    files <- list.files(path, full.names = TRUE)
    files <- files[!file.info(files)$isdir]  # filtra apenas arquivos
    if (length(files) == 0) {
      return(NULL)
    }
    data.frame(
      caminho = files,
      nome = basename(files),
      stringsAsFactors = FALSE
    )
  })
  
  # Renderizar checkboxes para selecionar arquivos
  output$file_checkboxes <- renderUI({
    df <- arquivos()
    if (is.null(df)) {
      return(p("Nenhum arquivo encontrado na pasta selecionada."))
    }
    checkboxGroupInput("arquivos_selecionados", "Arquivos disponíveis:",
                       choices = setNames(df$caminho, df$nome))
  })
  
  # Observador para o botão "Selecionar todos"
  observeEvent(input$select_all, {
    df <- arquivos()
    if (!is.null(df)) {
      updateCheckboxGroupInput(session, "arquivos_selecionados",
                               selected = df$caminho)
    }
  })
  
  # Observador para o botão "Limpar seleção"
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "arquivos_selecionados",
                             selected = character(0))
  })
  
  # Reativo: dados para pré-visualização (com base nos arquivos selecionados)
  preview_data <- reactive({
    req(input$arquivos_selecionados)
    caminhos <- input$arquivos_selecionados
    if (length(caminhos) == 0) return(NULL)
    
    nomes_originais <- basename(caminhos)
    extensoes <- tools::file_ext(nomes_originais)
    nomes_base <- tools::file_path_sans_ext(nomes_originais)
    
    novos_nomes <- ifelse(extensoes == "",
                          paste0(input$prefixo, nomes_base, input$sufixo),
                          paste0(input$prefixo, nomes_base, input$sufixo, ".", extensoes))
    
    data.frame(
      Original = nomes_originais,
      Novo = novos_nomes,
      stringsAsFactors = FALSE
    )
  })
  
  # Renderizar tabela de pré-visualização
  output$preview <- renderTable({
    preview_data()
  })
  
  # Evento de renomear
  observeEvent(input$rename_btn, {
    req(input$arquivos_selecionados)
    df_prev <- preview_data()
    caminhos_originais <- input$arquivos_selecionados
    novos_caminhos <- file.path(dirname(caminhos_originais[1]), df_prev$Novo)
    
    # Verificar se algum novo nome já existe (evitar sobrescrita)
    existentes <- file.exists(novos_caminhos)
    if (any(existentes)) {
      conflitos <- df_prev$Novo[existentes]
      output$status <- renderPrint(paste(
        "Erro: O(s) seguinte(s) arquivo(s) já existe(m):",
        paste(conflitos, collapse = ", ")
      ))
      return()
    }
    
    # Executar renomeação
    sucessos <- 0
    erros <- c()
    for (i in seq_along(caminhos_originais)) {
      tryCatch({
        file.rename(caminhos_originais[i], novos_caminhos[i])
        sucessos <- sucessos + 1
      }, error = function(e) {
        erros <<- c(erros, basename(caminhos_originais[i]))
      })
    }
    
    # Mensagem de status
    msg <- paste(sucessos, "arquivo(s) renomeado(s) com sucesso.")
    if (length(erros) > 0) {
      msg <- paste0(msg, " Erro ao renomear: ", paste(erros, collapse = ", "))
    }
    output$status <- renderPrint(msg)
  })
}

>>>>>>> 09dca4563fe39ef9140ca9eb9439dcbe99ff9362
shinyApp(ui, server)