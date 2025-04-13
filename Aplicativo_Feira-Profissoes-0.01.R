library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(bslib)
library(dplyr)
library(lubridate)
library(shinyFeedback)
library(tidyr)
library(ggridges)

# Inicializa dados vazios
dados_visitantes <- data.frame(
  Nome = character(),
  Idade = numeric(),
  Escola = character(),
  Genero = character(),
  Curso_Interesse = character(),
  Hora_Participacao = as.POSIXct(character()),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      .date-range-input .form-control {
        color: #2c3e50;
        background-color: #ecf0f1;
        border-color: #2c3e50;
      }
      .date-range-input label {
        color: #ecf0f1 !important;
      }
      #botao-estatistica {
        position: fixed;
        bottom: 30px;
        left: 30px;
        z-index: 9999;
        background-color: #3498db;
        color: white;
        border: none;
        border-radius: 50%;
        width: 60px;
        height: 60px;
        font-size: 28px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 4px 20px rgba(52, 152, 219, 0.5);
        transition: all 0.3s ease;
      }
      #botao-estatistica:hover {
        background-color: #2980b9;
        transform: scale(1.1);
      }
      #balon-estatistica {
        position: fixed;
        bottom: 110px;
        left: 6px;
        z-index: 9998;
        background-color: #34495e;
        border-radius: 12px;
        padding: 15px;
        border: 1px solid #2c3e50;
        box-shadow: 0 5px 25px rgba(0,0,0,0.3);
        display: none;
        max-width: 270px;
        animation: slideIn 0.3s ease;
        color: #ecf0f1;
      }
      @keyframes slideIn {
        from {opacity: 0; transform: translateY(20px);}
        to {opacity: 1; transform: translateY(0);}
      }
      .required-field::after {
        content: ' *';
        color: #e74c3c;
      }
      .shiny-output-error { 
        visibility: hidden; 
      }
      .shiny-output-error:before {
        visibility: visible;
        content: 'Dados insuficientes para gerar o gráfico. Adicione mais visitantes.';
        display: block;
        color: #e74c3c;
        font-weight: bold;
        padding: 10px;
        margin-bottom: 10px;
        background-color: rgba(231, 76, 60, 0.1);
        border-radius: 5px;
        border-left: 4px solid #e74c3c;
      }
      .nav-tabs {
        border-bottom: 1px solid #2c3e50;
      }
      .nav-tabs .nav-link.active {
        background-color: #34495e;
        color: #3498db;
        border-color: #2c3e50 #2c3e50 #34495e;
        font-weight: bold;
      }
      .nav-tabs .nav-link {
        color: #bdc3c7;
        transition: all 0.3s;
      }
      .nav-tabs .nav-link:hover {
        color: #3498db;
        border-color: #2c3e50 #2c3e50 #3498db;
      }
    "))
  ),
  
  theme = bs_theme(
    version = 5,
    bg = "#2c3e50",
    fg = "#ecf0f1",
    primary = "#3498db",
    secondary = "#e74c3c",
    success = "#2ecc71",
    info = "#9b59b6",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Poppins"),
    heading_font = font_google("Poppins"),
    bootswatch = "darkly"
  ),
  
  titlePanel(
    div("Feira de Profissões - Estatística IME/UFG 🎓",
        style = "font-size: 30px; text-align: center; color: #3498db; text-shadow: 0 2px 4px rgba(0,0,0,0.3);")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #34495e; padding: 20px; border-radius: 12px; box-shadow: 0 5px 15px rgba(0,0,0,0.2); border: 1px solid #2c3e50;",
      
      div(class = "required-field", textInput("nome", "Nome:", placeholder = "Digite seu nome...")),
      numericInput("idade", "Idade:", value = 18, min = 10, max = 100),
      
      selectizeInput(
        "escola",
        "Tipo de Escola:",
        choices = c("Pública", "Particular"),
        options = list(placeholder = 'Escolha ou digite...', create = TRUE)
      ),
      
      selectizeInput(
        "genero",
        "Gênero com o qual se identifica:",
        choices = c("Mulher", "Homem", "Pessoa não-binária", "Prefiro não informar", "Outro"),
        options = list(placeholder = 'Escolha ou digite...', create = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.genero == 'Outro'",
        textInput("genero_outro", "Especifique seu gênero:", placeholder = "Digite aqui...")
      ),
      
      div(class = "required-field", selectizeInput(
        "curso", 
        "Curso de Interesse:", 
        choices = c("Estatística", "Matemática", "Matemática Aplicada e Computacional", "Outro"),
        options = list(placeholder = 'Selecione um curso...')
      )),
      
      actionButton("adicionar", "Adicionar Visitante", 
                   class = "btn btn-primary",
                   style = "width: 100%; font-weight: bold; box-shadow: 0 2px 10px rgba(52, 152, 219, 0.4);")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela de Visitantes", DTOutput("tabela")),
        tabPanel("Distribuição por Idade", 
                 plotOutput("grafico_idade_escola")),
        tabPanel("Interesse nos Cursos", 
                 plotOutput("grafico_cursos_linha"),
                 plotOutput("grafico_escola_curso")),
        tabPanel("Análise Temporal", 
                 plotOutput("grafico_evolucao"),
                 plotOutput("grafico_horario"))
      )
    )
  ),
  
  tags$button(
    id = "botao-estatistica",
    class = "btn",
    HTML('📊')
  ),
  
  div(
    id = "balon-estatistica",
    h5("Ações e Filtros", style = "color: #3498db;"),
    actionButton("carregar_dados_btn", "📁 Carregar Dados", 
                 class = "btn btn-info btn-sm", 
                 style = "width: 100%; margin-bottom: 5px; font-weight: bold;"),
    actionButton("abrir_modal_baixar", "⬇️ Baixar Dados", 
                 class = "btn btn-success btn-sm", 
                 style = "width: 100%; margin-bottom: 10px; font-weight: bold;"),
    downloadButton("botao_real_baixar", label = "Baixar Dados", 
                   class = "btn btn-success btn-sm",
                   style = "width: 100%; margin-bottom: 10px; font-weight: bold;"),
    
    # No UI.R, altere os valores iniciais:
    dateRangeInput("data_range", "Período:", 
                   start = Sys.Date() - 30,  # Período maior
                   end = Sys.Date() + 1),     # Inclui hoje
    
    sliderInput("hora_range", "Horário (hora):", 
                min = 0, max = 23, value = c(0, 23))  # Todo o dia
  )
)

# SERVER
server <- function(input, output, session) {
  rv <- reactiveValues(data = dados_visitantes)
  
  # Mostrar/ocultar balão de estatísticas
  observe({
    shinyjs::onclick("botao-estatistica", {
      shinyjs::toggle("balon-estatistica")
    })
  })
  
  # Validação dos campos
  observe({
    if(is.null(input$nome) || input$nome == "") {
      showFeedbackDanger("nome", "Campo obrigatório")
    } else {
      hideFeedback("nome")
    }
    
    if(is.null(input$curso) || input$curso == "") {
      showFeedbackDanger("curso", "Campo obrigatório")
    } else {
      hideFeedback("curso")
    }
    
    if(input$genero == "Outro" && (is.null(input$genero_outro) || input$genero_outro == "")) {
      showFeedback("genero_outro", "Por favor especifique", color = "#f39c12")
    } else {
      hideFeedback("genero_outro")
    }
  })
  
  # Adicionar novo visitante
  observeEvent(input$adicionar, {
    req(input$nome, input$curso)
    
    if(input$nome == "" || input$curso == "") {
      showNotification("Preencha todos os campos obrigatórios", type = "error")
      return()
    }
    
    if(input$genero == "Outro" && (is.null(input$genero_outro) || input$genero_outro == "")) {
      showNotification("Por favor, especifique seu gênero", type = "error")
      return()
    }
    
    novo_dado <- data.frame(
      Nome = input$nome,
      Idade = input$idade,
      Escola = ifelse(is.null(input$escola) || input$escola == "", NA, input$escola),
      Genero = ifelse(input$genero == "Outro", input$genero_outro, input$genero),
      Curso_Interesse = input$curso,
      Hora_Participacao = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    rv$data <- bind_rows(rv$data, novo_dado)
    
    # Salvar dados em CSV
    tryCatch({
      write.csv(rv$data, "dados_visitantes.csv", row.names = FALSE)
      showNotification("Visitante adicionado com sucesso!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erro ao salvar dados:", e$message), type = "error")
    })
    
    # Limpar campos
    updateTextInput(session, "nome", value = "")
    updateNumericInput(session, "idade", value = 18)
    updateSelectizeInput(session, "escola", selected = "")
    updateSelectizeInput(session, "genero", selected = "")
    updateTextInput(session, "genero_outro", value = "")
    updateSelectizeInput(session, "curso", selected = "")
  })
  
  # Carregar dados
  observeEvent(input$carregar_dados_btn, {
    showModal(modalDialog(
      title = "Digite a Senha para Carregar os Dados",
      passwordInput("senha_carregar", "Senha:"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_carregar", "Confirmar", class = "btn btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirmar_carregar, {
    if(input$senha_carregar == "receba") {
      removeModal()
      showModal(modalDialog(
        title = "Carregar Dados",
        fileInput("file_upload", "Selecione o arquivo CSV",
                  accept = c(".csv")),
        footer = modalButton("Fechar")
      ))
    } else {
      showNotification("Senha incorreta!", type = "error")
    }
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      dados <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      
      if(nrow(dados) == 0) {
        showNotification("O arquivo está vazio!", type = "error")
        return()
      }
      
      required_columns <- c("Nome", "Idade", "Escola", "Genero", "Curso_Interesse", "Hora_Participacao")
      if(!all(required_columns %in% colnames(dados))) {
        showNotification("Arquivo não contém todas as colunas necessárias.", type = "error")
        return()
      }
      
      if(is.character(dados$Hora_Participacao)) {
        formatos <- c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S")
        
        for(fmt in formatos) {
          converted <- as.POSIXct(dados$Hora_Participacao, format = fmt, tz = "UTC")
          if(!any(is.na(converted))) {
            dados$Hora_Participacao <- converted
            break
          }
        }
        
        if(is.character(dados$Hora_Participacao)) {
          showNotification("Formato de data/hora não reconhecido no arquivo.", type = "error")
          return()
        }
      }
      
      rv$data <- dados
      
      datas <- as.Date(dados$Hora_Participacao)
      updateDateRangeInput(session, "data_range",
                           start = min(datas),
                           end = max(datas))
      
      showNotification("Dados carregados com sucesso!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erro ao carregar arquivo:", e$message), type = "error")
    })
  })
  
  # Remove o modal de senha e usa o botão diretamente
  # Modal de senha (opcional)
  observeEvent(input$abrir_modal_baixar, {
    showModal(modalDialog(
      title = "Digite a Senha para Baixar os Dados",
      passwordInput("senha_baixar", "Senha:"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_baixar", "Confirmar", class = "btn btn-success")
      )
    ))
  })
  
  # Validação da senha
  observeEvent(input$confirmar_baixar, {
    if(input$senha_baixar == "tchau") {
      removeModal()
      shinyjs::click("botao_real_baixar")
    } else {
      showNotification("Senha incorreta!", type = "error")
    }
  })
  # Handler de download corrigido
  output$botao_real_baixar <- downloadHandler(
    filename = function() {
      paste0("dados_visitantes_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(nrow(rv$data) > 0) # Garante que há dados
      
      # Formata as colunas de data/hora
      dados_export <- rv$data %>%
        mutate(Hora_Participacao = format(Hora_Participacao, "%Y-%m-%d %H:%M:%S"))
      
      # Escreve o arquivo CSV
      write.csv(dados_export, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  # Dados filtrados
  dados_filtrados <- reactive({
    req(nrow(rv$data) > 0)
    
    dados <- rv$data %>%
      mutate(
        Data = as.Date(Hora_Participacao),
        Hora = hour(Hora_Participacao)
      ) %>%
      filter(
        between(Data, input$data_range[1], input$data_range[2]),
        between(Hora, input$hora_range[1], input$hora_range[2])
      )
    
    dados
  })
  
  # Tabela de visitantes
  output$tabela <- renderDT({
    req(nrow(dados_filtrados()) > 0)
    
    # Formata Hora_Participacao (MM/DD HH:MM) e remove colunas Data/Hora
    dados_formatados <- dados_filtrados() %>%
      mutate(Hora_Participacao = format(Hora_Participacao, "%m/%d %H:%M")) %>%
      select(-Data, -Hora)  # Remove as colunas
    
    datatable(
      dados_formatados,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      style = "bootstrap"
    )
  })
  # 1. Gráfico de densidade por idade, escola e gênero
  output$grafico_idade_escola <- renderPlot({
    req(nrow(dados_filtrados()) > 0)
    
    ggplot(dados_filtrados(), aes(x = Idade, y = Genero, fill = Escola)) +
      geom_density_ridges(alpha = 0.7, scale = 0.9) +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(title = "Distribuição de Idade por Gênero e Tipo de Escola",
           x = "Idade",
           y = "Gênero",
           fill = "Tipo de Escola") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c3e50", color = NA),
        panel.background = element_rect(fill = "#2c3e50", color = NA),
        text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#bdc3c7"),
        legend.background = element_rect(fill = "#34495e"),
        legend.text = element_text(color = "#ecf0f1")
      )
  })
  
  # 2. Gráfico de linhas para cursos interessados (evolução temporal)
  output$grafico_cursos_linha <- renderPlot({
    req(nrow(dados_filtrados()) > 0)
    
    dados_curso <- dados_filtrados() %>%
      mutate(Dia = as.Date(Hora_Participacao)) %>%
      count(Dia, Curso_Interesse)
    
    ggplot(dados_curso, aes(x = Dia, y = n, color = Curso_Interesse, group = Curso_Interesse)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Set2") +
      labs(title = "Evolução do Interesse nos Cursos por Dia",
           x = "Data",
           y = "Número de Visitantes",
           color = "Curso") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c3e50", color = NA),
        panel.background = element_rect(fill = "#2c3e50", color = NA),
        text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#bdc3c7"),
        legend.background = element_rect(fill = "#34495e"),
        legend.text = element_text(color = "#ecf0f1")
      )
  })
  
  # 3. Gráfico de barras para escola vs curso
  output$grafico_escola_curso <- renderPlot({
    req(nrow(dados_filtrados()) > 0)
    
    ggplot(dados_filtrados(), aes(x = Escola, fill = Curso_Interesse)) +
      geom_bar(position = "dodge") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Distribuição de Cursos por Tipo de Escola",
           x = "Tipo de Escola",
           y = "Número de Visitantes",
           fill = "Curso") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c3e50", color = NA),
        panel.background = element_rect(fill = "#2c3e50", color = NA),
        text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#bdc3c7"),
        legend.background = element_rect(fill = "#34495e"),
        legend.text = element_text(color = "#ecf0f1")
      )
  })
  
  # 4. Gráfico de evolução temporal
  output$grafico_evolucao <- renderPlot({
    req(nrow(dados_filtrados()) > 0)
    
    dados_evolucao <- dados_filtrados() %>%
      group_by(Data) %>%
      summarise(Total = n())
    
    ggplot(dados_evolucao, aes(x = Data, y = Total, group = 1)) +
      geom_line(color = "#9b59b6", size = 1.5) +
      geom_point(color = "#9b59b6", size = 3) +
      labs(title = "Evolução de Participantes ao Longo do Tempo", 
           x = "Data", 
           y = "Número de Visitantes") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c3e50", color = NA),
        panel.background = element_rect(fill = "#2c3e50", color = NA),
        text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#bdc3c7")
      )
  })
  
  # 5. Gráfico de horário
  output$grafico_horario <- renderPlot({
    req(nrow(dados_filtrados()) > 0)
    
    ggplot(dados_filtrados(), aes(x = Hora, fill = Genero)) +
      geom_density(alpha = 0.7) +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Distribuição por Horário e Gênero", 
           x = "Hora do Dia", 
           y = "Densidade",
           fill = "Gênero") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2c3e50", color = NA),
        panel.background = element_rect(fill = "#2c3e50", color = NA),
        text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#bdc3c7"),
        legend.background = element_rect(fill = "#34495e"),
        legend.text = element_text(color = "#ecf0f1")
      )
  })
}

shinyApp(ui = ui, server = server)
