# Carregar os pacotes Shiny, shinyWidgets e shinythemes
library(shiny)
library(shinyWidgets)
library(shinythemes)

# --- UI (Interface do Usuário) ---
# Define a aparência e o layout do aplicativo
ui <- fluidPage(theme = shinytheme("superhero"), # Alterado para o tema "superhero"
                # Adiciona CSS customizado para centralizar o título e arredondar os cantos
                tags$head(
                  tags$style(HTML("
      /* Centraliza o elemento h2, que é usado pelo titlePanel */
      h2 {
        text-align: center;
      }
      
      /* Deixa os cantos dos painéis, caixas de texto e entradas mais arredondados */
      .well, .shiny-text-output, .form-control {
        border-radius: 12px;
      }
    "))
                ),
                # Título do Aplicativo
                titlePanel("Simulador de Contrato de Resseguro Quota Share"),
                
                # Adiciona um parágrafo explicativo sobre o contrato Quota Share
                p("No contrato de resseguro Quota Share (QS), a seguradora cede uma porcentagem fixa de todos os prêmios e sinistros de uma carteira para o ressegurador. Em troca, o ressegurador geralmente paga uma comissão para cobrir parte dos custos de aquisição da seguradora."),
                
                # Layout com uma barra lateral para entradas e uma área principal para saídas
                sidebarLayout(
                  # Painel da barra lateral para as entradas do usuário
                  sidebarPanel(
                    h4("Entradas da Carteira"),
                    # Entrada monetária para o montante de prêmio
                    autonumericInput(
                      inputId = "premio",
                      label = "Prêmio Total da Carteira (R$):",
                      value = 1000000,
                      decimalCharacter = ",",
                      digitGroupSeparator = "."
                    ),
                    
                    # Entrada monetária para o montante de sinistro
                    autonumericInput(
                      inputId = "sinistro",
                      label = "Sinistro Total da Carteira (R$):",
                      value = 600000,
                      decimalCharacter = ",",
                      digitGroupSeparator = "."
                    ),
                    
                    hr(), # Adiciona uma linha horizontal para separação visual
                    
                    h4("Parâmetros do Contrato"),
                    # Controle deslizante para o percentual de cessão
                    sliderInput("cessao", "Percentual de Cessão (%):",
                                min = 0, max = 100, value = 50, step = 1),
                    
                    # Controle deslizante para a comissão de resseguro
                    sliderInput("comissao", "Comissão de Resseguro (%):",
                                min = 0, max = 100, value = 20, step = 1)
                  ),
                  
                  # Painel principal para exibir os resultados calculados
                  mainPanel(
                    h3("Resultados do Contrato de Resseguro"),
                    # Saída para a sinistralidade da carteira original
                    h4("Sinistralidade da Carteira Original:"),
                    verbatimTextOutput("sinistralidade_original"),
                    
                    hr(),
                    
                    h4("Para a Seguradora:"),
                    # Saída para o prêmio retido
                    verbatimTextOutput("premio_retido"),
                    # Saída para a receita final da seguradora
                    verbatimTextOutput("receita_final_seguradora"),
                    # Saída para o sinistro retido
                    verbatimTextOutput("sinistro_retido"),
                    # Saída para o resultado do contrato para a seguradora
                    verbatimTextOutput("resultado_seguradora"),
                    
                    hr(),
                    
                    h4("Para o Ressegurador:"),
                    # Saída para o valor do prêmio quota share (bruto)
                    verbatimTextOutput("premio_quota"),
                    # Saída para o valor da comissão de resseguro
                    verbatimTextOutput("comissao_resseguro"),
                    # Saída para o valor do prêmio cedido (líquido)
                    verbatimTextOutput("premio_cedido"),
                    # Saída para o valor do sinistro recuperado (assumido pelo ressegurador)
                    verbatimTextOutput("sinistro_recuperado"),
                    # Saída para o resultado final do contrato para o ressegurador
                    verbatimTextOutput("resultado_contrato")
                  )
                )
)

# --- Server (Lógica do Aplicativo) ---
# Define como as entradas são processadas e as saídas são geradas
server <- function(input, output) {
  
  # --- Cálculos Reativos ---
  # Estas expressões são recalculadas automaticamente sempre que uma entrada muda
  
  # Calcula o prêmio bruto que é cedido ao ressegurador
  premio_cedido_bruto_calc <- reactive({
    req(input$premio, input$cessao) # Garante que os valores de entrada existam
    input$premio * (input$cessao / 100)
  })
  
  # Calcula o valor do sinistro que é recuperado do ressegurador
  sinistro_recuperado_calc <- reactive({
    req(input$sinistro, input$cessao) # Garante que os valores de entrada existam
    input$sinistro * (input$cessao / 100)
  })
  
  # Calcula o valor da comissão de resseguro a ser paga pela resseguradora
  comissao_valor_calc <- reactive({
    req(input$comissao) # Garante que o valor de entrada exista
    premio_cedido_bruto_calc() * (input$comissao / 100)
  })
  
  # Calcula o prêmio cedido líquido, após a comissão
  premio_cedido_liquido_calc <- reactive({
    premio_cedido_bruto_calc() - comissao_valor_calc()
  })
  
  # Calcula o resultado do contrato para o ressegurador
  # Resultado = (Prêmio Recebido Líquido) - (Sinistros Pagos)
  resultado_calc <- reactive({
    premio_cedido_liquido_calc() - sinistro_recuperado_calc()
  })
  
  # Calcula o prêmio retido pela seguradora
  premio_retido_calc <- reactive({
    req(input$premio) # Garante que o valor de entrada exista
    input$premio - premio_cedido_bruto_calc()
  })
  
  # Calcula o sinistro retido pela seguradora
  sinistro_retido_calc <- reactive({
    req(input$sinistro) # Garante que os valores de entrada exista
    input$sinistro - sinistro_recuperado_calc()
  })
  
  # Calcula a receita final da seguradora (prêmio retido + comissão recebida)
  receita_final_seguradora_calc <- reactive({
    premio_retido_calc() + comissao_valor_calc()
  })
  
  # Calcula o resultado do contrato para a seguradora
  resultado_seguradora_calc <- reactive({
    receita_final_seguradora_calc() - sinistro_retido_calc()
  })
  
  # Calcula a sinistralidade da carteira original (Sinistro / Prêmio)
  sinistralidade_original_calc <- reactive({
    req(input$premio, input$sinistro) # Garante que os valores de entrada existam
    # Evita divisão por zero se o prêmio for 0
    if (input$premio == 0) {
      return(0)
    }
    (input$sinistro / input$premio)
  })
  
  # --- Geração das Saídas ---
  # Formata e renderiza os resultados para serem exibidos na UI
  
  # Exibe a sinistralidade da carteira
  output$sinistralidade_original <- renderText({
    paste0(format(sinistralidade_original_calc() * 100, nsmall = 2, big.mark = ".", decimal.mark = ","), "%")
  })
  
  # Exibe o prêmio retido pela seguradora
  output$premio_retido <- renderText({
    paste("Prêmio Retido:",
          "R$", format(premio_retido_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe a receita final da seguradora
  output$receita_final_seguradora <- renderText({
    paste("Receita Final (Prêmio Retido + Comissão):",
          "R$", format(receita_final_seguradora_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe o sinistro retido pela seguradora
  output$sinistro_retido <- renderText({
    paste("Sinistro Retido:",
          "R$", format(sinistro_retido_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe o resultado do contrato para a seguradora
  output$resultado_seguradora <- renderText({
    resultado <- resultado_seguradora_calc()
    prefixo <- ifelse(resultado >= 0, "Lucro do Contrato:", "Prejuízo do Contrato:")
    
    paste(prefixo, 
          "R$", format(resultado, nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          "\n(Receitas: Prêmio Retido + Comissão | Despesas: Sinistro Retido)")
  })
  
  # Exibe o prêmio quota share (bruto)
  output$premio_quota <- renderText({
    paste("Prêmio Quota Share (Bruto):",
          "R$", format(premio_cedido_bruto_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe a comissão de resseguro como um campo separado
  output$comissao_resseguro <- renderText({
    paste("Comissão de Resseguro (Despesa):",
          "R$", format(comissao_valor_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe o prêmio cedido (líquido)
  output$premio_cedido <- renderText({
    paste("Prêmio Cedido (Líquido):",
          "R$", format(premio_cedido_liquido_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe a recuperação de sinistro
  output$sinistro_recuperado <- renderText({
    paste("Sinistro Recuperado (Despesa):", 
          "R$", format(sinistro_recuperado_calc(), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  # Exibe o resultado do contrato
  output$resultado_contrato <- renderText({
    resultado <- resultado_calc()
    # Adiciona uma lógica de cor simples para o resultado (positivo ou negativo)
    prefixo <- ifelse(resultado >= 0, "Lucro do Contrato:", "Prejuízo do Contrato:")
    
    paste(prefixo, 
          "R$", format(resultado, nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          "\n(Receitas: Prêmio Cedido Líquido | Despesas: Sinistro Recuperado)")
  })
}

# --- Executar o Aplicativo ---
# Combina a UI e a lógica do servidor para iniciar o aplicativo Shiny
shinyApp(ui = ui, server = server)

