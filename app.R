library(shiny)
library(ggplot2)

ui <- fluidPage(
	titlePanel("Curva Epidêmica"),
	
	sidebarLayout(
		sidebarPanel(
			width = 4,
			fluidRow(
				column(
					6,
					numericInput("n_dias", "Número de dias:", value = 10, min = 1, max = 60, step = 1)
				),
				column(
					6,
					dateInput("data_inicio", "Data inicial:", value = Sys.Date() - 9, format = "dd/mm/yyyy", language = "pt-BR")
				)
			),
			hr(),
			strong("Número de casos por dia:"),
			br(), br(),
			uiOutput("inputs_dias")
		),
		
		mainPanel(
			width = 8,
			plotOutput("curva_epidemica", height = "520px")
		)
	)
)

server <- function(input, output) {
	
	output$inputs_dias <- renderUI({
		req(input$n_dias, input$data_inicio)
		n <- max(1, min(60, as.integer(input$n_dias)))
		padrao <- c(1, 2, 5, 8, 6, 4, 3, 2, 1, 0)
		
		linhas <- lapply(seq(1, n, by = 2), function(i) {
			cols <- lapply(i:min(i + 1, n), function(idx) {
				id <- paste0("dia_", idx)
				val_existente <- isolate(input[[id]])
				val <- if (!is.null(val_existente)) {
					val_existente
				} else if (idx <= length(padrao)) {
					padrao[idx]
				} else {
					0
				}
				
				rotulo <- format(input$data_inicio + (idx - 1), "%d/%m")
				column(
					6,
					numericInput(id, label = rotulo, value = val, min = 0, step = 1)
				)
			})
			fluidRow(cols)
		})
		
		do.call(tagList, linhas)
	})
	
	dados_curva <- reactive({
		req(input$n_dias, input$data_inicio)
		n <- max(1, min(60, as.integer(input$n_dias)))
		
		casos <- sapply(seq_len(n), function(i) {
			val <- input[[paste0("dia_", i)]]
			if (is.null(val) || is.na(val) || val < 0) 0L else as.integer(val)
		})
		
		data.frame(
			dia = seq_len(n),
			label = format(input$data_inicio + (seq_len(n) - 1), "%d/%m"),
			casos = casos,
			stringsAsFactors = FALSE
		)
	})
	
	output$curva_epidemica <- renderPlot({
		df_dias <- dados_curva()
		n <- nrow(df_dias)
		
		lista_quadrados <- lapply(seq_len(n), function(i) {
			k <- df_dias$casos[i]
			if (k > 0) {
				data.frame(
					dia = df_dias$dia[i],
					y = seq_len(k) - 0.5
				)
			} else {
				NULL
			}
		})
		
		df_quadrados <- do.call(rbind, lista_quadrados)
		max_casos <- max(c(df_dias$casos, 5), na.rm = TRUE)
		
		quebras_y <- if (max_casos <= 20) {
			0:max_casos
		} else {
			unique(round(pretty(c(0, max_casos))))
		}
		
		p <- ggplot() +
			scale_x_continuous(
				breaks = df_dias$dia,
				labels = df_dias$label,
				limits = c(0.4, n + 0.6),
				expand = c(0, 0)
			) +
			scale_y_continuous(
				breaks = quebras_y,
				limits = c(0, max_casos + 0.5),
				expand = c(0, 0)
			) +
			coord_fixed(ratio = 1) +
			labs(
				title = "Curva Epidêmica",
				x = "Data de início dos sintomas",
				y = "Número de casos"
			) +
			theme_minimal(base_size = 13) +
			theme(
				panel.grid.major.x = element_blank(),
				panel.grid.minor = element_blank(),
				axis.line = element_line(color = "#2C3E50", linewidth = 0.6),
				axis.ticks = element_line(color = "#2C3E50"),
				axis.text.x = element_text(angle = if (n > 12) 45 else 0, hjust = if (n > 12) 1 else 0.5),
				plot.title = element_text(face = "bold")
			)
		
		if (!is.null(df_quadrados) && nrow(df_quadrados) > 0) {
			p <- p + geom_tile(
				data = df_quadrados,
				aes(x = dia, y = y),
				width = 0.92,
				height = 0.92,
				fill = "#1F618D",
				color = "white",
				linewidth = 0.8
			)
		}
		
		p
	})
}

shinyApp(ui = ui, server = server)
