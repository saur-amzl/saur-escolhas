# Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# --- 1. Criar a base original (no formato wide) ---
dados_wide <- tribble(
  ~cnae_subclasse, ~descricao_cnae_subclasse, ~AC, ~AM, ~MA, ~MT, ~AP, ~PA, ~RO, ~RR, ~TO,
  "47.11-3/01", "Com. varej. merc. em geral - hipermercados", "in natura", "in natura", "in natura", "mistos", "", "", "", "", "",
  "47.11-3/02", "Com. varej. merc. em geral - supermercados", "in natura", "in natura", "in natura", "mistos", "in natura", "in natura", "in natura", "in natura", "in natura",
  "47.12-1/00", "Minimercados, mercearias e armazéns", "in natura", "in natura", "in natura", "mistos", "in natura", "in natura", "in natura", "in natura", "in natura",
  "47.21-1/02", "Padaria e confeitaria (revenda)", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "ultraprocessado",
  "47.21-1/03", "Laticínios e frios", "", "in natura", "in natura", "ultraprocessado", "in natura", "ultraprocessado", "in natura", "", "",
  "47.21-1/04", "Doces, balas, bombons", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "", "",
  "47.22-9/01", "Carnes - açougues", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura",
  "47.22-9/02", "Peixaria", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura",
  "47.24-5/00", "Hortifrutigranjeiros", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura", "in natura",
  "47.29-6/02", "Lojas de conveniência", "in natura", "in natura", "mistos", "ultraprocessado", "", "ultraprocessado", "mistos", "mistos", "ultraprocessado",
  "47.29-6/99", "Produtos alimentícios diversos", "in natura", "in natura", "in natura", "mistos", "in natura", "in natura", "in natura", "mistos", "mistos",
  "56.11-2/01", "Restaurantes e similares", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos",
  "56.11-2/03", "Lanchonetes e casas de sucos", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "ultraprocessado", "mistos", "ultraprocessado", "ultraprocessado", "ultraprocessado",
  "56.11-2/04", "Bares sem entretenimento", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos",
  "56.11-2/05", "Bares com entretenimento", "mistos", "mistos", "mistos", "ultraprocessado", "mistos", "mistos", "mistos", "", "",
  "56.12-1/00", "Serviços ambulantes", "mistos", "in natura", "in natura", "mistos", "in natura", "in natura", "ultraprocessado", "mistos", "mistos",
  "56.20-1/03", "Cantinas (alimentação privativa)", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "mistos", "ultraprocessado", "mistos",
  "56.20-1/04", "Fornecimento de alimentos p/ consumo domiciliar", "", "in natura", "in natura", "mistos", "in natura", "mistos", "", "in natura", "in natura"
)

# --- 2. Transformar para formato longo e manter a ordem original dos CNAEs ---
dados_long <- dados_wide %>%
  pivot_longer(cols = AC:TO, names_to = "estado", values_to = "categoria") %>%
  filter(categoria != "") %>%
  mutate(
    cnae_label = paste(cnae_subclasse, descricao_cnae_subclasse, sep = " - "),
    cnae_label = factor(cnae_label, levels = unique(paste(cnae_subclasse, descricao_cnae_subclasse, sep = " - ")))
  )

# --- 3. Plotar o heatmap com a ordem original ---
gf <- ggplot(dados_long, aes(x = estado, y = cnae_label, fill = categoria)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "in natura" = "#4CAF50",
      "mistos" = "#FFC107",
      "ultraprocessado" = "#F44336"
    ),
    na.value = "grey90"
  ) +
  labs(
    x = "",
    y = "Subclasse CNAE",
    fill = "Categoria"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),                        # remove grid
    panel.background = element_rect(fill = "transparent", color = NA), # fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA),  # fundo total
    legend.background = element_rect(fill = "transparent"),            # fundo da legenda
    legend.box.background = element_rect(fill = "transparent"),        # fundo da caixa da legenda
    axis.text.x = element_text(hjust = 1),
    axis.text.y = element_text(size = 12)
  )

ggsave("grafico_cnae_estado.png", gf, width = 12, height = 8, dpi = 300)
