# Estabelecimentos Agropecuários (CNEFE) na Amazônia Legal


Este repositório contém os scripts responsáveis por analisar dados de Estabelecimentos Agropecuários do 
**Cadastro Nacional de Endereços para Fins Estatísticos (CNEFE)** do Censo Demográfico de 2022.

---

### Download dos dados

Os microdados da RAIS devem ser baixados manualmente a partir do link abaixo:

- [CNEFE (Censo 2022)](https://drive.google.com/file/d/1udKgw-KGxMOF5LPFXiitviCK-e7MBTXp/view?usp=sharing)
- [Município IBGE] (https://drive.google.com/drive/folders/1fTWpmhzX7Bp3h5W_O--4I0e50Uwl0X8U?usp=drive_link)
- [Áreas Urbanizadas IBGE] (https://drive.google.com/drive/folders/1F69XxwjhoDYAzA8YziSTJ0YS2W6S7ae3?usp=sharing)
- [Uso do Solo - Mapbiomas Col. 9 - 2023]  (https://drive.google.com/file/d/1tx5qJWbRw0ttozAYetGtdbIgsPwRT2Gl/view?usp=drive_link)

Após o download, adicione o(s) arquivo(s) na pasta `data/raw/`. Essa estrutura é essencial para que os scripts de leitura e transformação funcionem corretamente.

---

### Ambiente e pacotes

Os scripts estão escritos na linguagem **R**.
Todos os pacotes necessários estão listados no início de cada script. Certifique-se de instalá-los previamente para garantir a execução correta.

---

###  Scripts

Os scripts devem ser executados na seguinte ordem:

- `1_preparacao_dados.sh` — Realiza o pré-processamento dos dados de raster, alinhamento e reprojeção dos dados brutos de municípios e uso do solo.
- `2_contagem_pixel.R` — Conta os pixels para cada tipo de uso do solo e área urbana/rural, utilizando rasters de alta resolução. Os dados são salvos em um banco de dados SQLite.
- `3_sumarizacao.R` — Processa os dados do banco SQLite, agrupa as informações por estado e município, e gera tabelas com as estatísticas do Censo Agropecuário, exportando os resultados para uma planilha Excel.

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com