# Estabelecimentos Agropecuários (CNEFE) na Amazônia Legal


Este repositório contém os scripts responsáveis por analisar dados do Censo Agropecuário (CNEFE) de 2022. Os dados processados são provenientes de informações geoespaciais e de uso do solo, com foco em municípios da Amazônia Legal e suas respectivas regiões metropolitanas e imediatas.

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