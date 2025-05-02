# Segurança Alimentar

Este repositório contém os scripts responsáveis por gerar as tabelas utilizadas nos resultados da sessão de **Consumo de Alimentos**, com foco específico na **segurança alimentar**. Os dados processados aqui têm como base:

- Os microdados da **Pesquisa de Orçamentos Familiares (POF)**  
- As edições da **Pesquisa Nacional por Amostra de Domicílios (PNAD)** de 2004, 2009 e 2013  
- A **PNAD Contínua (PNADC)** de 2023

---

### Pré-requisitos

Antes de executar os scripts desta pasta, é necessário que os scripts da pasta `01-etl-microdados-pof` tenham sido executados previamente, pois seus outputs são utilizados nesta etapa.

---

### Download dos dados

Os microdados da PNAD e da PNAD Contínua devem ser baixados manualmente a partir dos links abaixo:

- [PNAD 2004](https://drive.google.com/drive/folders/18ZNM_rLEoKDObjB_Puy5ifVLImu4x6J_?usp=drive_link)  
- [PNAD 2009](https://drive.google.com/drive/folders/1oEsSSU6eSFrnb-gsyEYAfeBM2FHBEDWt?usp=drive_link)  
- [PNAD 2013](https://drive.google.com/drive/folders/1xfLgsZadAGVkynyokbMVr31RWIK_HiyJ?usp=drive_link)  
- [PNADC 2023](https://drive.google.com/drive/folders/1kK31mVT9ZIhEx1DpOzkfFFecBIogdPhI?usp=drive_link)

Após o download, organize os arquivos extraídos nas seguintes pastas:

- `data/raw/PNAD/2004`
- `data/raw/PNAD/2009`
- `data/raw/PNAD/2013`
- `data/raw/PNADC`

Essa estrutura é essencial para que os scripts de leitura e transformação funcionem corretamente.

---

###  Scripts disponíveis

Os scripts estão organizados cronologicamente pelas edições da pesquisa:

- `1_seguranca_alimentar_pof_2018.R` — Processamento da segurança alimentar com base na POF 2017/2018  
- `2_seguranca_alimentar_pnad_2013.R` — Processamento da PNAD 2013  
- `3_seguranca_alimentar_pnad_2009.R` — Processamento da PNAD 2009  
- `4_seguranca_alimentar_pnad_2004.R` — Processamento da PNAD 2004  
- `5_seguranca_alimentar_pnadc_2023.R` — Processamento da PNAD Contínua 2023  
- `6_tabela_final.R` — Consolidação das bases e geração da tabela final para análise

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com