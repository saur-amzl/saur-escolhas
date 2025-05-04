# Comercialização de Alimentos

Este repositório contém os scripts responsáveis por gerar as tabelas utilizadas nos resultados da sessão de **Comercialização de Alimentos**. Os dados processados aqui têm como base:

- Os microdados da **Pesquisa de Orçamentos Familiares (POF)** de 2017/2018  
- Dados da **Relação Anual de Informações Sociais (RAIS)** de 2023  

---

### Pré-requisitos

Antes de executar os scripts desta pasta, é necessário que os scripts das pastas `01-etl-microdados-pof` e `02-consumo-de-alimentos` tenham sido executados previamente, pois seus outputs são utilizados nesta etapa.

---

### Download dos dados

Os microdados da RAIS devem ser baixados manualmente a partir do link abaixo:

- [RAIS](https://drive.google.com/drive/folders/1sWHNUGiJG17KR9IRiNNCmjnCxY7ku6Hv?usp=drive_link)  

Após o download, adicione o(s) arquivo(s) na pasta `data/raw/RAIS`. Essa estrutura é essencial para que os scripts de leitura e transformação funcionem corretamente.

---

###  Scripts disponíveis

Os scripts devem ser executados sequencialmente:

- `1_dados_rais.R` — Processa os dados da RAIS, filtrando estabelecimentos ativos dos setores de interesse (CNAEs), localizados na Amazônia Legal e em regiões selecionadas (RMs e RIs). Gera tabelas com o número de estabelecimentos e vínculos empregatícios por município e por estado.
- `2_locais_aquisicao_pof.R` — Processa os microdados da POF 2017-2018, com foco nos locais de aquisição de alimentos. Integra bases de consumo alimentar, mapeamentos de produtos e classificações dos locais, gerando uma base analítica por unidade de consumo (UC), detalhada por tipo de produto e local de compra.
- `3_classificacao.R` — Integra os dados da POF e da RAIS, classifica os estabelecimentos em categorias (in natura, ultraprocessado e misto), agrega os resultados por município, estado e região metropolitana, e exporta os dados para Excel com formatação visual.
---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com