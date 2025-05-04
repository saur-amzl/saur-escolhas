# Empregos no Setor Agroalimentar

Este repositório contém os scripts responsáveis por gerar tabelas que analisam o número de pessoas ocupadas/empregadas no setor agroalimentar. Os dados processados têm como base:

- Microdados da **Relação Anual de Informações Sociais (RAIS)** de 2023  
- Microdados da **Pesquisa Nacional por Amostra de Domicílios Contínua (PNAD Contínua)** de 2023 (4º trimestre)

---

## Setores agroalimentares considerados

Os setores do agroalimentar foram definidos com base na Classificação Nacional de Atividades Econômicas (CNAE), conforme segue:

- **Produção primária ou agropecuária:** Seção A — Agricultura, Pecuária, Produção Florestal, Pesca e Aquicultura  
- **Comércio de alimentos:** Grupos 46.2 (Comércio de Matérias-primas Agrícolas e Animais Vivos), 46.3 (Comércio de Produtos Alimentícios, Bebidas e Fumos) e 47.2 (Supermercados e Hipermercados)  
- **Serviços alimentares:** Divisão 56 (Alimentação), da Seção I — Alojamento e Alimentação  
- **Fabricação de produtos alimentícios:** Divisões 10 (Produtos Alimentícios), 11 (Bebidas) e 12 (Produtos de Fumo), da Seção C — Indústrias de Transformação

---
## Download dos dados

Os microdados da RAIS e da PNAD Contínua devem ser baixados manualmente nos links abaixo:

- [RAIS 2023](https://drive.google.com/drive/folders/1sWHNUGiJG17KR9IRiNNCmjnCxY7ku6Hv?usp=drive_link)  
- [PNADC 2023](https://drive.google.com/drive/folders/1kK31mVT9ZIhEx1DpOzkfFFecBIogdPhI?usp=drive_link)

Após o download, os arquivos devem ser salvos nos seguintes diretórios:


- `data/raw/RAIS`
- `data/raw/PNADC`. 

Essa estrutura de pastas é essencial para o funcionamento correto dos scripts.

---

## Scripts disponíveis

Os scripts devem ser executados na seguinte ordem:

- `1_dados_rais.R` — Carrega os dados da RAIS, identifica estabelecimentos do setor agroalimentar em municípios selecionados da Amazônia Legal, calcula estatísticas por cadeia produtiva em níveis nacional, estadual e municipal/metropolitano.
- `2_dados_pnadc.R` — Processa os microdados da PNAD Contínua 2023 (4º trimestre) para identificar e quantificar as ocupações no setor agroalimentar, com foco no Brasil, nos estados da Amazônia Legal e em suas capitais.

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com