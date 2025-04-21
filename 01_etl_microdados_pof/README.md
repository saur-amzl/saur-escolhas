# ETL dos Microdados da Pesquisa de Orçamentos Familiares (POF)

Este repositório contém scripts para o **ETL (Extract, Transform, Load — Extrair, Transformar e Carregar)** dos microdados da **Pesquisa de Orçamentos Familiares (POF)** dos períodos **2008-2009** e **2017-2018**. O objetivo é realizar o primeiro processamento dos dados e gerar tabelas base referentes a **domicílios**, **pessoas** e **aquisição alimentar**.

---

## 📦 Etapa Inicial

- Download dos microdados da [POF 2008/2009](https://drive.google.com/drive/folders/1QLS0sRwo51Iybk0B-qtZXfrSL89xI89x?usp=drive_link) e [POF 2017/2018](https://drive.google.com/drive/folders/1sLQTUNtKr0KxnXpfhNFW8YzWXWUC9YiH?usp=drive_link) na pasta `data/raw`.

---

## 🗂️ Nome dos Arquivos de Script

Os scripts estão organizados por ano e tipo de operação:

- `1_1_leitura_pof2008.R` – Leitura dos microdados da POF 2008/2009  
- `1_2_memoria_pof2008.R` – Cálculo e tratamento baseado nos dados de 2008/2009  
- `2_1_leitura_pof2018.R` – Leitura dos microdados da POF 2017/2018  
- `2_2_memoria_pof2018.R` – Cálculo e tratamento baseado nos dados de 2017/2018  

Esses scripts foram adaptados a partir dos arquivos disponibilizados pelo IBGE nos diretórios de programas e memória de cálculo:

- [`Leitura dos Microdados - R.R`](../data/raw/2017-2018/Programas_de_Leitura_20230713/R/Leitura%20dos%20Microdados%20-%20R.R)  
- [`Tabela de Alimentação.R`](../data/raw/2017-2018/Memoria_de_Calculo_20230929/R/Tabela%20de%20Alimentacao.R)

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com