# ETL dos Microdados da Pesquisa de Orçamentos Familiares (POF)

Este repositório contém scripts para o **ETL (Extract, Transform, Load — Extrair, Transformar e Carregar)** dos microdados da **Pesquisa de Orçamentos Familiares (POF)** dos períodos **2008-2009** e **2017-2018**. O objetivo é realizar o processamento inicial desses dados e gerar tabelas base referentes a **domicílios**, **pessoas** e **aquisição alimentar**.

---

### Download dos dados

Para executar os scripts deste repositório, é necessário realizar o download manual dos microdados da POF diretamente dos links abaixo:

- [POF 2008/2009](https://drive.google.com/drive/folders/1QLS0sRwo51Iybk0B-qtZXfrSL89xI89x?usp=drive_link)  
- [POF 2017/2018](https://drive.google.com/drive/folders/1sLQTUNtKr0KxnXpfhNFW8YzWXWUC9YiH?usp=drive_link)

Após o download, coloque os arquivos extraídos na pasta:

`data/raw/`

Essa estrutura é necessária para que os scripts de leitura e transformação funcionem corretamente.

---
### Ambiente e pacotes

Os scripts estão escritos na linguagem **R**.
Todos os pacotes necessários estão listados no início de cada script. Certifique-se de instalá-los previamente para garantir a execução correta.

---
### Scripts 

Os scripts estão organizados por ano e por etapa de processamento:

- `1_1_leitura_pof2008.R` – Leitura dos microdados da POF 2008/2009  
- `1_2_memoria_pof2008.R` – Cálculo e tratamento com base nos dados de 2008/2009  
- `2_1_leitura_pof2018.R` – Leitura dos microdados da POF 2017/2018  
- `2_2_memoria_pof2018.R` – Cálculo e tratamento com base nos dados de 2017/2018  

Esses scripts foram desenvolvidos com base nos códigos fornecidos pelo IBGE junto aos microdados, disponíveis localmente após o download, nas seguintes pastas:

- `2017-2018/Programas_de_Leitura_20230713/R/Leitura dos Microdados - R.R`: script original para leitura dos arquivos da POF  
- `2017-2018/Memoria_de_Calculo_20230929/R/Tabela de Alimentacao.R`: script com os procedimentos de cálculo para tabulações específicas

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com