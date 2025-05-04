# Análise de Dados do Setor Agroalimentar


Este repositório contém um conjunto de scripts para a análise de dados relacionados ao setor agroalimentar no Brasil, com foco especial na **Amazônia Legal**. Os dados utilizados incluem microdados de pesquisas oficiais, como a **Pesquisa de Orçamentos Familiares (POF)**, a **Pesquisa Nacional por Amostra de Domicílios (PNAD)** e a **Relação Anual de Informações Sociais (RAIS)**, entre outros.

O trabalho abrange as seguintes áreas:

1. **ETL de Microdados da POF**
2. **Consumo de Alimentos**
3. **Segurança Alimentar**
4. **Comercialização de Alimentos**
5. **Empregos no Setor Agroalimentar**
6. **Estabelecimentos Agropecuários (CNEFE)**

## Estrutura do Repositório

O repositório é organizado da seguinte forma:

### 01-etl-microdados-pof
Este repositório contém os scripts responsáveis pela extração, transformação e carregamento (ETL) dos microdados da **Pesquisa de Orçamentos Familiares (POF)**. O processamento aqui realizado gera tabelas base de **domicílios**, **pessoas** e **aquisição alimentar**.

### 02-consumo-de-alimentos
Os scripts desta pasta são responsáveis por gerar as tabelas usadas na análise de **Consumo de Alimentos**, com foco nas quantidades adquiridas, gastos e locais de aquisição.

### 03-seguranca-alimentar
Aqui são processados dados de segurança alimentar, com base nos microdados da **POF**, **PNAD** (2004, 2009, 2013) e **PNAD Contínua (2023)**.

### 04-comercializacao-de-alimentos
Esta pasta contém scripts que analisam a **comercialização de alimentos** com dados da **POF 2017/2018** e da **RAIS 2023**. A análise foca nos locais de aquisição de alimentos e nos estabelecimentos do setor agroalimentar.

### 05-empregos-setor-agroalimentar
Scripts nesta pasta analisam os empregos no setor agroalimentar, utilizando dados da **RAIS 2023** e da **PNAD Contínua 2023**. São analisados os setores relacionados à produção primária, comércio, serviços alimentares e fabricação de produtos alimentícios.

### 06-estabelecimentos-agro
Esta pasta contém scripts que processam dados do **Censo Agropecuário (CNEFE)** de 2022 para analisar estabelecimentos agropecuários na **Amazônia Legal**, com foco nas informações geoespaciais e uso do solo.

---

## Pré-requisitos

Antes de executar os scripts, certifique-se de que os dados foram corretamente baixados e armazenados nas pastas especificadas. Além disso, os scripts das pastas anteriores devem ser executados conforme a sequência de execução apresentada em cada pasta, pois alguns dados dependem dos resultados de etapas anteriores.

## Como Executar

1. Baixe os dados necessários de cada pesquisa nas pastas correspondentes, conforme indicado nos README das pastas.
2. Organize os arquivos na estrutura de pastas recomendada.
3. Execute os scripts sequencialmente, conforme a ordem descrita em cada pasta. Isso garantirá que todos os dados sejam processados corretamente.

---

## Contato

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com