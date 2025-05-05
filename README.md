# Análise de Dados do Setor Agroalimentar

Este repositório contém um conjunto de scripts desenvolvidos no âmbito do projeto **"Levantamento de dados sobre os sistemas alimentares urbano-regionais na Amazônia Legal"** do Instituto Escolhas. Os dados utilizados incluem microdados de pesquisas oficiais, como a **Pesquisa de Orçamentos Familiares (POF)**, a **Pesquisa Nacional por Amostra de Domicílios (PNAD)** e a **Relação Anual de Informações Sociais (RAIS)**, entre outros.

## Estrutura do Repositório

O repositório está organizado em subpastas, conforme descrito a seguir:

### `01-etl-microdados-pof`
Contém os scripts responsáveis pela extração, transformação e carregamento (ETL) dos microdados da **POF**. O processamento gera tabelas base de **domicílios**, **pessoas** e **aquisição alimentar**.

### `02-consumo-de-alimentos`
Scripts utilizados para gerar as tabelas de análise de **consumo alimentar**, com foco nas quantidades adquiridas, nos gastos e nos locais de aquisição.

### `03-seguranca-alimentar`
Processa dados relacionados à **segurança alimentar**, com base nos microdados da **POF**, **PNAD** (2004, 2009, 2013) e **PNAD Contínua 2023**.

### `04-comercializacao-de-alimentos`
Scripts para análise da **comercialização de alimentos**, utilizando dados da **POF 2017/2018** e da **RAIS 2023**. A abordagem considera os locais de aquisição e os estabelecimentos do setor agroalimentar.

### `05-empregos-setor-agroalimentar`
Análise dos **empregos no setor agroalimentar**, com base na **RAIS 2023** e na **PNAD Contínua 2023**. Inclui a identificação de setores como produção primária, comércio, serviços alimentares e indústria de transformação de alimentos.

### `06-estabelecimentos-agro`
Processa dados do **Cadastro Nacional de Endereços para Fins Estatísticos (CNEFE)** de 2022 para análise dos **estabelecimentos agropecuários** na Amazônia Legal, com base em informações geoespaciais e de uso do solo.

---

## Pré-requisitos

Antes de executar os scripts, é necessário:

- Baixar os microdados das fontes oficiais e organizá-los nas pastas indicadas em cada diretório;
- Garantir que as bibliotecas e dependências estejam corretamente instaladas (consultar cada script individualmente para detalhes);
- Executar os scripts na ordem sugerida, pois muitos resultados dependem das etapas anteriores.

## Como Executar

1. Faça o download dos dados brutos nas pastas correspondentes, conforme instruções nos READMEs específicos.
2. Organize os arquivos na estrutura de diretórios indicada.
3. Execute os scripts seguindo a ordem sugerida em cada pasta.

---

## Contato

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com