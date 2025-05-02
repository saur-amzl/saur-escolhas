# Consumo de Alimentos

Este repositório contém os scripts responsáveis por gerar as tabelas utilizadas nos resultados da sessão de **Consumo de Alimentos** deste estudo. Os dados processados aqui têm como base os microdados da Pesquisa de Orçamentos Familiares (POF), previamente tratados.

---

### Pré-requisitos

Antes de executar os scripts desta pasta, certifique-se de que os scripts da pasta `01-etl-microdados-pof` já foram executados, pois os dados gerados naquela etapa são insumo direto para os procedimentos aqui descritos.

---

### Scripts disponíveis

Os scripts estão organizados por tema e escopo geográfico:

- `1_qtidade_aquisicao_alimentos_brasil.R` — Quantidade adquirida de alimentos (Brasil)  
- `2_qtidade_aquisicao_alimentos_regional.R` — Quantidade adquirida de alimentos (por região)  
- `3_gastos_aquisicao_alimentacao_brasil.R` — Gastos com aquisição alimentar (Brasil)  
- `4_gastos_aquisicao_alimentacao_regional.R` — Gastos com aquisição alimentar (por região)  
- `5_locais_aquisicao.R` — Tipos e distribuição dos locais de aquisição de alimentos  
- `processar_aquisicao.R` — Script com a função para realizar o processamento da base de aquisição alimentar

---

## 👩‍💻 Autoria

**Marluce da Cruz Scarabello**  
Maio de 2025  
📧 mascarabello@gmail.com