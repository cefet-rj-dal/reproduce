# Real data (raw) — Brazilian health indicators

Monthly rate series of mortality and hospitalization related to mental and
behavioural disorders (alcohol use and mood disorders), for Brazil. Processed
indicators derived from the Brazilian Unified Health System (SUS), freely
reusable with attribution.

These files are immutable inputs; do not edit them.

## Files

File-name pattern: `txs_<data_type>_<event_type>.csv`, where the components are
kept in Portuguese to match the source files:

- `tipo_dado` (data type): `mortalidade` = mortality, `internacao` = hospitalization
- `tipo_evento` (event type): `alcool` = alcohol use, `humor` = mood disorders

Files:

- `txs_mortalidade_alcool.csv`      — mortality, alcohol use
- `txs_mortalidade_humor.csv`       — mortality, mood disorders
- `txs_internacao_alcool.csv`       — hospitalization, alcohol use
- `txs_internacao_humor.csv`        — hospitalization, mood disorders

## Data dictionary

Column names are kept in Portuguese to match the source schema; English
descriptions are given below. The same structure applies to the alcohol-use and
mood-disorder series. `Obitos` (deaths) and its derived rates refer to the
mortality dataset; in the hospitalization dataset the corresponding count field
is `Internacoes` (hospitalizations).

| Variable (Portuguese) | English meaning | Type | Description |
|---|---|---|---|
| `Tipo_Local` | Location type | Text | Geographic level (country, region, or federative unit). |
| `Local` | Location | Text | Code of the corresponding location. |
| `Nome_Local` | Location name | Text | Full name of the analyzed location. |
| `Ano` | Year | Integer | Calendar year of the rate. |
| `Mes` | Month | Integer | Calendar month of the rate. |
| `Sexo` | Sex | Text | Sex category (male or female). |
| `Faixa_Etaria` | Age group | Text | Age group. |
| `Obitos` / `Internacoes` | Deaths / Hospitalizations | Integer | Event count, stratified by location, year, sex, and age group. |
| `Populacao` | Population | Integer | Population size stratified by location, year, sex, and age group. |
| `Taxa_Especifica` | Specific rate | Decimal | Events per 100,000 inhabitants within detailed population strata. |
| `Taxa_Modificada` | Modified rate | Decimal | Age-standardized component of the specific rate, weighted by the 2010 standard population. |
| `Taxa_Bruta` | Crude rate | Decimal | Crude rate per 100,000 inhabitants, without demographic adjustment. |
| `Taxa_Padronizada` | Standardized rate | Decimal | Overall age-standardized rate, summing modified rates across strata using the 2010 standard population. |
| `Taxa_Padronizada_Por_Sexo` | Standardized rate by sex | Decimal | Sex-specific age-standardized rate using 2010 population weights. |
| `Taxa_Bruta_Por_Sexo` | Crude rate by sex | Decimal | Sex-specific crude rate (total events / total population). |
| `Taxa_Padronizada_Por_Faixa_Etaria` | Standardized rate by age group | Decimal | Age-specific standardized rate using 2010 population weights. |
| `Taxa_Bruta_Por_Faixa_Etaria` | Crude rate by age group | Decimal | Age-specific crude rate (total events / total population). |

## Column selected by the analysis

`04-analyze-real.R` selects the target rate according to the stratum:

- overall: `Taxa_Padronizada` (standardized rate)
- by sex: `Taxa_Padronizada_Por_Sexo`
- by age group: `Taxa_Padronizada_Por_Faixa_Etaria`
- sex × age group: `Taxa_Modificada`

Analyses in the manuscript use `Local == "Brasil"` (national level; `Brasil` =
Brazil).

## Temporal coverage

- Mortality: `Ano >= 2000 & Ano < 2025`.
- Hospitalization: `Ano >= 2009`.

## Provenance

The indicators derive from SUS administrative data processed by the **Saude
Mental Publica Digital** (Digital Public Mental Health) project, hosted by ICICT
(Institute of Scientific and Technological Communication and Information in
Health), Oswaldo Cruz Foundation (Fiocruz), Ministry of Health, Brazil. The
files were provided directly by the project team as a refined version of the
series, rather than downloaded from the public dashboard
(https://saudemental.icict.fiocruz.br).

- Source project: Saude Mental Publica Digital (ICICT/Fiocruz)
- URL: https://saudemental.icict.fiocruz.br
- Received on: 2025-05-10

The data may be used freely provided the source is cited.

### Suggested citation

De Boni RB, Salles R, Souto G, Cardoso C, Cruz S, Lima J, Carrijo RG,
Carvalho CC, Cunha AP, Fiel B, Kapczinski K, Mota JC, Passos IC,
Ribeiro-Alves M, Oliveira RAD, Pimentel SC, Pedroso M. Saude Mental Publica
Digital — indicator panels. Available at https://saudemental.icict.fiocruz.br
(accessed 2025-05-10).