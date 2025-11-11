Flight Delay Association Rules: Brazilian Domestic Flights

This folder documents the analysis reported in:

- Sternberg, A., Carvalho, D., Murta, L., Soares, J., Ogasawara, E. An analysis of Brazilian flight delays based on frequent patterns. Transportation Research Part E: Logistics and Transportation Review, 2016. DOI: 10.1016/j.tre.2016.09.013.

Objectives
- Identify frequent patterns (association rules) related to flight delays in Brazilian domestic operations.
- Answer six research questions on causes, timing, differences across airlines/airports, airport–airline relationships, and conditions under which departure delays do not propagate to arrival.
- Quantify rule support, confidence, and lift to rank delay-related patterns (e.g., under adverse weather conditions).

Method Overview
- Data preparation: index or discretize relevant attributes (e.g., time of day, weather, IFR/VFR status, previous delays) into mining-ready items.
- Frequent pattern mining: generate association rules (antecedent ⇒ consequent) with support, confidence, and lift.
- Stratified analysis: evaluate rules globally, and per airport and airline, to compare how conditions relate to delays.
- Interpretation: prioritize rules with higher lift and actionable antecedents (e.g., fog ⇒ arrival delay), and validate against domain knowledge.

Replicating the Study (guidance)
- Data: use Brazilian domestic flight datasets for the target period, including fields for airport, airline, schedule, realized times, and weather/operational conditions.
- Preprocessing: produce indexed/discretized attributes (e.g., time bins, meteorological flags) suitable for association-rule mining.
- Mining: apply an association-rule miner (e.g., Apriori or FP-Growth) with appropriate thresholds (support/confidence) and compute lift; compare rules across subsets (airport, airline).
- Reporting: summarize top rules by lift and confidence; present global and stratified findings; highlight cases where delay propagation is and isn’t observed.

Repository Contents
- There are no experiment scripts or notebooks in this folder at the moment. The steps above serve as a guide to reconstruct the analysis using your preferred data mining environment.
- If code or notebooks are later added, they will follow a semantic numbering convention, for example:
  - `1-data-preparation.R` or `.ipynb`: loading and indexing/discretization
  - `2-rule-mining.R` or `.ipynb`: association rules and metrics
  - `3-analysis-by-airport-airline.R` or `.ipynb`: stratified comparisons
  - `4-figures-and-tables.R` or `.ipynb`: result aggregation and plots

Related Docs
- Paper (DOI): https://doi.org/10.1016/j.tre.2016.09.013

Notes
- If you have an existing pipeline for association rules, you can map your feature names to the attributes described above (e.g., fog, thunderstorm, IFR, time of day, previous delays, airport/airline identifiers) and reproduce the same style of analysis.

