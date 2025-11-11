Mixed Graph Complementarity (MGC): Evaluating Communication Tools

Paper
- Carvalho, L., Assis, L., Lima, L., Bezerra, E., Guedes, G., Ziviani, A., Porto, F., Barbastefano, R., Ogasawara, E. Evaluating the complementarity of communication tools for learning platforms. CSEDU 2018 – Proceedings of the 10th International Conference on Computer Supported Education, 2018. DOI: 10.5220/0006798701420153.

Goals
- Assess whether a New Communication Tool (NCT) adds complementary communication capabilities with respect to Current Communication Tools (CCT) in organizational or learning platforms.
- Use a Mixed Graph Framework (MGF) to capture and compare communication flows, quantifying changes via graph‑based metrics such as centralities.
- Explore complementarity across different group configurations and scales (e.g., SMEs) using synthetic data.

What’s Here
- This folder provides the paper context and pointers. The source code and full experimental setup for MGF are hosted externally.

Get the Code and Experiments
- Mixed Graph Framework (MGF) repository: https://github.com/eogasawara/mgf
  - Contains the MGF implementation, synthetic data generators, and experiment drivers.
  - Follow the repository’s instructions to build datasets, run experiments, and reproduce figures/tables.

Suggested Semantic Organization (if you add scripts here)
- `1-data-generation.R/.ipynb`: synthesize or load communication graphs for CCT and NCT.
- `2-mgf-construction.R/.ipynb`: build mixed graphs and compute centrality measures.
- `3-complementarity-analysis.R/.ipynb`: compare NCT vs CCT metrics across scenarios.
- `4-figures-and-tables.R/.ipynb`: aggregate and visualize results.

Related Docs
- Paper DOI: https://doi.org/10.5220/0006798701420153
- MGF GitHub: https://github.com/eogasawara/mgf

Notes
- If you later include local scripts or notebooks, please use the semantic numbering above and update this README to point to them. If you create Markdown exports of notebooks, list them here for quick reference.

