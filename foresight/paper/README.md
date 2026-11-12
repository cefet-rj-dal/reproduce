# Submission review notes

These notes record confirmations and unresolved points for the submission version. Manuscript edits must remain conservative: preserve any wording that is correct, coherent, and technically defensible, and change only verified errors or inconsistencies.

## Confirmed

- The Wilcoxon comparisons use `rstatix::wilcox_effsize(..., paired = TRUE)` with its default effect-size method.
- The default `rstatix` measure is `r = |Z| / sqrt(N)`. It is nonnegative, so its sign does not indicate the direction of the difference.
- The `rstatix` magnitude thresholds are: below `0.3` small, from `0.3` to below `0.5` moderate, and `0.5` or above large.
- The statistical tutorial documenting this procedure is in <https://github.com/eogasawara/mylibrary/tree/main/examples/statistics>.
- Tomczak and Tomczak (2014) is the reference declared by `rstatix` for this effect-size procedure. No DOI is registered for the article. The article is stored in the local `references` directory and included in `references.bib` as `tomczak_need_2014`.
- `lusquino_extending_2020` supports the ReW training mechanism described in the manuscript. The two ReW figures are original illustrations with numerical examples different from the source figure.
- The TSPredIT repository is public and is not the repository withheld in the manuscript. The withheld URL refers specifically to the accompanying repository containing the experimental code and configurations and must be restored in the final accepted version.
- The Related Works section describes a literature search, but does not call it a systematic review or present exact database query strings. No additional systematic-review reporting is assumed.

## Post-review clarifications

The submission manuscript should remain self-contained and should not expose open editorial questions. The points below record details to verify against the experimental implementation and clarify when preparing a revision in response to reviewers.

1. **Supervised-window indexing — resolved:** the Background retains `|X| - s + 1` for the complete matrix of all windows. The ForeSight section now correctly defines the supervised set with `m = |X'| - s`, input window `w_i = {x'_i, ..., x'_{i+s-1}}`, and following target `x'_{i+s}`.

2. **Evidence supporting RQ2 — resolved for submission:** the manuscript now separates the demonstrated effects: the ranking supports the effect of encoding technique on predictive performance, while the dimensional sensitivity figure supports the increase in computational cost with `r`. The Results and Conclusion no longer claim that latent dimension directly improves predictive accuracy. Accuracy by dimension remains an optional analysis for a later revision, not a dependency of the current conclusion.

3. **Experimental sampling:** the evaluation units can be distinguished from the manuscript: the ranking uses series-level sMAPE, whereas the Pareto analysis uses the aggregate dataset-level sMAPE defined in the protocol. The 26 groups of ten series account for the 260 clustered series. What remains to confirm for a revision is only how the ten series in each group were selected, including any fixed list, criterion, or random seed.

4. **Cluster-wise Wilcoxon interpretation — resolved for submission:** the Results and Conclusion now report statistical significance and effect-size magnitude without deriving direction from the nonnegative `rstatix` effect size. A later revision may add paired differences or method-specific medians if a reviewer requests an explicit directional summary.

5. **MSRFF implementation:** confirm the number of scales `L`, the values and implementation meaning of `sigma_l`, and how the scale-specific matrices are combined. In particular, verify whether each scale contributes `r` components, `r/L` components, or is aggregated by another operation, so that the final representation has the dimension `r` varied in the experiments. Wang et al. and Novelli et al. use related but not identical scale conventions, and the current public TSPredIT repository does not contain the ForeSight/MSRFF implementation needed to resolve this point independently.

6. **Energy estimate:** the manuscript defines `E = t_run * eta * Phi`, where `eta` is processor utilization and `Phi` is processor power, but does not report the values or acquisition procedure used for these terms. Confirm the value and interpretation of `Phi` (for example, nominal power or TDP), how `eta` was measured or assumed, and whether it varied by execution. These details should be reported in a revision because estimated energy is used in the Pareto analysis.

## Post-review methodology clarifications

1. **ForeSight WNN backbone:** the method is described as compatible with ReW and CReW, and the experimental section states that both were implemented. The result figures, however, identify variants only as `FS+encoding`. Confirm whether each reported ForeSight result uses ReW, CReW, a combination of both, or a configuration selected between them, and document the selection rule in a revision.

2. **Binarization and ReW aggregation:** confirm which thermometer-encoding variant was used in the experiments. The method discusses distributive encoding, while the evaluation workflow also displays Gaussian encoding and the protocol varies only the number of bits. Also confirm which ReW output aggregation function was adopted among the arithmetic mean, harmonic mean, geometric mean, and median options described in the Background.

3. **Learnable temporal encoders:** confirm the implementation source and training configuration used for ConvSPE, TS2Vec, and TRep, including whether representations were trained per series, per dataset, or globally; whether pretrained weights were used; and the relevant architecture, optimizer, learning-rate, epoch, batch-size, and random-seed settings. The inaccurate generic equation previously attributed to TS2Vec has been removed; the manuscript now describes its temporal, instance-wise, and hierarchical contrastive objectives consistently with the original article.

4. **Time-series cross-validation:** confirm the number and construction of validation partitions, whether the windows were expanding or sliding, the validation horizon, the optimization metric, and how performance was aggregated to select each configuration. Also record the software and package versions whose defaults were used, because unspecified library defaults may change between versions.

5. **RO and SA forecasting protocols — resolved for submission:** the public TSPredIT implementation and tutorials establish that the model is fitted once. RO performs sequential one-step forecasts using newly observed test values to update the input context, whereas SA generates the horizon without intermediate true values and sliding-window models recursively incorporate previous predictions. This distinction is now explicit in the manuscript. The naïve fallback remains the last known observation for WNN `don't know answer` outputs.

6. **Feature-based clustering pipeline:** identify the six selected `catch22` characteristics, the criterion used to select them from the PCA analysis, whether K-Means received the selected original features or PCA component scores, the candidate values of `K`, the selected silhouette value, and whether features were computed from complete series or training partitions only.

7. **Ranking confidence intervals:** ask Fernando to document how the 95% confidence intervals in the ranking figures were calculated, including the analysis or resampling unit, how the five repetitions were aggregated, the interval-estimation procedure, and the interpretation of the blue band and red highlights. The manuscript retains the term `95% confidence interval`; this note requests documentation for a future revision and does not indicate that the calculation is incorrect.

## Optional experimental repository checks

The manuscript and figures are the primary evidence for the submission. The checks below are optional opportunities for a later revision rather than unresolved requirements of the present text. The public TSPredIT repository is not the experimental repository for this manuscript.

1. **ForeSight versus ReW/CReW:** the ranking already provides the aggregate comparison and shows the ForeSight variants ahead of ReW and CReW. If requested during review, inspect the paired per-series results for additional sMAPE differences, tests, effect sizes, or proportions of improved series.

2. **Predictive-result magnitudes:** the Pareto figure already presents sMAPE jointly with estimated energy, while the ranking presents relative performance across series. A compact table of exact sMAPE values by method and dataset is optional supplementary evidence if requested during review.

3. **Accuracy by latent dimension — no longer required by the current claim:** the manuscript now limits dimensional sensitivity to the demonstrated runtime effect. Inspect accuracy by `r` only if a later revision seeks to extend RQ2 beyond the evidence currently displayed.

4. **Cluster feature summaries:** the manuscript currently presents this component as a qualitative feature-based analysis supported by representative examples and cluster-wise tests. Centroids or feature distributions would be optional quantitative reinforcement in a later revision.

5. **Direction of Wilcoxon comparisons — no longer required by the current statistical wording:** the manuscript now reports significance and magnitude without treating the effect-size sign as direction. Paired sMAPE differences or method-specific medians remain optional additions for a later revision.

6. **Pareto knee identification — resolved for submission:** the manuscript no longer classifies ForeSight as an algorithmically identified knee point. It states only the directly supported result that ForeSight with MSRFF lies on the Pareto frontier and offers a favorable trade-off.

## Submission follow-up

- **Experimental repository:** record the withheld repository URL here or in the submission records so that it can be restored after peer review.

## Resolved manuscript correction

The Background now reports the confirmed default `rstatix` calculation, `r = |Z| / sqrt(N)`, and no longer attributes direction to its sign. The Tomczak and Tomczak (2014) bibliographic entry was also added.
