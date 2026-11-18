source(
  "renv/activate.R"
)

if (
  requireNamespace(
    "reticulate",
    quietly = TRUE
  )
) {
  reticulate::py_require(
    packages = c(
      "numpy<3",
      "pandas",
      "scikit-learn",
      "torch==2.15.0"
    ),
    python_version = ">=3.11,<3.13",
    action = "add"
  )
}
