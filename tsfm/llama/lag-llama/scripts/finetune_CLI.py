import argparse
import pandas as pd
import numpy as np
from ts_lag_llama import *


def split_time_series(dataframe, column_name, n):
    """Split a time series using the last n observations as test.

    Parameters
    - dataframe: DataFrame containing the time series
    - column_name: name of the time series column
    - n: number of final observations for the test set

    Returns
    - (train, test): arrays with the initial data and the last n observations
    """
    if column_name not in dataframe.columns:
        raise ValueError(f"Column '{column_name}' is not present in the DataFrame.")

    if n <= 0:
        raise ValueError("The number of observations 'n' must be > 0.")

    if len(dataframe) < n:
        raise ValueError("Not enough observations to split the last 'n' elements.")

    train = dataframe[column_name].iloc[:-n].to_list()
    test = dataframe[column_name].iloc[-n:].to_list()

    # Formatting for Lag-LLaMA
    train = np.array(train, dtype=float)
    test = np.array(test, dtype=float)

    return train, test


if __name__ == "__main__":
    # Arguments
    parser = argparse.ArgumentParser(description="Configure Lag-LLaMA fine-tune or zero-shot prediction.")

    parser.add_argument("--train_dataset", type=str, required=True, help="Path to training dataset (CSV).")
    parser.add_argument("--steps_ahead", type=int, default=1, help="Number of forecast steps (default: 1)")
    parser.add_argument("--num_epochs", type=int, default=50, help="Number of epochs for fine-tuning (default: 50)")
    parser.add_argument("--samples", type=int, default=100, help="Number of samples for prediction (default: 100)")
    parser.add_argument("--val_dataset", type=str, default=None, help="Optional validation dataset path")
    parser.add_argument("--zero_shot", action="store_true", help="Enable zero-shot mode (default: False)")

    args = parser.parse_args()

    # Load dataset
    df = pd.read_csv(args.train_dataset)

    # Debug information
    print("\nFine-tune configuration:")
    print(f"- Dataset: {args.train_dataset}")
    print(f"\tRows: {df.shape[0]}")
    print(f"\tSteps ahead: {args.steps_ahead}")
    print(f"\tEpochs: {args.num_epochs}")
    print(f"\tValidation dataset: {args.val_dataset}")
    print(f"\tZero-shot: {args.zero_shot}")

    column_name = df.columns[0]
    train_dataset = np.array(df[column_name].to_list(), dtype=float)

    # Note: val_dataset is not used here, but is accepted as an argument for future use
    predictions = ts_lag_llama_predict(
        dataset=train_dataset,
        steps_ahead=args.steps_ahead,
        samples=args.samples,
        epochs=args.num_epochs,
        val_dataset=None,
        zs=args.zero_shot,
    )

    # Minimal, exportable result DataFrame; extend as needed
    results = pd.DataFrame({
        # "samples": predictions[0].samples.tolist(),  # optionally persist full samples
        "means": predictions[0].mean,
        "medians": predictions[0].median,
        "steps_ahead": predictions[0].prediction_length,
    })

    print(results)
    print(f"Writing results to /tmp/results_{column_name}.csv")
    results.to_csv(f"/tmp/results_{column_name}.csv", index=False)

