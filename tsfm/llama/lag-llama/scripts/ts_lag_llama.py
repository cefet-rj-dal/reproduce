import torch, os, sys

file_path = os.path.dirname(__file__)
folder_path = os.path.join(file_path, '..')
normalized_path = os.path.realpath(folder_path)

sys.path.append(normalized_path)

# Core libs
import numpy as np

# GluonTS and Lag-LLaMA imports
from lag_llama.gluon.estimator import LagLlamaEstimator
from gluonts.dataset.common import ListDataset

MODEL_PATH = os.path.join(normalized_path, 'models/lag-llama.ckpt')

def ts_lag_llama_predict(
  dataset: np.ndarray,
  steps_ahead: int = 1,
  samples: int = 100,
  epochs: int = 50,
  dataset_info = {"start": "1970-01-01 00:00:00", "frequency": "A"},
  val_dataset=None,
  zs=False,
):
    """
    Predict future values using a pre-trained Lag-LLaMA checkpoint.

    Parameters
    - dataset: 1D array-like time series used as conditioning context
    - steps_ahead: forecast horizon (prediction_length)
    - samples: number of Monte Carlo samples to draw
    - epochs: max epochs for training when fine-tuning
    - dataset_info: dict with 'start' and 'frequency' for GluonTS ListDataset
    - val_dataset: optional validation dataset when fine-tuning
    - zs: if True runs zero-shot; otherwise fine-tunes before predicting
    """
    device = torch.device('cuda') if torch.cuda.is_available() else torch.device('cpu')
    model = torch.load(MODEL_PATH, map_location=device)

    estimator_args = model["hyper_parameters"]["model_kwargs"]
    
    estimator = LagLlamaEstimator(
      ckpt_path=MODEL_PATH,
      prediction_length=steps_ahead,  # how many steps to predict into the future
      context_length=len(dataset),    # how many past values to condition on
      
      trainer_kwargs={"max_epochs": epochs},    # lightning trainer arguments
      num_batches_per_epoch=50,
      
      # Model hyperparameters
      input_size=estimator_args["input_size"],
      n_layer=estimator_args["n_layer"],
      n_embd_per_head=estimator_args["n_embd_per_head"],
      n_head=estimator_args["n_head"],
      scaling=estimator_args["scaling"],
      time_feat=estimator_args["time_feat"],
    )
    
    dataset = __format_ts(dataset, dataset_info)
    
    if zs:
        # zero-shot: create predictor without fine-tuning
        predictor = estimator.create_predictor(  # PyTorchPredictor
            transformation=estimator.create_transformation(),  # Chain
            module=estimator.create_lightning_module()  # LagLLamaLightningModule
        )
    else:
        # fine-tuning then predicting
        predictor = estimator.train(training_data=dataset, validation_data=val_dataset)
    
    forecasts = predictor.predict(
        dataset=dataset,
	    num_samples=samples
	)
    
    return list(forecasts)


def __format_ts(x, dataset_info: dict):
    """Format 1D array-like x into a GluonTS ListDataset with metadata."""
    if len(np.array(x).shape) != 1:
      x = np.array(x).reshape(-1).tolist()
    
    dataset = ListDataset(
      data_iter=[{
        "start": dataset_info.get("start"),
        "target": x
      }], freq=dataset_info.get("frequency")
    )
    
    return dataset
