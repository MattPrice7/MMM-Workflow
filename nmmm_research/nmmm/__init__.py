"""NMMM research utilities.

This package is intentionally separate from the R production MMM workflow.
"""

__version__ = "0.1.0"

from .simulator import make_synthetic_mmm_panel
from .baselines import fit_transformed_ridge_mmm
from .evaluation import evaluate_prediction_fit, evaluate_contribution_recovery
from .training_data_validation import validate_nmmm_training_data, write_training_data_validation
from .torch_training import load_torch_nmmm_checkpoint, save_torch_nmmm_checkpoint
from .torch_tft_training import fit_tft_mmm, load_tft_mmm_checkpoint, save_tft_mmm_checkpoint

__all__ = [
    "__version__",
    "make_synthetic_mmm_panel",
    "fit_transformed_ridge_mmm",
    "evaluate_prediction_fit",
    "evaluate_contribution_recovery",
    "validate_nmmm_training_data",
    "write_training_data_validation",
    "save_torch_nmmm_checkpoint",
    "load_torch_nmmm_checkpoint",
    "fit_tft_mmm",
    "save_tft_mmm_checkpoint",
    "load_tft_mmm_checkpoint",
]
