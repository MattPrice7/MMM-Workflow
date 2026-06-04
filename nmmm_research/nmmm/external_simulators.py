"""Optional external simulator adapters.

These adapters keep third-party simulation tools out of the core import path.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Optional


@dataclass
class ExternalSimulationResult:
    simulator: str
    data: Any
    truth: Any
    metadata: Dict[str, Any]


def run_pysimmmulator_config(config_path: str, output_path: Optional[str] = None) -> ExternalSimulationResult:
    """Run PySiMMMulator from a config file when it is installed.

    PySiMMMulator is preferred for broad simulator benchmarking because it is an
    open-source MMM simulator designed to generate inputs plus true channel ROI.
    The exact returned object can vary by package version, so this adapter keeps
    the raw objects and records metadata rather than forcing a brittle schema.
    """
    try:
        from pysimmmulator import Simulate, load_config
    except Exception as exc:  # pragma: no cover - optional dependency.
        raise ImportError(
            "PySiMMMulator is not installed. Install with `pip install pysimmmulator`."
        ) from exc

    config_path = str(Path(config_path).expanduser().resolve())
    cfg = load_config(config_path=config_path)
    simulator = Simulate()
    result = simulator.run_with_config(config=cfg)

    data = None
    truth = None
    if isinstance(result, tuple) and len(result) >= 2:
        data, truth = result[0], result[1]
    else:
        data = getattr(result, "data", None) or getattr(result, "df", None) or getattr(simulator, "final_df", None)
        truth = (
            getattr(result, "truth", None)
            or getattr(result, "channel_roi", None)
            or getattr(simulator, "channel_roi", None)
        )

    if output_path is not None:
        out = Path(output_path).expanduser().resolve()
        out.mkdir(parents=True, exist_ok=True)
        if hasattr(data, "to_csv"):
            data.to_csv(out / "pysimmmulator_data.csv", index=False)
        if hasattr(truth, "to_csv"):
            truth.to_csv(out / "pysimmmulator_truth.csv", index=False)

    return ExternalSimulationResult(
        simulator="pysimmmulator",
        data=data,
        truth=truth,
        metadata={
            "config_path": config_path,
            "output_path": str(Path(output_path).expanduser().resolve()) if output_path else None,
            "raw_result_type": type(result).__name__,
            "data_type": type(data).__name__,
            "truth_type": type(truth).__name__,
        },
    )


def available_external_simulators() -> Dict[str, bool]:
    """Return optional simulator availability without importing the full package."""
    available = {}
    try:
        import pysimmmulator  # noqa: F401

        available["pysimmmulator"] = True
    except Exception:
        available["pysimmmulator"] = False
    return available
