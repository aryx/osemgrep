"""Library module with typed functions."""


def add(a: int, b: int) -> int:
    """Add two integers."""
    return a + b


def compute_ratio(a: int, b: int) -> float:
    """Compute the ratio of two integers."""
    return a / b


def show(value: object) -> None:
    """Print a value."""
    print(value)
