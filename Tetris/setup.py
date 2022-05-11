"""
# Setup Script

Derived from the setuptools sample project at
https://github.com/pypa/sampleproject/blob/main/setup.py

"""

# Always prefer setuptools over distutils
from setuptools import setup, find_packages
import pathlib

here = pathlib.Path(__file__).parent.resolve()

# Get the long description from the README file
long_description = (here / "readme.md").read_text(encoding="utf-8")

_VLSIR_VERSION = "1.0.0.dev0"

setup(
    name="tetris",
    version=_VLSIR_VERSION,
    description="Gridded Semi-Custom Integrated Circuit Layout",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/dan-fritchman/Layout21",
    author="Dan Fritchman",
    packages=find_packages(),
    python_requires=">=3.7, <4",
    install_requires=[f"vlsir=={_VLSIR_VERSION}", "numpy==1.21.5"],
    extras_require={
        "dev": ["pytest==5.2", "coverage", "pytest-cov", "black==19.10b0", "twine"]
    },
)
