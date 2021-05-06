from setuptools import setup, find_packages

setup(
    name="ksc",
    version="0.5",
    description="Python interface for Knossos",
    author="The Knossos Team",
    author_email="Knossos@service.microsoft.com",
    install_requires=["sexpdata", "numpy", "prettyprinter", "pyrsistent"],
    packages=["ksc", "ksc.backends", "ksc.tracing", "ksc.tracing.functions"],
)
