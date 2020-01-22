from setuptools import setup, find_packages

setup(name='ksc',
      version='0.1',
      description='Python interface for Knossos',
      author='Ryota Tomioka',
      author_email='ryoto@microsoft.com',
      install_requires=['sexpdata'],
      packages=["ksc", "ksc.backends"]
     )
