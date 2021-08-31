# Pylint config is now set in .pylintrc
# look for all python files in git
python3 -m pylint $(git ls-files '*.py')
