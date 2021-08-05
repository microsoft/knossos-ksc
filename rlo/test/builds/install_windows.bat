echo ----- upgrade pip -----
python -m pip install --upgrade pip || exit /b
echo ----- pip install -----
pip install -r test/builds/cpu_requirements.txt
python test\builds\print_requirements.py > requirements.txt
pip install -r requirements.txt || exit /b
