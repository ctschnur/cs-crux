:: Args:
::    %1: path to git-bash.exe
::    %2: conda environment name

:: eval "$(conda shell.bash hook)"
CALL conda.bat activate %1
