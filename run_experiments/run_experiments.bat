@echo off
set "BASE_PATH=C:\Users\wksadmin\Dropbox\DC3\Dev"
set "PYTHON_PATH=C:\ProgramData\miniconda3\envs\eyes4icu\python.exe"
set "participant_id=Not Set"

:menu
cls
echo.
echo === Experiment Runner ===
echo.
echo 1. Set Participant ID
echo 2. Calibration
echo 3. Stimulus
echo 4. Record Pupil Data 
echo 5. EyeLink 1000
echo.
echo Current Participant ID: %participant_id%
echo.
set /p choice="Please choose (1-5) or 'q' to exit:  "
if /i "%choice%"=="Q" exit
if "%choice%"=="1" goto change_id
if "%choice%"=="2" goto run1
if "%choice%"=="3" goto run2
if "%choice%"=="4" goto run3
if "%choice%"=="5" goto run_eyelink
echo Invalid choice!
pause
goto menu

:change_id
set /p participant_id="Enter new participant ID: "
goto menu

:run1
if "%participant_id%"=="Not Set" (
    echo Please set participant ID first!
    pause
    goto menu
)
echo Running Calibration for participant %participant_id%...
"%PYTHON_PATH%" "%BASE_PATH%\psa_data_quality\run_experiments\calibration.py" --participant_id %participant_id%
goto menu

:run2
if "%participant_id%"=="Not Set" (
    echo Please set participant ID first!
    pause
    goto menu
)
echo Running Display Stimulus for participant %participant_id%...
"%PYTHON_PATH%" "%BASE_PATH%\psa_data_quality\run_experiments\display_stimulus.py" --participant_id %participant_id%
goto menu

:run3
if "%participant_id%"=="Not Set" (
    echo Please set participant ID first!
    pause
    goto menu
)
echo Running Recorder for participant %participant_id%...
"%PYTHON_PATH%" "%BASE_PATH%\pupil-center-shift-measurement\recorder.py" --participant_id %participant_id%
goto menu

:run_eyelink
echo Running EyeLink Stimulus...
cd /d "%BASE_PATH%\psa_data_quality\run_experiments\eyelink1000plus\display_stimulus_deploy"
display_stimulus_deploy.exe
cd /d "%BASE_PATH%"
goto menu