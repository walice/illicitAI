cd ~/work
wget -q -O ~/work/Data.zip https://github.com/walice/illicitAI-data/raw/main/Data.zip
unzip ~/work/Data.zip -d ~/work/
rm ~/requirements.R -f
mkdir ~/work/Results/
#papermill Scripts/Predictions.ipynb Scripts/Predictions.ipynb