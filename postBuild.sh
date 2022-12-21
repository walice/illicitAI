cd ~/work
wget -q -O ~/work/Data.zip https://zenodo.org/record/7470475/files/Data.zip?download=1
mkdir ~/work/Data
cd ~/work/Data
unzip ~/work/Data.zip -d ~/work/Data
rm ~/requirements.R -f
mkdir ~/work/Results/ ~/work/Figures/ ~/work/Figures/Network_Maps/
#papermill Scripts/Predictions.ipynb Scripts/Predictions.ipynb