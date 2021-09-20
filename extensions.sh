git clone git://github.com/minrk/nbextension-scratchpad
jupyter nbextension install nbextension-scratchpad --sys-prefix
jupyter nbextension enable nbextension-scratchpad/main --sys-prefix
rm ~/work/nbextension-scratchpad/ -rf

pip install nbresuse 
jupyter serverextension enable --py nbresuse --sys-prefix
jupyter nbextension install --py nbresuse --sys-prefix
jupyter nbextension enable --py nbresuse --sys-prefix

pip install jupyter-resource-usage
jupyter serverextension enable --py jupyter-resource-usage --sys-prefix
jupyter nbextension install --py jupyter-resource-usage --sys-prefix
jupyter nbextension enable --py jupyter-resource-usage --sys-prefix