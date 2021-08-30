git clone git://github.com/minrk/nbextension-scratchpad
jupyter nbextension install nbextension-scratchpad --sys-prefix
jupyter nbextension enable nbextension-scratchpad/main --sys-prefix
rm ~/work/nbextension-scratchpad/ -rf