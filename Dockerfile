FROM jupyter/scipy-notebook:ubuntu-20.04
LABEL maintainer="Alice Lepissier <alice.lepissier@gmail.com>"


###### START Binder code ######
# from https://mybinder.readthedocs.io/en/latest/tutorials/dockerfile.html
# Binder does not allow root container processes
# and ${NB_USER} will run the JupyterLab process on Binder
ARG NB_USER
ARG NB_UID
ENV USER ${NB_USER}
ENV NB_UID ${NB_UID}
ENV HOME /home/${NB_USER}

# To work on Binder, the contents of the repo must be in ${HOME}
COPY . ${HOME}/work
USER root
RUN chown -R ${NB_UID} ${HOME}
###### END Binder code ######


###### START R code ######
# from https://github.com/jupyter/docker-stacks/blob/master/r-notebook/Dockerfile

# R pre-requisites
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends \
    fonts-dejavu \
    unixodbc \
    unixodbc-dev \
    r-cran-rodbc \
    gfortran \
    gcc \
    # libfontconfig1-dev is a dependency of kableExtra/systemfonts
    libfontconfig1-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Fix for devtools https://github.com/conda-forge/r-devtools-feedstock/issues/4
RUN ln -s /bin/tar /bin/gtar


USER ${NB_UID}


# R packages including IRKernel which gets installed globally
RUN conda install --quiet --yes \
    'r-base=4.1.3' \
    'r-caret' \
    'r-crayon' \
    'r-devtools' \
    # e1071 is a dependency of the caret R package
    'r-e1071' \
    'r-forecast' \
    'r-hexbin' \
    'r-htmltools' \
    'r-htmlwidgets' \
    'r-irkernel' \
    'r-randomforest' \
    'r-rcurl' \
    'r-rmarkdown' \
    'r-rodbc' \
    'r-rsqlite' \
    'r-shiny' \
    'r-tidymodels' \
    'r-tidyverse' \
    # START new packages - maybe would be better in requirements.R
    'r-here' \
    #'r-feather' \
    'r-ggridges' \
    'r-janitor' \
    'r-kableExtra' \
    'r-lfe' \
    'r-plm' \
    'r-stargazer' \
    'r-WDI' \
    # END new packages
    'unixodbc' && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

# Install R libraries
COPY ./requirements.R .
RUN Rscript requirements.R && rm requirements.R
###### END R code ######


###### START RStudio code ######
USER root
# Earlier RStudio version
# from https://github.com/riazarbi/datasci-gui-minimal/blob/a281fc00c1149cd2d472e117b451d73042ba9e32/Dockerfile
#ENV RSTUDIO_VERSION=1.4.1722
# Later RStudio version
# from https://github.com/riazarbi/datasci-gui-minimal/blob/focal/Dockerfile
ENV RSTUDIO_VERSION=2022.02.1-461
# Flag is needed to make RStudio work on JupyterLab
ENV RSESSION_PROXY_RSTUDIO_1_4=yes

# RStudio pre-requisites
# from https://support.rstudio.com/hc/en-us/articles/206794537-Common-dependencies-for-RStudio-Workbench-and-RStudio-Server
# and https://github.com/rocker-org/rocker-versioned2
# and https://github.com/riazarbi/datasci-gui-minimal/blob/focal/Dockerfile
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends \
    bash-completion \
    ca-certificates \
    file \
    gdebi-core \
    libapparmor1 \
    #libgc1c2 \
    libblas-dev \
    libc6 \
    libclang-dev \
    libbz2-* \
    libcurl4 \
    libcurl4-openssl-dev \
    #libcurl4-gnutls-dev \
    libedit2 \
    libicu* \
    libjpeg-turbo* \
    liblapack-dev \
    libobjc4 \
    libpangocairo-* \
    #libpcre2* \
    libpng16* \
    libpq5 \
    #libpq-dev \
    #libssl1.1 \
    libssl-dev \
    libtiff* \
    #libuser \
    #libuser1-dev \
    liblzma* \
    lsb-release \
    procps \
    psmisc \
    python-setuptools \
    rrdtool \
    tree \
    sudo \
    wget \
    zip unzip && \        
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENV PATH=$PATH:/${NB_USER}/lib/rstudio-server/bin \
    R_HOME=/opt/conda/lib/R
ARG LITTLER=${R_HOME}/library/littler

RUN \
    # Install RStudio
    # from https://github.com/radiant-rstats/docker/blob/master/files/install-rstudio.sh
    # and https://github.com/rocker-org/rocker-versioned2/blob/master/scripts/install_rstudio.sh
    wget -q https://s3.amazonaws.com/rstudio-ide-build/server/bionic/amd64/rstudio-server-${RSTUDIO_VERSION}-amd64.deb && \
    gdebi -n rstudio-server-${RSTUDIO_VERSION}-amd64.deb && \
    rm rstudio-server-${RSTUDIO_VERSION}-amd64.deb && \
    rm -rf /var/lib/apt/lists/* && \
    # Set default CRAN mirror
    echo -e "local({\n r <- getOption('repos')\n r['CRAN'] <- 'https://cloud.r-project.org'\n  options(repos = r)\n })" > $R_HOME/etc/Rprofile.site && \
    \
    # Littler provides install2.r script
    R -e "install.packages(c('littler', 'docopt'))" && \
    \
    # Modify littler scripts to conda R location
    sed -i 's/\/${NB_USER}\/local\/lib\/R\/site-library/\/opt\/conda\/lib\/R\/library/g' \
        ${LITTLER}/examples/*.r && \
    # Create symbolic links
    ln -s ${LITTLER}/bin/r ${LITTLER}/examples/*.r /usr/local/bin/ && \
    echo "${R_HOME}/lib" | sudo tee -a /etc/ld.so.conf.d/littler.conf && \
    ldconfig && \
    fix-permissions ${CONDA_DIR} && \
    fix-permissions /home/${NB_USER}
###### END RStudio code ######


USER ${NB_USER}


###### START Jupyter code ######

# Jupyter notebook extensions & packages
RUN \
    # Python packages
    pip install lightgbm papermill \
    openpyxl pyreadr networkx joypy && \
    \
    # Install Jupyter Notebook extensions
    pip install jupyter_contrib_nbextensions jupyter_nbextensions_configurator && \
    jupyter contrib nbextension install --sys-prefix && \
    # Enable GUI configurator for Jupyter Notebook extensions
    # Known issue: configurator does not load with JupyterLab 3
    # https://github.com/Jupyter-contrib/jupyter_nbextensions_configurator/issues/127
    #jupyter nbextensions_configurator enable --sys-prefix && \ 
    \
    # Enable extensions from jupyter_contrib_nbextensions
    # from https://github.com/ipython-contrib/jupyter_contrib_nbextensions/tree/master/src/jupyter_contrib_nbextensions/nbextensions
    jupyter nbextension enable export_embedded/main --sys-prefix && \
    jupyter nbextension enable hinterland/hinterland --sys-prefix && \
    jupyter nbextension enable scratchpad/main --sys-prefix && \
    jupyter nbextension enable toc2/main --sys-prefix && \
    \
    # https://rise.readthedocs.io/en/stable/installation.html
    pip install RISE && \
    jupyter nbextension enable rise --py --sys-prefix && \
    \
    # https://github.com/data-8/nbzip/blob/master/README.md
    pip install nbzip && \
    jupyter serverextension enable --py nbzip --sys-prefix && \
    jupyter nbextension install --py nbzip --sys-prefix && \
    jupyter nbextension enable --py nbzip --sys-prefix && \
    \
    # https://nbdime.readthedocs.io/en/latest/installing.html
    pip install --upgrade nbdime && \
    nbdime extensions --enable --sys-prefix
    
# Other Python packages    
RUN conda install -y -c conda-forge cartopy && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

# Jupyter Lab extensions
# https://github.com/jupyterlab/jupyterlab-github
#RUN pip install jupyterlab_github

# Jupyter Server & RStudio configuration
# Documentation: https://github.com/jupyterhub/jupyter-server-proxy
# and https://github.com/jupyterhub/jupyter-rsession-proxy
# Known issues with RStudio and Jupyter described at
# https://github.com/jupyterhub/jupyter-rsession-proxy/issues/93
# and https://github.com/jupyterhub/jupyter-rsession-proxy/issues/95
RUN pip install jupyter-server-proxy==3.2.1 jupyter-rsession-proxy==2.0.1 && \
    # Remove cache
    rm -rf ~/.cache/pip ~/.cache/matplotlib ~/.cache/yarn && \
    \
    conda clean --all -f -y && \
    fix-permissions ${CONDA_DIR} && \
    fix-permissions /home/${NB_USER}
###### END Jupyter code ######