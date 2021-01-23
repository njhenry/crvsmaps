##
## Execute this script with a single argument to specify the environment name
## Default environment is "r_env"
## Example: ./conda_create_R_environment.sh my_new_env
##
ENV_NAME=${1:-crvsmaps};

# Remind the user of the new environment name
echo "*** Creating environment $ENV_NAME: ***" &&
  echo "";

conda activate base &&
  # Install with the basic packages that might have C dependencies
  conda create -n $ENV_NAME -y -c conda-forge r-base=4 gxx_linux-64 gdal \
    r-codetools r-sf r-devtools &&
  conda activate $ENV_NAME &&
  # Install R packages (done separately so that environment solving doesn't
  #   take forever)
  conda install -c conda-forge -y nano r-rgdal r-rmarkdown r-maptools r-mapview \
    r-rsqlite r-tmb r-spdep r-argparse r-rcppeigen r-foreign r-yaml r-ggplot2 \
    r-raster r-tictoc r-knitr r-fasterize r-car r-rcolorbrewer r-matrix \
    r-matrixstats r-data.table r-httr r-glue r-sp r-optimx &&
  # Install R quality-of-life tools
  pip install -U radian &&
  $CONDA_PREFIX/bin/R --vanilla -e "devtools::install_github('jalvesaq/colorout')" &&
  # Get out of any conda environments
  conda deactivate && conda deactivate;

# Completion message
echo "" &&
  echo "Your environment, $ENV_NAME, is ready! To access it, run:" &&
  echo "    conda activate $ENV_NAME";
