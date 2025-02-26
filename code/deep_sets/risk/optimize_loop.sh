#!/bin/bash
# To run: bash optimize_loop.sh k nsims test_k theory optset_size

source ~/miniconda3/etc/profile.d/conda.sh

# mkdir data_k${1}_nsim${2}_${4}
# mkdir data_k${1}_nsim${2}_${4}/outfiles

# Rscript --vanilla --verbose 01_calc_restrictions.R k=$1 nsims=$2 test_k=$3 theory=$4> data_k${1}_nsim${2}_${4}/outfiles/01_calc_restrictions.log 2>&1

conda activate torch_env
python 02_trainer.py $1 $2 $4
# python 03_optimize_garp_test.py $1 $2 $4 $5
conda deactivate

# Rscript --vanilla --verbose 04_test_optimizer_result.R k=$1 nsims=$2 optset_size=$4 y_var=$5>data_k${1}_nsim${2}_${4}/outfiles/04_test_optimizer_result_${4}.log 2>&1