#!/bin/bash
# To run: bash optimize_loop.sh k nsims test_k optset_size y_var

source ~/miniconda3/etc/profile.d/conda.sh

mkdir data_k${1}_nsim${2}_${5}
mkdir data_k${1}_nsim${2}_${5}/outfiles

# Rscript --vanilla --verbose 01_generate_violations.R k=$1 nsims=$2 test_k=$3 y_var=$5 > data_k${1}_nsim${2}_${5}/outfiles/01_generate_violations.log 2>&1

# conda activate torch_env
# python 02_trainer.py $1 $2 $5
# python 03_optimize_garp_test.py $1 $2 $4 $5
# conda deactivate

Rscript --vanilla --verbose 04_test_optimizer_result.R k=$1 nsims=$2 optset_size=$4 y_var=$5>data_k${1}_nsim${2}_${5}/outfiles/04_test_optimizer_result_${4}.log 2>&1

# I suspect something is going wrong with this part of the pipeline;
# reoptimizing is returning predicted mean CCEIs around 0.76,
# which is suspiciously low
# conda activate torch_env
# python 05_finetune.py $1 $2 $4
# python 06_reoptimize_garp_test.py $1 $2 $4
# conda deactivate

# Rscript --vanilla --verbose 07_test_reoptimizer.R k=$1 nsims=$2 optset_size=$4 >data_k${1}_nsim${2}/outfiles/07_test_reoptimizer_${4}.log 2>&1
