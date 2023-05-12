#!/bin/bash

# run with:
# sh scripts/07a_wwdt_wget_wrcc.sh

mkdir -p data/netcdf
cd data/netcdf


# MDN - temperature (1 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/mdn1/mdn1_12_PRISM.nc


# PDSI (no timescale)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/pdsi_12_PRISM.nc


# PDSI (1 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi1/pdsi1_12_PRISM.nc

# PON: precip (1 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pon1/pon1_12_PRISM.nc


# SPEI (1 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_12_PRISM.nc

# SPEI (12 month)
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_1_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_2_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_3_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_4_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_5_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_6_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_7_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_8_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_9_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_10_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_11_PRISM.nc
wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei12/spei12_12_PRISM.nc

# SPEI (24 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spei24/spei24_12_PRISM.nc


# spi (1 month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/spi1/spi1_12_PRISM.nc

# SCPDSI
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi1/scpdsi1_12_PRISM.nc

# palmer-z (previous month)
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_1_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_2_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_3_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_4_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_5_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_6_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_7_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_8_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_9_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_10_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_11_PRISM.nc
# wget -nc -c -nd http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi1/pzi1_12_PRISM.nc


