#!/bin/bash

mkdir -p data/noaa
cd data/noaa

#today=`date +"%Y%m%d"`
today="20210805"
# divisional metadata: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt

# Palmer Drought Severity Index (PDSI)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsist-v1.0.0-${today}

# Palmer Hydrological Drought Index (PHDI)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-phdidv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-phdist-v1.0.0-${today}

# Modified Palmer Drought Severity Index (PMDI)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pmdidv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pmdist-v1.0.0-${today}

# Precip
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpnst-v1.0.0-${today}

# max avg temp (deg F)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxdv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxst-v1.0.0-${today}

# min avg temp (deg F)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmindv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tminst-v1.0.0-${today}

# mean avg temp (deg F)
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpcdv-v1.0.0-${today}
wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpcst-v1.0.0-${today}

# to look for number lines that have CA region 07:
# less climdiv-phdidv-v1.0.0-${today} | grep '^0407' | wc -l


# metadata
#wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt
#wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/drought-readme.txt
#wget -nc -c -nd https://www.ncei.noaa.gov/pub/data/cirs/climdiv/state-readme.txt
