#!/bin/bash

# Define os caminhos de entrada e saída
INPUTDIR="/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-escolhas/data/raw"
INTDIR="/Users/marlucescarabello/Dropbox/Work/gtworkspace_outros/saur-escolhas/data/intermediate"

# Printar os caminhos
echo "Caminho de entrada: $INPUTDIR"
echo "Caminho de saída: $INTDIR"

# Rasterizações 
#gdal_rasterize \
#   -a_nodata 0 \
#   -ts 155241 158828 \
#   -a_srs EPSG:4326 \
#   -te -74.89767 -34.86586 -33.06106 7.937424 \
#   -burn 1 -add \
#   -ot Int32 -of GTiff \
#   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
#   "$INPUTDIR/re_amzl_cnefe_estab_agropecuario.gpkg" \
#   "$INTDIR/re_amzl_cnefe_estab_agropecuario_30m.tif"
   
gdal_rasterize \
   -burn 1 \
   -a_nodata 0 \
   -ts 155241 158828 \
   -a_srs EPSG:4326 \
   -te -74.89767 -34.86586 -33.06106 7.937424 \
   -ot Int32 -of GTiff \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INPUTDIR/AreasUrbanizadas2019_Brasil/AU_2022_AreasUrbanizadas2019_Brasil.shp" \
   "$INTDIR/AU_2022_AreasUrbanizadas2019_Brasil_30m.tif"    
    

gdal_rasterize \
   -a cd_mun_num \
   -a_nodata 0 \
   -ts 155241 158828 \
   -a_srs EPSG:4326 \
   -te -74.89767 -34.86586 -33.06106 7.937424 \
   -ot Int32 -of GTiff \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INPUTDIR/BR_Municipios_2023/BR_Municipios_2023.shp" \
   "$INTDIR/pa_br_municipios_2023_30m.tif"
    

# Corte com gdalwarp
gdalwarp \
   -te -74.89767 -41.27 -19.2 7.937424 \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INTDIR/pa_br_municipios_2023_30m.tif" \
   "$INTDIR/re_amzl_municipios_2023_30m_crop.tif"
 
gdalwarp \
   -te -74.89767 -41.27 -19.2 7.937424 \
   -overwrite \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INTDIR/re_amzl_cnefe_estab_agropecuario_30m.tif" \ 
   "$INTDIR/re_amzl_cnefe_estab_agropecuario_30m_crop.tif"
 
   
gdalwarp \
   -te -74.89767 -19.2 -41.27 7.937424 \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INTDIR/AU_2022_AreasUrbanizadas2019_Brasil_30m.tif" \
   "$INTDIR/re_amzl_ibge_area_surbanizadas_30m_crop.tif"    

   
gdalwarp \
   -te -74.89767 -41.27 -19.2 7.937424 \
   -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "PREDICTOR=2" -co "BIGTIFF=IF_SAFER" -co "NUM_THREADS=8" \
   "$INPUT_DIR/brasil_coverage_2023.tif" \ 
   "$INTDIR/re_amzl_mapbiomas_2023_30m_crop.tif"      
