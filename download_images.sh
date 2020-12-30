#
# Retrieve image data from https://fei.edu.br/~cet/facedatabase.html
#

COLOR_1=https://fei.edu.br/~cet/frontalimages_manuallyaligned_part1.zip
COLOR_2=https://fei.edu.br/~cet/frontalimages_manuallyaligned_part2.zip

NORMALIZED_1=https://fei.edu.br/~cet/frontalimages_spatiallynormalized_part1.zip
NORMALIZED_2=https://fei.edu.br/~cet/frontalimages_spatiallynormalized_part2.zip

function download {
  sourcefile=$1
  outdir=./data/$2

  tempfile=${outdir}_temp.zip

  echo "downloading $sourcefile..."
  curl -s $sourcefile > $tempfile
  echo "unzipping into $outdir..."
  unzip -n -q -j $tempfile -d $outdir
  rm $tempfile
}

download $COLOR_1 "color"
download $COLOR_2 "color"
download $NORMALIZED_1 "normalized"
download $NORMALIZED_2 "normalized"

# make output dir
mkdir -p output


