R="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
Rscript -e "source('./rmarkdown.r'); convertRMarkdown(images.dir='$DIR/images')"
