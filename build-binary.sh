#!/bin/bash

arch="$1"
version=$(grep "^version" scarf.cabal | sed 's/ //g'  | cut -d ':' -f 2)
archive="scarf-$version-$arch.tar.gz"

echo "Creating $archive"

stack install
cp ~/.local/bin/scarf .
strip scarf
tar -czvf "$archive" scarf

echo "Done!"

