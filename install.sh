#!/bin/sh

PLATFORM=""

VERSION="0.5.0"

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Installing scarf on linux"
    PLATFORM="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Installing scarf on mac"
    PLATFORM="mac"
else
    echo "Sorry, Scarf is not yet supported on your platform. Open up an issue on github or email help@scarf.sh to let us know you'd like scarf for your platform"
    exit 255
fi

echo "Please note that Scarf will send anonymized package usage statistics to their respective software developers. To learn more, see https://docs.scarf.sh"

pushd . >> /dev/null

cd /tmp || (echo "no /tmp directory found to extract scarf into" && exit 1)

curl "https://s3.us-west-2.amazonaws.com/scarf-sh/downloads/scarf/latest/scarf-${VERSION}-${PLATFORM}.tar.gz" -o scarf.tar.gz

tar -zxvf scarf.tar.gz

mkdir -p ~/.scarf/original
mkdir -p ~/.scarf/bin
mkdir -p ~/.scarf/include
cp scarf ~/.scarf/bin

popd >> /dev/null

echo "Installation successful! Scarf will install programs into ~/.scarf/bin, so please add it to your PATH."
