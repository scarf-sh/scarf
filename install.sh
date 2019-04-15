#!/bin/sh

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

PLATFORM=""

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Installing scarf on linux"
    PLATFORM="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Installing scarf on mac"
    PLATFORM="mac"
else
    echo "${RED}Sorry, Scarf is not yet supported on your platform. Open up an issue on github or email help@scarf.sh to let us know you'd like scarf for your platform${NC}"
    exit 255
fi

pushd . >> /dev/null

cd /tmp || (echo "no /tmp directory found to extract scarf into" && exit 1)
wget "https://s3.us-west-2.amazonaws.com/scarf.sh/downloads/scarf/latest/scarf-0.1.0.0-${PLATFORM}.tar.gz" -O scarf.tar.gz

tar -zxvf scarf.tar.gz

mkdir -p ~/.scarf/original
mkdir -p ~/.scarf/bin
cp scarf ~/.scarf/bin

popd >> /dev/null

echo "${GREEN}Installation successful! Please add ~/.scarf/bin to your PATH${NC}"
