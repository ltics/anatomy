#!/bin/bash

dependencies=("https://github.com/ziman/lightyear")
wd=$(pwd)

function checkSuccess {
    if [ $1 != 0 ]; then
        cleanup
        echo $2
        exit $1
    fi
}

function cleanup {
    echo "Cleaning up ..."
    cd $wd
    rm -rf .anatomy-install
}

mkdir -p .anatomy-install
cd .anatomy-install

for dep in ${dependencies[*]}; do
    name=$(basename $dep)

    echo "Downloading $name ..."
    git clone "$dep" &> /dev/null
    checkSuccess $? "Failed to download $name"
    cd "$name"

    echo "Building $name ..."
    idris --build *.ipkg &> /dev/null
    checkSuccess $? "Failed to build $name"

    echo "Installing $name ..."
    idris --install *.ipkg &> /dev/null
    checkSuccess $? "Failed to install $name"
    cd ../
done

cleanup
echo "*** Done: All dependencies are installed"
