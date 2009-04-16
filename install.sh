#!/bin/sh
#

WHERE=`pwd`

set -e

CABAL_EXISTS=`which cabal`

if [ -n ${CABAL_EXISTS} ]
then
    echo "cabal found"
else
    for i in $TMPDIR /tmp c:/temp; do
	if test -d $i; then
	    dir=$i
	    break
	fi
	dir=$HOME
    done


    cd $dir
    wget http://hackage.haskell.org/packages/archive/cabal-install/0.6.2/cabal-install-0.6.2.tar.gz
    tar -zxf cabal-install-0.6.2.tar.gz
    cd cabal-install-0.6.2
    ./bootstrap.sh

    export PATH=${HOME}/.cabal/bin:${PATH}
fi

cabal update

cd ${WHERE}
cabal install

echo "Done."
echo ""
echo "********************************************************"
echo 'Please make sure ${HOME}/.cabal/bin is in your ~/.bashrc'
echo "e.g., via adding the following line:"
echo ""
echo 'export PATH=${HOME}/.cabal/bin:${PATH}'
echo "********************************************************"
