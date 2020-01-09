# Tinker Makefiles expect everything to be in $(HOME)/tinker
cd ../build
HOME=$(pwd)


cd tinker/source

# Compile Tinker
cp ../../../source/* .
make -j 4
