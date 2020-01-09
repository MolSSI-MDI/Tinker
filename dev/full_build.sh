# Create the build directory
cd ..
rm -rf build
mkdir build
cd build

# Compile the FFTW library
build_dir=$(pwd)
mkdir fftw
cp -r ../fftw .
cd fftw
ls
./configure --enable-threads --prefix=${build_dir}/fftw_install
make -j 4
make install
cd ..

# Compile Tinker
cp ../make/buildmake.py .
./buildmake.py ../source/*.f > Makefile
sed -i 's/-L$(FFTWDIR)/-Lfftw_install/g' Makefile
cp ../source/* .
make -j 4
