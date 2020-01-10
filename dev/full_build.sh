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
./configure --enable-threads --prefix=${build_dir}/source/fftw_install
make -j 4
make install
cd ..

# Copy the MDI library
cp -r ../mdi .

# Is this a Mac or Linux machine?
unameOut="$(uname -s)"

# Compile Tinker
cd source
cp ../../make/buildmake.py .
./buildmake.py ../../source/*.f > Makefile
case "${unameOut}" in
    Darwin*)    sed -i '' 's/-L$(FFTWDIR)/-Lfftw_install/g' Makefile;;
    *)          sed -i 's/-L$(FFTWDIR)/-Lfftw_install/g' Makefile;;
esac
cp ../../source/* .
make -j 4
