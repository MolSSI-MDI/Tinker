# Create the build directory
cd ..
rm -rf build
mkdir build
cd build
build_dir=$(pwd)
mkdir tinker
cd tinker

# Tinker Makefiles expect everything to be in $(HOME)/tinker
HOME=${build_dir}

# Compile the FFTW library
cp -r ../../fftw .
cp -r ../../mdi .
cd fftw
ls
./configure --enable-threads --prefix=${build_dir}/tinker/fftw
make -j 32
make install
cd ..

# Copy the MDI library
cp -r ../../mdi .

# Compile Tinker
mkdir source
cd source
cp ../../../make/buildmake.py .
./buildmake.py ../../../source/*.f > Makefile
cp ../../../source/* .
make -j 32
