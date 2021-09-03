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
cp -r ../../params .
cd fftw
ls
./configure --enable-threads --prefix=${build_dir}/tinker/fftw
make -j 4
make install
cd ..

# Copy the MDI library
cp -r ../../mdi .

# Copy buildmake.py
mkdir source
cd source
cp ../../../make/buildmake.py .

# If F77 has been set, modify buildmake.py to use the correct F77
[[ ! -z "${FC}" ]] && sed -i "s/F77 := gfortran/F77 := ${FC}/g" buildmake.py

# Create a build file for Tinker
./buildmake.py ../../../source/*.f > Makefile

# Compile Tinker
cp ../../../source/* .
make -j 4
