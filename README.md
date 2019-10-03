# lotus_extra

Additional functionality aimed to extend Lotus

Unlike the rest of the code, this onli works with gfortran-5 or newer.
To set everything correctly,
```
	sudo apt-get install gfortran-5
	echo "alias gfortran='gfortran'" >> ~/.bashrc
	echo "export FC=gfortran-5" >> ~/.bashrc
	source ~/.bashrc
```
And rebuild lotus_geom_lib and lotus_oop

To configure and build this project:
```
	mkdir build
	cd build
	cmake ..
	# For non-default configuration options, do:
	# ccmake .
	# and change whatever you like.
	make
	make install
```

By default, this will install the library and include files in the
main project directory (i.e. here, where the README is). To clean,
simply remove the build and install directories:
```
	rm -r build lib include
```

It is better practice to install the code elsewhere in a dedicated
install directory. Something like:
```
	~/lib/lotus_extra
```
seems like a good option. This way differently configured versions
of the code may be kept separately without the need to recompile
every time, e.g. when working on a Lotus executable that needs to
be run with/without MPI or in DEBUG/RELEASE modes interchangably
during development.
