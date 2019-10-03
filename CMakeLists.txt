# CMakeLists files in this project can
# refer to the root source directory of the project as ${PROJECT_SOURCE_DIR} and
# to the root binary directory of the project as ${PROJECT_BINARY_DIR}.
cmake_minimum_required (VERSION 2.8.11)
project (LotusExtraModules Fortran)
enable_language(Fortran)

# Pick up all of the source files using the file(GLOB...), which allows for wildcard additions.
file(GLOB SOURCES "src/*.[fF]90")

# Set build type.
set(CMAKE_BUILD_TYPE Release)

# Generate the shared library from the sources
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)
add_library(LotusExtraModules STATIC ${SOURCES})

# Set the location for library installation.
install(TARGETS LotusExtraModules DESTINATION ${PROJECT_SOURCE_DIR}/lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION ${PROJECT_SOURCE_DIR}/include)
