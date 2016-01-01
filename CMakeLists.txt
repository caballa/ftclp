project(FTCLP)

cmake_minimum_required (VERSION 2.8.11)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR ) 
  message (FATAL_ERROR 
    "In-source builds are not allowed. Please clean your source tree and try again.")  
endif()

# Add path for custom modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake") 

if(NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
endif(NOT CMAKE_BUILD_TYPE)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O3 -Wall")

option (FTCLP_STATIC_EXE "Static executable." OFF)

find_package(GMP REQUIRED)
if (GMP_FOUND)
  include_directories (${GMP_INCLUDE_DIR})
  include_directories (${GMPXX_INCLUDE_DIR})  
else()
  set(GMP_LIB "")
  set(GMPXX_LIB "")
endif()

include(ExternalProject)

set_property(DIRECTORY PROPERTY EP_STEP_TARGETS configure build test)

find_package (Git)
if (NOT GIT_FOUND)
  message (FATAL_ERROR "Could not find git: aborting because cannot download Ciao")
endif()

find_package(CIAO)
if (NOT CIAO_FOUND)
  set (CIAO_TAG "origin/master" CACHE STRING "Ciao tag to use")
  set (CIAO_REPO "https://github.com/ciao-lang/ciao.git" CACHE STRING "Ciao repository")

  ExternalProject_Add(ciao
    GIT_REPOSITORY ${CIAO_REPO}
    GIT_TAG ${CIAO_TAG}
    PREFIX ${CMAKE_BINARY_DIR}/ciao
    INSTALL_DIR ${CMAKE_BINARY_DIR}/run/ciao
    CONFIGURE_COMMAND ./ciao-boot.sh configure --instype=global --with-docs=no  --core:update_bashrc=no --core:update_cshrc=no --core:with_emacs_mode=no --core:update_dotemacs=no --install_prefix=<INSTALL_DIR> --lpdoc:htmldir=/tmp/ciao/html --core:with_java_interface=no --contrib:with_tabling=no
    BUILD_IN_SOURCE 1
    BUILD_COMMAND ./ciao-boot.sh build
    INSTALL_COMMAND ./ciao-boot.sh install
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1)

   ExternalProject_Get_Property (ciao INSTALL_DIR)
   set (CIAO_ROOT ${INSTALL_DIR} CACHE FILEPATH "Forced location of ciao" FORCE)
   message (WARNING "No Ciao found. Run \n\tcmake --build . --target ciao && cmake ${CMAKE_SOURCE_DIR}")
   return()

endif ()

include_directories (${CIAO_INCLUDE_DIR})

find_package(Wget REQUIRED)
if (NOT WGET_FOUND)
    message (FATAL_ERROR "No wget found.")
endif ()


set (MSAT_ROOT "${CMAKE_BINARY_DIR}/run/msat" CACHE STRING "Root path to msat")      
set (MSAT_NAME "mathsat-5.2.11-linux-x86_64")
set (MSAT_TARBALL "http://mathsat.fbk.eu/download.php?file=${MSAT_NAME}.tar.gz" CACHE STRING "msat tarball")
add_custom_target (msat 
  COMMAND ${WGET_EXECUTABLE} "${MSAT_TARBALL}" 
  COMMAND ${CMAKE_COMMAND} -E rename "download.php?file=${MSAT_NAME}.tar.gz" "${MSAT_NAME}.tar.gz"
  COMMAND ${CMAKE_COMMAND} -E tar "xvzf" "${MSAT_NAME}.tar.gz" 
  COMMAND ${CMAKE_COMMAND} -E remove_directory "${MSAT_ROOT}" 
  COMMAND ${CMAKE_COMMAND} -E make_directory "${MSAT_ROOT}" 
  COMMAND ${CMAKE_COMMAND} -E rename "${MSAT_NAME}" "${MSAT_ROOT}")

find_package (MSAT)
if (MSAT_FOUND)
  include_directories (${MSAT_INCLUDE_DIR})
  set (MSAT_LIBS ${MSAT_LIBRARY})
  #set (MSAT_INSTALL_DIR ${MSAT_ROOT})
else()
  message (WARNING "No msat found. Run \n\tcmake --build . --target msat && cmake ${CMAKE_SOURCE_DIR}")
  return()
endif()

add_custom_target (reqs DEPENDS ciao msat)

# Not needed on OSX.
find_library(RT_LIB NAMES rt)
if (NOT RT_LIB)
  set(RT_LIB "")
endif()
mark_as_advanced(RT_LIB)

set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

# These are used by config.h.cmake
get_filename_component (GMP_LIBDIR ${GMP_LIB} DIRECTORY)
set (GMP_LIBDIR "\"${GMP_LIBDIR}\"")
set (SOLVERS_LIBDIR "\"${CMAKE_BINARY_DIR}/solvers_C\"" )
set (TRIE_LIBDIR "\"${CMAKE_BINARY_DIR}/trie_C\"" )
set (MSAT_LIBDIR "\"${MSAT_ROOT}/lib\"") 
set (FTCLP_SRCDIR "\"${CMAKE_SOURCE_DIR}\"") 

configure_file( include/config.h.cmake
                ${CMAKE_BINARY_DIR}/include/config.h )

include_directories (${FTCLP_SOURCE_DIR}/include)

include_directories (${CMAKE_BINARY_DIR}/include)

# C/C++
add_subdirectory (scripts)
add_subdirectory (trie_C)
add_subdirectory (solvers_C)

# Prolog configuration stuff
add_custom_target (prolog_config 
    # delete old file if exist
    COMMAND rm -f "${CMAKE_SOURCE_DIR}/solvers_C/linker_opts.pl"
    COMMAND rm -f "${CMAKE_SOURCE_DIR}/trie_C/linker_opts.pl"
    COMMAND rm -f "${CMAKE_SOURCE_DIR}/ftclp_paths.pl"
    # generate configuration files
    COMMAND ${CMAKE_BINARY_DIR}/scripts/gen_linker_opts 
            "${CMAKE_SOURCE_DIR}/solvers_C/linker_opts.pl" "solvers_C"
    COMMAND ${CMAKE_BINARY_DIR}/scripts/gen_linker_opts 
            "${CMAKE_SOURCE_DIR}/trie_C/linker_opts.pl" "trie_C"
    COMMAND ${CMAKE_BINARY_DIR}/scripts/gen_paths
            "${CMAKE_SOURCE_DIR}/ftclp_paths.pl")

if (FTCLP_STATIC_EXE)
   add_custom_target (ftclp COMMAND ${CIAO_BIN} -S "${CMAKE_SOURCE_DIR}/ftclp.pl")
else ()
   add_custom_target (ftclp COMMAND ${CIAO_BIN}  "${CMAKE_SOURCE_DIR}/ftclp.pl")
endif ()

add_dependencies (ftclp prolog_config)

# install(FILES ftclp 
#   DESTINATION bin
#   PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_WRITE GROUP_READ WORLD_READ WORLD_EXECUTE)
# install(FILES ${MSAT_ROOT}/lib/libmathsat.a DESTINATION lib)
# install(DIRECTORY ${MSAT_ROOT}/include/ DESTINATION include)
# install(DIRECTORY include/ DESTINATION include)
# install(FILES scripts/horn_prover DESTINATION bin)






