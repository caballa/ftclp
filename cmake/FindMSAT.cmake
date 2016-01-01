if (NOT MSAT_FOUND)

  find_path(MSAT_INCLUDE_DIR 
    NAMES mathsat.h
    PATHS ${MSAT_ROOT}/include
    NO_DEFAULT_PATH
    )

  find_library(MSAT_LIBRARY
    NAMES mathsat
    PATHS ${MSAT_ROOT}/lib
    NO_DEFAULT_PATH
    )

  include (FindPackageHandleStandardArgs)

  find_package_handle_standard_args (MSAT REQUIRED_VARS MSAT_INCLUDE_DIR MSAT_LIBRARY) 

  mark_as_advanced (MSAT_INCLUDE_DIR MSAT_LIBRARY)

endif()