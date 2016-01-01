if (NOT CIAO_FOUND)

  find_path(CIAO_INCLUDE_DIR 
    NAMES ciao_prolog.h
    PATHS ${CIAO_ROOT}/lib/ciao/ciaoengine-1.15/include
    NO_DEFAULT_PATH
    )
  
  find_program(CIAO_BIN 
    NAMES ciaoc
    PATHS ${CIAO_ROOT}/bin
    NO_DEFAULT_PATH
    )
  
  include (FindPackageHandleStandardArgs)
  
  find_package_handle_standard_args (CIAO REQUIRED_VARS CIAO_ROOT CIAO_INCLUDE_DIR CIAO_BIN) 
  
  mark_as_advanced (CIAO_ROOT CIAO_INCLUDE_DIR CIAO_BIN)

endif()