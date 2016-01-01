set (MSAT_LIBS
     ${MSAT_LIBS}
     ${GMP_LIB}
     ${GMPXX_LIB}
     ${RT_LIB}
    ) 
     
add_library(ciao_smt.LIB
  ciao_smt.cpp
  msat.cpp
  util/MurmurHash3.cpp
  )

target_link_libraries (ciao_smt.LIB ${MSAT_LIBS})

set_target_properties (ciao_smt.LIB PROPERTIES OUTPUT_NAME "ciao_smt")

if (FTCLP_STATIC_EXE)
  set_target_properties (ciao_smt.LIB PROPERTIES LINK_SEARCH_START_STATIC ON)
endif ()

#install (TARGETS ciao_smt.LIB ARCHIVE DESTINATION lib)
