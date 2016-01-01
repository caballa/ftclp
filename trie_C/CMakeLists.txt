add_library(ciao_trie.LIB
  ciao_trie.cpp
  critbit.c
  )

set_target_properties (ciao_trie.LIB PROPERTIES OUTPUT_NAME "ciao_trie")

if (FTCLP_STATIC_EXE)
  set_target_properties (ciao_trie.LIB PROPERTIES LINK_SEARCH_START_STATIC ON)
endif ()

# install (TARGETS ciao_trie.LIB ARCHIVE DESTINATION lib)
