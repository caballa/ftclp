add_executable (gen_linker_opts gen_linker_opts.cc)

add_executable (gen_paths gen_paths.cc)

#install (TARGETS gen_linker_opts RUNTIME DESTINATION bin)

#install (TARGETS gen_paths RUNTIME DESTINATION bin)