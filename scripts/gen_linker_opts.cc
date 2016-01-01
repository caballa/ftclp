#include "config.h"
#include <fstream>  
#include <iostream>  

using namespace std;

int main (int arg, char**argv) {

  if (arg < 2) {
    cerr << "ERROR: output file name not given.\n";
    return -1;
  }

  ofstream ofs;
  ofs.open (argv[1], ofstream::out | ofstream::app);

  if (string(argv[2]) == "solvers_C") { 
    ofs << ":- extra_linker_opts([" 
        << "' -L" << SOLVERS_LIBDIR << "',\n"
        << "' -L" << GMP_LIBDIR << "',\n"
        << "' -L" << MSAT_LIBDIR << "']).\n";
  } else if (string(argv[2]) == "trie_C") {
    ofs << ":- extra_linker_opts(' -L" << TRIE_LIBDIR << "').\n";
  }

  ofs.close();
  return 0;
}
