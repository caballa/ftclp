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

  ofs << "ftclp_path(" << "\'" << FTCLP_SRCDIR "\'" << ").\n";
  ofs << "frontend_path(" << "\'" << FTCLP_SRCDIR << "/frontend" << "\'" << ").\n";

  ofs.close();
  return 0;
}
