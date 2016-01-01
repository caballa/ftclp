/* Author: Jorge. A Navas, The University of Melbourne 2012-2013  */

#ifndef __CIAO_COMMON__H
#define __CIAO_COMMON__H

#include <iostream>

using namespace std;

namespace Ciao {

  // Exception for internal errors
  class error {
    
  private:
    string _msg;
    error();
    
  public:

    error(string msg): _msg(msg) { 
    }

    string message() {
      return _msg;
    }

    const char * what() {
      const char * r = _msg.c_str();
      return r;
    }

    ostream& write(ostream& o)  {
      o << message();
      return o;
    }
    
  }; // class error

  inline ostream& operator<<(ostream&o, error e){
    e.write(o);
    return o;
  }

} // end namespace

#endif /* __CIAO_COMMON__H */
