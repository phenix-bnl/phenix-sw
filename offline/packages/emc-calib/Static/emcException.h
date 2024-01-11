#ifndef __EMCEXCEPTION_H__
#define __EMCEXCEPTION_H__

#include <exception>
#include <string>

/** First try to an exception class. Not much used.*/

class emcException : public std::exception
{

public:
  emcException(const char* content = "?") : std::exception()
  {
    fContent = "<EXCEPTION><EMCAL> " ;
    fContent += content ;
    fContent += std::exception::what() ;
  }

  virtual ~emcException() throw()
  { }

  virtual const char* what() const throw()
  { 
    return fContent.c_str();
  }

private:
  std::string fContent;
} ;

#endif
