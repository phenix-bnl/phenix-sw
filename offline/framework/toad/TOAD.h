#ifndef ROOT__TOAD
#define ROOT__TOAD

#include <string>
#include <stdexcept>
#include <iostream>

class TOAD
{

private:
  std::string pfn;
  std::string package_name;
  int verb;

public:
  TOAD(const std::string& pn) :
    package_name(pn),
    verb(0)
  {
    if (package_name.empty()) {
      const char* msg = "TOAD : You must specify a module name in this constructor, TOAD comitting suicide";
      std::cerr << msg << std::endl;
      throw std::invalid_argument(msg);
    }

    std::cout << "TOAD : Initializing for package = " << package_name << std::endl;
  }
  virtual ~TOAD(){}

  std::string location(const std::string& lname);
  std::string localSearch(const std::string& lname);
  void SetVerbosity(int v){verb = v;}
};

#endif
