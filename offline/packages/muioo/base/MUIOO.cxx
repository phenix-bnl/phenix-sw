#include <MUIOO.h>
#include <cmath>

// //_______________________________________________________________
// void MUIOO::TRACE( const std::string& message)
// { std::cout << "TRACE: " << message << std::endl; }  
//   
// //_______________________________________________________________
// void MUIOO::TRACE( const std::string& message, float)
// { std::cout << "TRACE: " << message << "\t" << val << std::endl; }  

//_______________________________________________________________
void MUIOO::PRINT(std::ostream& os, const std::string& message)
{
  const int max_col=80;
  if(!message.size()) {
    os << std::string(max_col,'-') << std::endl;
    return;
  }
  int fill = max_col - message.size() - 2;
  int pre = static_cast<int>(std::floor(fill/2.0));
  int post = fill - pre;
  os << std::string(pre,'-') << " ";
  os << message << " ";
  os << std::string(post,'-') << std::endl;  
}

