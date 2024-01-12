
#ifndef __CONFIGPARSER_H__
#define __CONFIGPARSER_H__

#include <boost/version.hpp> // to get BOOST_VERSION
#if (__GNUC__ == 4 && __GNUC_MINOR__ == 8 && (BOOST_VERSION == 106300 || BOOST_VERSION == 105700))
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#include <boost/spirit/include/classic_core.hpp>
#pragma GCC diagnostic warning "-Wunused-local-typedefs"
#else
#include <boost/spirit/include/classic_core.hpp>
#endif

using namespace boost::spirit::classic;

struct assign_string
{
  assign_string(std::string& str_)
    : str(str_) {};
  
  template<typename IteratorT >
  void operator()(IteratorT first, IteratorT last) const
  {
    str.assign(first, last);
  }
  
  std::string& str;
};

struct keyvalue_grammar : public grammar<keyvalue_grammar>
{
  // Constructor, takes two reference to string
  // str_key and str_value will hold the parse result

  keyvalue_grammar(std::string& str_key_, std::string& str_val_)
    : str_key(str_key_), str_val(str_val_){};
  
  template <typename ScannerT>
  struct definition
  {    
    definition(keyvalue_grammar const& self)  
    { 
      equal = ch_p('=');
      
      key = (+(alnum_p|ch_p('_')))[ assign_string(self.str_key) ];
      
      value = (+(alnum_p|ch_p('.')))[assign_string(self.str_val)];
      
      key_value = key >> equal >> value;
    }
    
    rule<ScannerT>  key, equal, value, key_value;
    rule<ScannerT> const& start() const { return key_value; };
  };
  
  std::string& str_key;
  std::string& str_val;
};

template<typename keyvalue_container> 
class add_keyvalue_pair
{
public:
  // key_ and val_ should point to the string modified in keyvalue_grammar
  
  // kvc_ is the map of key - values
  
  add_keyvalue_pair( keyvalue_container& kvc_, std::string& key_, std::string& val_)
    : kvc( kvc_ ), key(key_), val(val_)
  {
  }
  
  // the method called by the parser    
  
  template <typename IteratorT>
  void operator()(IteratorT first, IteratorT last) const
  {
    kvc.insert( keyvalue_container::value_type(key, val) );
  }
private:
  std::string& key;
  std::string& val;
  keyvalue_container& kvc;
};

// template<typename keyvalue_container> 
// struct line_grammar : public grammar<line_grammar>
// {
//   // Constructor

//   // kvc_ is a key-value container

//   // str_command will hold the application name

//   line_grammar( keyvalue_container& kvc_, std::string& str_command_)
//     : kvc(kvc_), str_command(str_command_)
//   {};
  
//   template <typename ScannerT>
//   struct definition
//   {
//     definition( line_grammar<keyvalue_container> const& self )
//       : key_value( key, value )
//     { 
//       line = key_value
// 	[ 
// 	 add_keyvalue_pair<keyvalue_container>( 
// 					       self.kvc, 
// 					       key, value ) 
// 	 ];
//     }            
    
//     rule<ScannerT> line;
//     keyvalue_grammar key_value;
//     std::string key;
//     std::string value;
//     rule<ScannerT> const& start() const { return line; }
//   };
  
//   keyvalue_container& kvc;
//   std::string& str_command;
// };

#endif
