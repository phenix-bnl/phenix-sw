// $Id: MUTOO_HASH_MAP.h,v 1.2 2009/08/26 14:18:41 hpereira Exp $
#ifndef __MUTOO_HASH_MAP_H__
#define __MUTOO_HASH_MAP_H__

/*
  \file    MUTOO_HASH_MAP.h
  \brief   handle the hash_map mess between various compilers
  \author  Hugo Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2009/08/26 14:18:41 $
*/
  
/*
This class implement the 'controversary' class hash_map in a hopefully 
portable way. 
To use hash_maps in code that include this file use e.g:
  MUTOO::hash_map< key_type, value_type >::type my_hash_map;
*/

#ifdef __GNUC__

  #ifdef __ICC
    
    #include <hash_map>
    namespace MUTOO { 
      template<typename T, typename M> class hash_map 
      {
        public:
        typedef std::hash_map<T, M> type;
        
      };
    };
    
  #else

    #include <boost/unordered_map.hpp>
    namespace MUTOO
    {
    
      template<typename T, typename M> class hash_map 
      {
        public:
        typedef boost::unordered_map<T, M> type;
        
      };
      
    };
    
  #endif

#else

  namespace MUTOO { 
  
    template<typename T, typename M> class hash_map 
    {

      public:
      typedef std::hash_map<T, M> type;
        
    };
  
  };

#endif
  
#endif
