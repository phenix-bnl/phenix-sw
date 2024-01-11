// $Id: MUIOO_HASH_MAP.h,v 1.1 2006/04/22 01:57:57 hpereira Exp $
#ifndef __MUIOO_HASH_MAP_H__
#define __MUIOO_HASH_MAP_H__

/*
  \file    MUIOO_HASH_MAP.h
  \brief   handle the hash_map mess between various compilers
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:57:57 $
*/
  

#ifdef __GNUC__
  #ifdef __ICC
    #include <hash_map>
    namespace MUIOO { using std::hash_map; };
  #else
    #include <ext/hash_map>
    namespace MUIOO { using ::__gnu_cxx::hash_map; }; // GCC 3.1 and later
  #endif
#else
  namespace MUIOO { using std::hash_map; }; // GCC 3.1 and later
#endif
  
#endif
