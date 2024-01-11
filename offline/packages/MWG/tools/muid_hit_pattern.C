// $Id: muid_hit_pattern.C,v 1.2 2009/05/03 18:06:05 hpereira Exp $

/*
\file muid_hit_pattern.C
\brief converts new framework muid hit pattern into old framework hit pattern
*/

#include "Tools.h"
int Tools::get_muid_hit_pattern( int muioo_hit_pattern )
{
  
  int out( 0 );
  for (int ipl = 0; ipl<5; ipl++) 
  {
    out |= ((muioo_hit_pattern&1) | ((muioo_hit_pattern&32) >> 4)) << (2*ipl);
    muioo_hit_pattern>>=1;
  }
  return out;
  
}
