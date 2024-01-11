#include<iostream>
#include<set>
#include<TMutKeyGen.h>

/*! @ingroup test */

/*! \file testKeyGen.cxx 
\brief exercise TMutKeyGen
*/

int main(){  
  
  enum {N_ARM=2,N_STATION=3,N_OCTANT=8,N_HALF_OCTANT=2,N_GAP=3,N_CATHODE=2,N_STRIP=128};
#define DEBUG
#ifdef DEBUG  
//    for(int m=0;m<N_STRIP;m++) {
//      Key key = TMutKeyGen::get_key(0,0,0,0,0,0,0,m);
//      key.print();
//    }
//    for(int m=0;m<N_CATHODE;m++) {
//      Key key = TMutKeyGen::get_key(0,0,0,0,0,0,m,0);
//      key.print();
//    }
//    for(int m=0;m<N_GAP;m++) {
//      Key key = TMutKeyGen::get_key(0,0,0,0,0,m,0,0);
//      key.print();
//    }
//    for(int m=0;m<N_HALF_OCTANT;m++) {
//      Key key = TMutKeyGen::get_key(0,0,0,0,m,0,0,0);
//      key.print();
//    }
//    for(int m=0;m<N_OCTANT;m++) {
//      Key key = TMutKeyGen::get_key(0,0,0,m,0,0,0,0);
//      key.print();
//    }
//    for(int m=0;m<N_STATION;m++) {
//      Key key = TMutKeyGen::get_key(0,0,m,0,0,0,0,0);
//      key.print();
//    }
//    for(int m=0;m<N_ARM;m++) {
//      Key key = TMutKeyGen::get_key(0,m,0,0,0,0,0,0);
//      key.print();
//    }
#endif
  
  //    std::set<PHKey::key_type> key_set;
  for(int i=0;i<N_ARM;i++) {
    for(int j=0;j<N_STATION;j++) {
      for(int k=0;k<N_OCTANT;k++) {
        for(int l=0;l<N_HALF_OCTANT;l++) {
	  for(int m=0;m<N_GAP;m++) {
	    for(int n=0;n<N_CATHODE;n++) {
	      PHKey::key_type key = TMutKeyGen::get_compact_key(0,i,j,k,l,m,n);
	      key.print();
	    }
	  }
	}
      }
    }
  }

}
/*! \endcode */




