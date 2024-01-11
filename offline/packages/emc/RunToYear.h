
#ifndef __RUNTOYEAR_H__
#define __RUNTOYEAR_H__


namespace {

  static int RunToYear(int runnumber)
  {
    if ( runnumber > 1 && runnumber < 20000 ) 
      {
	return 1;
      }
    else if ( runnumber >= 20000 && runnumber < 50000 ) 
      {
	// return 2;
	return 3; 
	// (temporary 18-SEP-2003) allow re-running Run2 data with new reco.
      }
    else if ( runnumber >= 50000 && runnumber < 95000 )
      {
	return 3;
      }
    else if ( runnumber >= 95000 )
      {
	return 4;
      }
    return -1;
  }
};




#endif /* !__RUNTOYEAR_H__ */
