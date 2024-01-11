#ifndef __TRXNPSCINTGEOM_H__
#define __TRXNPSCINTGEOM_H__

// $Id: TRxnpScintGeom.h,v 1.2 2007/03/10 17:12:48 phnxmuid Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpScintGeom.h
        \brief Container class to geom info for each scintilator
        \author Chun Zhang
        \version $Revision: 1.2 $
        \date    $Date: 2007/03/10 17:12:48 $
*/
//////////////////////////////////////////////////////////////////

class TRxnpScintGeom
{
 public:
  enum SURVEY{ INNER_RIGHT = 4, INNER_LEFT = 5, MIDDLE_RIGHT = 1, MIDDLE_LEFT = 2, OUTER_RIGHT = 0, OUTER_LEFT = 3, NSURVEYPOSITION = 6};
  
  // constructor
  //
  TRxnpScintGeom(int arm,
		 int ring,
		 int scint):
    _arm(arm),
    _ring(ring),
    _scint(scint)
    {
      for(int ipos = 0; ipos < NSURVEYPOSITION; ipos++)
	{
	  _phi[ipos] = 0;
	  _theta[ipos] = 0;
	  _z[ipos] = 0;
	}
    }
  // destructor
  //
  virtual ~TRxnpScintGeom() {;}
  
  // public accessor
  //
  // getter
  //
  inline int get_arm() const { return _arm;}
  inline int get_ring() const { return _ring;}
  inline int get_scint() const { return _scint;}

  // take average of the survey positions. Note middle is shared
  //
  float get_theta() const;
  float get_phi() const;
  float get_z() const;

  inline float get_theta(int ipos) const { return _theta[ipos];}
  inline float get_phi(int ipos) const { return _phi[ipos];}
  inline float get_z(int ipos) const { return _z[ipos];}

  // setter
  //
  inline void set_theta(int ipos, float theta) { _theta[ipos] = theta;}
  inline void set_phi(int ipos, float phi) { _phi[ipos] = phi;}
  inline void set_z(int ipos, float z) { _z[ipos] = z;}
  
  private:
  // detector location.
  //
  int _arm;
  int _ring;
  int _scint;

  // Geometry survey
  //
  float _phi[NSURVEYPOSITION];
  float _theta[NSURVEYPOSITION];
  float _z[NSURVEYPOSITION];    

};

#endif
