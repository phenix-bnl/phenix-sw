// $Id: SvxSnglPisaHitv1.h,v 1.2 2011/02/08 23:47:27 youzy Exp $  
#ifndef __SVXSNGLHITV1_H_
#define __SVXSNGLHITV1_H_

/*!
  \file SvxSnglPisaHitv1.h
  \brief Forward vertex phool interface to pisa hit
  \author Sasha Lebedev (lebedev@iastate.edu)
  \version $Revision: 1.2 $
  \date $Date: 2011/02/08 23:47:27 $
*/

#include "PHObject.h"
#include "SvxSnglPisaHit.h"

//! Forward vertex phool interface to pisa hit
class SvxSnglPisaHitv1 : public SvxSnglPisaHit
{
  
  public:
  
  //! constructor
  SvxSnglPisaHitv1();
    
  //! copy constructor from pointer to base class
  SvxSnglPisaHitv1( const SvxSnglPisaHit* );
  
  //! copy constructor from reference to base class
  SvxSnglPisaHitv1( const SvxSnglPisaHit& );
  
  //! destructor
  virtual ~SvxSnglPisaHitv1() {}
  
  int GetSvxCount() const
  {return SvxCount; }
  
  void SetSvxCount( const int val) 
  {SvxCount = val; return;}
  
  int GetMctrack() const  
  {return mctrack;}
  
  void SetMctrack(const int val) 
  {mctrack=val;}
  
  int GetIdPart() const 
  {return idPart;}
  
  void SetIdPart(const int val)  
  {idPart=val;}
  
  float GetXGlobal() const 
  {return xyzglobal[0];}
  
  void SetXGlobal(const float val) 
  {xyzglobal[0]=val;}
  
  float GetYGlobal() const 
  {return xyzglobal[1];}
  
  void SetYGlobal(const float val) 
  {xyzglobal[1]=val;}
  
  float GetZGlobal() const 
  {return xyzglobal[2];}
  
  void SetZGlobal(const float val)
  {xyzglobal[2]=val;}
  
  float GetDele() const  
  {return dele;}
  
  void SetDele(const float val)  
  {dele=val;}
  
  float GetPmomX() const 
  {return pmomxyz[0];}
  
  void SetPmomX(const float val) 
  {pmomxyz[0]=val;}
  
  float GetPmomY() const
  {return pmomxyz[1];}
  
  void SetPmomY(const float val) 
  {pmomxyz[1]=val;}
  
  float GetPmomZ() const 
  {return pmomxyz[2];}
  
  void SetPmomZ(const float val)  
  {pmomxyz[2]=val;}
  
  float GetTof() const  
  {return timeOfFlight;}
  
  void SetTof(const float val) 
  {timeOfFlight=val;}
  
  float GetXLocalIn() const 
  {return xyzlocalIn[0];}
  
  void SetXLocalIn(const float val)  
  {xyzlocalIn[0]=val;}
  
  float GetYLocalIn() const 
  {return xyzlocalIn[1];}
  
  void SetYLocalIn(const float val)  
  {xyzlocalIn[1]=val;}
  
  float GetZLocalIn() const 
  {return xyzlocalIn[2];}
  
  void SetZLocalIn(const float val)  
  {xyzlocalIn[2]=val;}
  
  float GetXLocalOut() const  
  {return xyzlocalOut[0];}
  
  void SetXLocalOut(const float val) 
  {xyzlocalOut[0]=val;}
  
  float GetYLocalOut() const  
  {return xyzlocalOut[1];}
  
  void SetYLocalOut(const float val) 
  {xyzlocalOut[1]=val;}
  
  float GetZLocalOut() const  
  {return xyzlocalOut[2];}
  
  void SetZLocalOut(const float val) 
  {xyzlocalOut[2]=val;}
  
  float GetXGlobalIn() const 
  {return xyzglobalIn[0];}
  
  void SetXGlobalIn(const float val)  
  {xyzglobalIn[0]=val;}
  
  float GetYGlobalIn() const 
  {return xyzglobalIn[1];}
  
  void SetYGlobalIn(const float val)  
  {xyzglobalIn[1]=val;}
  
  float GetZGlobalIn() const 
  {return xyzglobalIn[2];}
  
  void SetZGlobalIn(const float val)  
  {xyzglobalIn[2]=val;}
  
  float GetXGlobalOut() const 
  {return xyzglobalOut[0];}
  void SetXGlobalOut(const float val) 
  {xyzglobalOut[0]=val;}
  
  float GetYGlobalOut() const  
  {return xyzglobalOut[1];}
  
  void SetYGlobalOut(const float val) 
  {xyzglobalOut[1]=val;}
  
  float GetZGlobalOut() const  
  {return xyzglobalOut[2];}
  
  void SetZGlobalOut(const float val) 
  {xyzglobalOut[2]=val;}
  
  int GetHitVolume(const short int i) const  
  {return hitVolume[i];}
  
  void SetHitVolume(const short int i, const int val) 
  {hitVolume[i]=val;}
  
  int GetTrack() const  
  {return track;}
  
  void SetTrack(const int val) 
  {track=val;}
  
  int GetLayer() const  
  {return layer;}
  
  void SetLayer(const int val) 
  {layer=val;}
  
  int GetIsubevent() const  
  {return isubevent;}
  
  void SetIsubevent(const int val) 
  {isubevent=val;}
  
  int GetNfile() const  
  {return nfile;}
  
  void SetNfile(const int val)
  {nfile=val;}
  
  protected:
  
  //! track number filled after read of PISA hits files
  int mctrack; 
  
  //! GEANT particle ID  assigned from siliID
  int idPart;
  
  //! global hit position
  float xyzglobal[3];
  
  //! energy loss
  float dele; 
  
  //! momentum
  float pmomxyz[3];
  
  //! time of flight
  float timeOfFlight;
  
  //! entrance position in volume (local coordinates)
  float xyzlocalIn[3];
  
  //! exit position in volume (local coordinates) 
  float xyzlocalOut[3];
  
  //! entrance position in volume (global coordinates) 
  float xyzglobalIn[3];
  
  //! exit position in volume (global coordinates)  
  float xyzglobalOut[3];
  
  //! volume id
  int hitVolume[9];
  
  //! track in subevent
  int track; 
  
  //! layer (*not* counting from 0)
  int layer;
  
  //! subevent number
  int isubevent; 
  
  //! input file id.
  int nfile;
  
  //! object counter
  static int SvxCount;
  
  ClassDef(SvxSnglPisaHitv1,1)
  
};
#endif

