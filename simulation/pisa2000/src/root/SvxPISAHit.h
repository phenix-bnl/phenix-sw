#ifndef _SvxPISAHit_
#define _SvxPISAHit_
// $Id: SvxPISAHit.h,v 1.4 2011/02/08 22:44:04 hubert Exp $

/*!
  \file  SvxPISAHit.h
  \brief container for pisa hits
  \author  V. L. Rykov, H. Van Hecke
  \version $Revision: 1.4 $
  \date    $Date: 2011/02/08 22:44:04 $
*/


#include <TObject.h>
#include <vector>

//! container for pisa svx and fvtx hits
class SvxPISAHit : public TObject 
{

  private:
  
  //! Track number filled after read of PISA hits files
  Int_t mctrack; 
  
  //! GEANT particle ID assigned from siliID
  Int_t idPart;  
  Float_t xyzglobal[3];
  Float_t dele; 
  Float_t pmomxyz[3];
  
  //! Corrected by V. L. Rykov 09/03/2003
  Float_t timeOfFlight;
  Float_t xyzlocalIn[3];
  Float_t xyzlocalOut[3];
  Float_t xyzglobalIn[3];
  Float_t xyzglobalOut[3];
  Int_t hitVolume[9];
  
  //! track in subevent
  Int_t track;   
  
  //! layer (*not* counting from 0) 
  Int_t layer;  
  Int_t isubevent; 
  Int_t nfile;
    
  //! static interface
  static std::vector<SvxPISAHit> _hits;

  public:
  
  //! empty constructor
  SvxPISAHit();

  //! constructor
  SvxPISAHit(
    Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Float_t timeOfFlight,
    Float_t xyzlocalIn[],  Float_t xyzlocalOut[],
    Float_t xyzglobalIn[], Float_t xyzglobalOut[],
    Int_t hitVolume[],
    Int_t track, Int_t layer, Int_t siliID,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 

  //! destructor
  virtual ~SvxPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetSvxCount()
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const SvxPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static SvxPISAHit *GetSvxHitEvt() 
  {return _hits.empty() ? 0: &_hits[0]; }
  
  static void SvxClear() 
  { _hits.clear(); }
  
  //@}
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetIdPart(const Int_t siliID) 
  { idPart = siliID; }
  
  Int_t GetIdPart() const 
  { return idPart; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Int_t GetLayer() const
  { return layer; }
  
  Float_t GetX() const 
  { return xyzglobal[0]; }
  
  Float_t GetY() const 
  { return xyzglobal[1]; }
  
  Float_t GetZ() const 
  { return xyzglobal[2]; }
  
  Float_t GetDele() const 
  { return dele; }
  
  Float_t GetPx() const 
  { return pmomxyz[0]; }
  
  Float_t GetPy() const 
  { return pmomxyz[1]; }
  
  Float_t GetPz() const 
  { return pmomxyz[2]; }
  
  Float_t GetTof() const
  { return timeOfFlight; }
  
  Float_t GetXLI() const
  { return xyzlocalIn[0]; }
  
  Float_t GetYLI() const 
  { return xyzlocalIn[1]; }
  
  Float_t GetZLI() const 
  { return xyzlocalIn[2]; }
  
  Float_t GetXLO() const 
  { return xyzlocalOut[0]; }
  
  Float_t GetYLO() const 
  { return xyzlocalOut[1]; }
  
  Float_t GetZLO() const 
  { return xyzlocalOut[2]; }
  
  Float_t GetXGI() const
  { return xyzglobalIn[0]; }
  
  Float_t GetYGI() const 
  { return xyzglobalIn[1]; }
  
  Float_t GetZGI() const 
  { return xyzglobalIn[2]; }
  
  Float_t GetXGO() const 
  { return xyzglobalOut[0]; }
  
  Float_t GetYGO() const
  { return xyzglobalOut[1]; }
  
  Float_t GetZGO() const 
  { return xyzglobalOut[2]; }
  
  Int_t GetHitVol0() const 
  { return hitVolume[0]; }
  
  Int_t GetHitVol1() const 
  { return hitVolume[1]; }
  
  Int_t GetHitVol2() const 
  { return hitVolume[2]; }
  
  Int_t GetHitVol3() const 
  { return hitVolume[3]; }
  
  Int_t GetHitVol4() const 
  { return hitVolume[4]; }
  
  Int_t GetHitVol5() const 
  { return hitVolume[5]; }
  
  Int_t GetHitVol6() const 
  { return hitVolume[6]; }
  
  Int_t GetHitVol7() const 
  { return hitVolume[7]; }
  
  Int_t GetHitVol8() const 
  { return hitVolume[8]; }
    
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(SvxPISAHit,1)
};

#endif
