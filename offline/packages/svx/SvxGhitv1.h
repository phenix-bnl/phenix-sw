// =================
// FILE: SvxGhitv1.h
// =================

#ifndef __SVXGHITV1_HH_
#define __SVXGHITV1_HH_

#include "SvxGhit.h"
#include "SvxGhitList.h"

/**
 * Implementation class (ver. 1) of SvxGhit (SVX GEANT/PISA hit).
 *
 * @date  Original version by Jeffery Mitchell as of 20-Nov-2003
 * @date  Modified by V. L. Rykov on 08-Feb-2004
 * @date  Modified by V. L. Rykov on 10-May-2004: Sorting related stuff is added.
 */
class SvxGhitv1 : public SvxGhit
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitv1(SvxGhitList* lst = NULL, SvxGhit* ghit = NULL);
  SvxGhitv1(SvxGhit* ghit);
  virtual ~SvxGhitv1()
    {/*std::cout << "SvxGhitv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  // """"""""""""""""""""
  void set_hitID      (const int val)
    { hitID      =         val; if ( ghitList ) ghitList->unSort() ;}
  void set_svxSection (const int val)
    { svxSection = (short) val; if ( ghitList ) ghitList->unSort() ;}
  void set_layer      (const int val)
    { layer      = (short) val; if ( ghitList ) ghitList->unSort() ;}
  void set_ladder     (const int val)
    { ladder     = (short) val; if ( ghitList ) ghitList->unSort() ;}
  void set_sensor     (const int val)
    { sensor     = (short) val; if ( ghitList ) ghitList->unSort() ;}

  void set_isubevent   (const int   val) {isubevent  = val ;}
  void set_nfile       (const int   val) {nfile      = val ;}
  void set_mctrack     (const int   val) {mctrack    = val ;}
  void set_idPart      (const int   val) {idPart     = val ;}
  void set_track       (const int   val) {track      = val ;}
  void set_dele        (const float val) {dele       = val ;}
  void set_tof         (const float val) {tof        = val ;}
  void set_xyzglobal   (const int ind, const float vl) {xyzglobal[ind]   = vl;}
  void set_xyzlocalin  (const int ind, const float vl) {xyzlocalin[ind]  = vl;}
  void set_xyzlocalout (const int ind, const float vl) {xyzlocalout[ind] = vl;}
  void set_pmomxyz     (const int ind, const float vl) {pmomxyz[ind]     = vl;}

  // Get the data members
  // """"""""""""""""""""
  int get_isubevent    () const { return isubevent; }
  int get_nfile        () const { return nfile;     }
  int get_mctrack      () const { return mctrack;   }
  int get_idPart       () const { return idPart;    }
  int get_track        () const { return track;     }
  float get_dele       () const { return dele;      }
  float get_tof        () const { return tof;       }
  float get_xyzglobal   (const unsigned int ind) const
    { return xyzglobal[ind]; }
  float get_xyzlocalin  (const unsigned int ind) const
    { return xyzlocalin[ind]; }
  float get_xyzlocalout (const unsigned int ind) const
    { return xyzlocalout[ind]; }
  float get_pmomxyz     (const unsigned int ind) const
    { return pmomxyz[ind];   }

  // Sortability
  Int_t Compare(const TObject* svxhit) const { return SvxHit::Compare(svxhit);}
  void  set_ListPointer (SvxGhitList* lst) { ghitList = lst ;}

  // Methods
  // """""""
  virtual SvxHit* Clone()           { return new SvxGhitv1(this); }
  virtual void    Copy(SvxHit* hit);
  void print() const;
//  Int_t Compare(const TObject* svxhit) const;

 protected:

  // Data member definition
  int isubevent;          ///< PISA subevent number
  int nfile;              ///< PISA file number
  int mctrack;            ///< Monte Carlo track id
  int idPart;             ///< GEANT Particle ID
  int track;              ///< Track number
  float dele;             ///< Energy loss
  float tof;              ///< Time-of-flight
  float xyzglobal[3];     ///< Average global coordinates
  float xyzlocalin[3];    ///< Entry local coordinates
  float xyzlocalout[3];   ///< Exit local coordinates
  float pmomxyz[3];       ///< Momentum vector

  /// Pointer to the container
  SvxGhitList* ghitList;  //!

  //---
  ClassDef(SvxGhitv1,1)
};
#endif
