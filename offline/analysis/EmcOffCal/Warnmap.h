#ifndef WARNMAP_H__
#define WARNMAP_H__

#include "EmcAnaCommon.h"

class Warnmap {
  Int_t m_warnmap[EmcAnaCommon::N_ARMSECT][EmcAnaCommon::N_YPOS_PBGL][EmcAnaCommon::N_ZPOS_PBGL];

 public:
     Warnmap();
     virtual ~Warnmap(){}

     void ResetMap();
     void ReadMap(const Char_t* fname);

     Int_t IsBad(Int_t as, Int_t y, Int_t z);
     Int_t IsHot(Int_t as, Int_t ys, Int_t z);
     Int_t IsDead(Int_t as, Int_t y, Int_t z);
     Int_t IsUncalib(Int_t as, Int_t y, Int_t z);
     Int_t IsAroundHot(Int_t as, Int_t y, Int_t z);
     Int_t IsAroundDead(Int_t as, Int_t y, Int_t z);
     Int_t IsAroundUncalib(Int_t as, Int_t y, Int_t z);
     Int_t IsEdge(Int_t as, Int_t y, Int_t z);

     Double_t GetGoodFraction(Int_t as, Int_t bl_include_edge = true);

};

inline Int_t Warnmap::IsBad(Int_t as, Int_t y, Int_t z)
{
     return ( IsHot(as, y, z) || IsDead(as, y, z) || IsUncalib(as, y, z) || 
              IsAroundHot(as, y, z) || IsAroundDead(as, y, z) || 
              IsAroundUncalib(as, y, z) || IsEdge(as, y, z) );
};

inline Int_t Warnmap::IsHot(Int_t as, Int_t y, Int_t z) 
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_HOT);
};

inline Int_t Warnmap::IsDead(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_DEAD);
};

inline Int_t Warnmap::IsUncalib(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_UNCALIB);
};

inline Int_t Warnmap::IsAroundHot(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_AROUND_HOT);
};

inline Int_t Warnmap::IsAroundDead(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_AROUND_DEAD);
};

inline Int_t Warnmap::IsAroundUncalib(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_AROUND_UNCALIB);
};

inline Int_t Warnmap::IsEdge(Int_t as, Int_t y, Int_t z)
{
     return (m_warnmap[as][y][z] & EmcAnaCommon::MASK_EDGE);
};

#endif // WARNMAP_H__
