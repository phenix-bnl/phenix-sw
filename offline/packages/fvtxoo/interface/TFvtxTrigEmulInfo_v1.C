
#include "TFvtxTrigEmulInfo_v1.h"

#include <iostream>

using namespace std;


ClassImp(TFvtxTrigEmulInfo_v1)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
TFvtxTrigEmulInfo_v1::TFvtxTrigEmulInfo_v1(TFvtxTrigEmulInfo* info)
  : TFvtxTrigEmulInfo(info)
{
  Reset();
}

void TFvtxTrigEmulInfo_v1::Reset()
{

  m_trig = false;
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    m_trigArm[iarm] = false;

    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      m_trigCage[iarm][icage] = false;

      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        m_trigSector[iarm][icage][isector] = false;

        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
        {
          m_trigStation[iarm][icage][isector][istation] = false;
        }
      }

      for (int ifem = 0; ifem < FVTXOO::MAX_SECTOR / 2; ifem++)
      {
        m_trigFEM[iarm][icage][ifem] = false;
      }
    }
  }

}

void TFvtxTrigEmulInfo_v1::set_trig(bool trig)
{
  m_trig = trig;
}

void TFvtxTrigEmulInfo_v1::set_trigArm(bool trig,
                                       int arm)
{
  if (arm >= 0 && arm < FVTXOO::MAX_ARM)
    m_trigArm[arm] = trig;
  else
    cout << PHWHERE << ": Invalid inputs" << endl;
}

void TFvtxTrigEmulInfo_v1::set_trigCage(bool trig,
                                        int arm,
                                        int cage)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) )
    m_trigCage[arm][cage] = trig;
  else
    cout << PHWHERE << ": Invalid inputs" << endl;
}

void TFvtxTrigEmulInfo_v1::set_trigFEM(bool trig,
                                       int arm,
                                       int cage,
                                       int fem)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (fem >= 0 && fem < FVTXOO::MAX_SECTOR / 2) )
    m_trigFEM[arm][cage][fem] = trig;
  else
    cout << PHWHERE << ": Invalid inputs" << endl;
}

void TFvtxTrigEmulInfo_v1::set_trigSector(bool trig,
    int arm,
    int cage,
    int sector)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (sector >= 0 && sector < FVTXOO::MAX_SECTOR) )
    m_trigSector[arm][cage][sector] = trig;
  else
    cout << PHWHERE << ": Invalid inputs" << endl;
}

void TFvtxTrigEmulInfo_v1::set_trigStation(bool trig,
    int arm,
    int cage,
    int sector,
    int station)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (sector >= 0 && sector < FVTXOO::MAX_SECTOR) &&
       (station >= 0 && station < FVTXOO::MAX_STATION) )
    m_trigStation[arm][cage][sector][station] = trig;
  else
    cout << PHWHERE << ": Invalid inputs" << endl;
}


bool TFvtxTrigEmulInfo_v1::did_trigFire()
{
  return m_trig;
}

bool TFvtxTrigEmulInfo_v1::did_trigFireArm(int arm)
{
  if (arm >= 0 && arm < FVTXOO::MAX_ARM)
    return m_trigArm[arm];
  else
  {
    cout << PHWHERE << ": Invalid inputs" << endl;
    return false;
  }
}

bool TFvtxTrigEmulInfo_v1::did_trigFireCage(int arm,
    int cage)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) )
    return m_trigCage[arm][cage];
  else
  {
    cout << PHWHERE << ": Invalid inputs" << endl;
    return false;
  }
}

bool TFvtxTrigEmulInfo_v1::did_trigFireFEM(int arm,
    int cage,
    int fem)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (fem >= 0 && fem < FVTXOO::MAX_SECTOR / 2) )
    return m_trigFEM[arm][cage][fem];
  else 
  {
    cout << PHWHERE << ": Invalid inputs" << endl;
    return false;
  }
}

bool TFvtxTrigEmulInfo_v1::did_trigFireSector(int arm,
    int cage,
    int sector)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (sector >= 0 && sector < FVTXOO::MAX_SECTOR) )
    return m_trigSector[arm][cage][sector];
  else 
  {
    cout << PHWHERE << ": Invalid inputs" << endl;
    return false;
  }
}

bool TFvtxTrigEmulInfo_v1::did_trigFireStation(int arm,
    int cage,
    int sector,
    int station)
{
  if ( (arm >= 0 && arm < FVTXOO::MAX_ARM) &&
       (cage >= 0 && cage < FVTXOO::MAX_CAGE) &&
       (sector >= 0 && sector < FVTXOO::MAX_SECTOR) &&
       (station >= 0 && station < FVTXOO::MAX_STATION) )
    return m_trigStation[arm][cage][sector][station];
  else
  {
    cout << PHWHERE << ": Invalid inputs" << endl;
    return false;
  }
}


