// ===============
// FILE: FvtxTrigEmulator.C
// ===============

// ******************************************************
//
// Class: FvtxTrigEmulator implementation
//
// Author:  D. McGlinchey (darren.mcglinchey@colorado.edu)
//
// Revisions: 21 Apr 2016 - initial version
//
// ***************************************************************************

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>

#include <FvtxTrigEmulator.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <recoConsts.h>

#include <TMutNode.h>
#include <TFvtxHitMap.h>
#include <TFvtxTrigEmulInfo_v1.h>

#include <RunToTime.hh>

using namespace std;


//------------------------------------------------------------------------------
FvtxTrigEmulator::FvtxTrigEmulator(const string &name):
  SubsysReco(name),
  d_hitmap(NULL),
  d_triginfo(NULL),
  _timer(PHTimeServer::get()->insert_new(name))
{

  memset(m_hitarray, 0, sizeof(m_hitarray));
  memset(m_trigStation, 0, sizeof(m_trigStation));
  memset(m_trigSector, 0, sizeof(m_trigSector));
  memset(m_trigFEM, 0, sizeof(m_trigFEM));
  memset(m_trigCage, 0, sizeof(m_trigCage));
  memset(m_trigArm, 0, sizeof(m_trigArm));
  m_trig = false;

  //initialize the trigger parameters
  m_cfgCoincMode = 0;
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    m_cfgAdcThre[iarm] = 1;
    m_cfgSectorStationThre[iarm] = 3;
    m_cfgStationNhitThre[iarm] = 15;
    m_cfgCageThre[iarm] = 9;
    m_cfgFemLogic[iarm] = 0;
    m_cfgArmLogic[iarm] = 0;
    m_cfgMode[iarm] = 0;
  }

}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
FvtxTrigEmulator::~FvtxTrigEmulator()
{
  return;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::Init(PHCompositeNode *topNode)
{
  if (verbosity > 0) cout << PHWHERE << ": Execution started..." << endl;

  // Create the node tree
  CreateNodeTree(topNode);

  // print the trigger configuration
  PrintTrigConfig();

  if (verbosity > 0) cout << PHWHERE << ": Execution completed." << endl;

  return EVENT_OK;

}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Run-dependent initialization
int FvtxTrigEmulator::InitRun(PHCompositeNode *topNode)
{

  if (verbosity > 0) cout << PHWHERE << ": Execution started..." << endl;


  if (verbosity > 0) cout << PHWHERE << ": Execution completed." << endl;

  return EVENT_OK;

}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
int FvtxTrigEmulator::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  if (verbosity > 0) cout << PHWHERE << ": Execution started..." << endl;

  // Get the required data nodes from the node tree
  // ... need to do something with the returned value ...
  GetNodes(topNode);


  // Fill the hit array from the data nodes
  // ... need to do something with the returned value ...
  FillHitArray();


  // Make the trigger decision based on the logic of the current
  // trigger settings
  // ... need to do something with the returned value ...
  EmulateTriggerDecision();


  // Fill the TFvtxTrigEmulInfo object and put it on the node tree
  // ... need to do something with the returned value ...
  SaveTriggerDecision();

  _timer.get()->stop();
  if (verbosity > 0) {cout << PHWHERE << ": Event processed." << endl;}

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::CreateNodeTree(PHCompositeNode *topNode)
{

  // FVTX working space _fvtx_node
  PHNodeIterator nodeItr(topNode);

  PHCompositeNode* _fvtx_node =
    static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if (!_fvtx_node) {
    _fvtx_node = new PHCompositeNode("FVTXOO");
    topNode->addNode(_fvtx_node);
  }

  // Node to hold TFvtxTrigEmulInfo object
  PHIODataNode<PHObject>* TFvtxTrigEmulInfoNode = NULL;
  TFvtxTrigEmulInfoNode =
    (PHIODataNode<PHObject>*)nodeItr.findFirst("PHIODataNode",
        "TFvtxTrigEmulInfo");
  if (!TFvtxTrigEmulInfoNode)
  {
    TFvtxTrigEmulInfo* mfvtxtrigemulinfo = new TFvtxTrigEmulInfo_v1();
    TFvtxTrigEmulInfoNode =
      new PHIODataNode<PHObject>(mfvtxtrigemulinfo,
                                 "TFvtxTrigEmulInfo",
                                 "PHObject");
    _fvtx_node->addNode(TFvtxTrigEmulInfoNode);
  }

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::GetNodes(PHCompositeNode *topNode)
{

  //------------------------------------------------------------------
  d_hitmap = NULL;
  d_hitmap = findNode::getClass<TFvtxHitMap>(topNode, "TFvtxHitMap");
  if (d_hitmap == NULL)
  {
    cout << PHWHERE << ": TFvtxHitMap node not found" << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------------------

  //------------------------------------------------------------------
  d_triginfo = NULL;
  d_triginfo = findNode::getClass<TFvtxTrigEmulInfo>(topNode, "TFvtxTrigEmulInfo");
  if (d_triginfo == NULL)
  {
    cout << PHWHERE << ": TFvtxTrigEmulInfo node not found" << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------------------

  return EVENT_OK;
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
int FvtxTrigEmulator::FillHitArray()
{

  //First zero the array
  memset(m_hitarray, 0, sizeof(m_hitarray));

  //Fill the array from the hit map
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
        {
          TFvtxHitMap::iterator hit_iter = d_hitmap->get(
                                             iarm, icage, istation, isector);

          // Get the total number of hits in this sector
          while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
          {
            TFvtxHit *hit = (TFvtxHit*) hit_ptr->get();

            // Make sure the hit passes the ADC threshold for the trigger
            if (hit->get_adc() >= m_cfgAdcThre[iarm])
              m_hitarray[iarm][icage][isector][istation]++;
          } // while
        } // istation
      } // isector
    } // icage
  }// iarm

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::EmulateTriggerDecision()
{

  // reset the trigger decisions
  memset(m_trigStation, 0, sizeof(m_trigStation));
  memset(m_trigSector, 0, sizeof(m_trigSector));
  memset(m_trigFEM, 0, sizeof(m_trigFEM));
  memset(m_trigCage, 0, sizeof(m_trigCage));
  memset(m_trigArm, 0, sizeof(m_trigArm));
  m_trig = false;

  // Trigger logic should go in this function
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {

    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {

      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        // Get the number of stations which fired
        int nstat = 0;
        int nhits = 0;
        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
        {
          nhits += m_hitarray[iarm][icage][isector][istation];

          if (m_hitarray[iarm][icage][isector][istation] >= m_cfgStationNhitThre[iarm])
          {
            m_trigStation[iarm][icage][isector][istation] = true;
            nstat++;
          }
        } // istation


        // did the number of fired stations fire the sector?
        // "Multitrack" mode, compare stations above threshold
        if (m_cfgMode[iarm] == 0 && nstat >= m_cfgSectorStationThre[iarm])
          m_trigSector[iarm][icage][isector] = true;
        // "Multihit" mode, compare total number of hits in all sectors
        if (m_cfgMode[iarm] == 1 && nhits >= m_cfgStationNhitThre[iarm])
          m_trigSector[iarm][icage][isector] = true;


      }// isector

      // deal with FEM trigger for the given cage
      int nfem = 0;
      for (int ifem = 0; ifem < FVTXOO::MAX_SECTOR / 2; ifem++)
      {
        int sctr = 2 * ifem;

        // OR
        if ( m_cfgFemLogic[iarm] == 0 &&
             (m_trigSector[iarm][icage][sctr] || m_trigSector[iarm][icage][sctr + 1]) )
        {
          m_trigFEM[iarm][icage][ifem] = true;
          nfem++;
        }
        // AND
        if ( m_cfgFemLogic[iarm] == 1 &&
             (m_trigSector[iarm][icage][sctr] && m_trigSector[iarm][icage][sctr + 1]) )
        {
          m_trigFEM[iarm][icage][ifem] = true;
          nfem++;
        }

      } // ifem

      // Did the cage trigger fire?
      if (nfem >= m_cfgCageThre[iarm])
        m_trigCage[iarm][icage] = true;

    } // icage


    // get the arm decision
    if (m_cfgArmLogic[iarm] == 0) // OR
    {
      if (m_trigCage[iarm][0] || m_trigCage[iarm][1])
        m_trigArm[iarm] = true;
    }
    else  // AND
    {
      if (m_trigCage[iarm][0] && m_trigCage[iarm][1])
        m_trigArm[iarm] = true;
    }

  }



  // Final trigger decision
  if ( m_cfgCoincMode == 0 &&
       ( m_trigArm[0] || m_trigArm[1]) )
  {
    m_trig = true;
  }
  if ( m_cfgCoincMode == 1 &&
       ( m_trigArm[0] && m_trigArm[1]) )
  {
    m_trig = true;
  }




  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::SaveTriggerDecision()
{
  // Store trigger info
  if (verbosity > 0)
    cout << PHWHERE << ": Setting trigger decisions " << endl;

  d_triginfo->Reset();
  d_triginfo->set_trig(m_trig);

  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    d_triginfo->set_trigArm(m_trigArm[iarm], iarm);

    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      d_triginfo->set_trigCage(m_trigCage[iarm][icage], iarm, icage);

      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        d_triginfo->set_trigSector(m_trigSector[iarm][icage][isector],
                                   iarm, icage, isector);

        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
        {
          d_triginfo->set_trigStation(m_trigStation[iarm][icage][isector][istation],
                                      iarm, icage, isector, istation);
        } // istation
      } // isector

      for (int ifem = 0; ifem < FVTXOO::MAX_SECTOR / 2; ifem++)
      {
        d_triginfo->set_trigFEM(m_trigFEM[iarm][icage][ifem],
                                iarm, icage, ifem);
      } // ifem

    } // icage
  } // iarm

  if (verbosity > 0)
  {
    PrintHits();
    // PrintTrigDecision();
  }

  return EVENT_OK;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::PrintHits()
{
  cout << "****************************************************" << endl;
  cout << PHWHERE << ": Hits" << endl;
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << " Arm" << iarm << " Trig: " << m_trigArm[iarm] << endl;
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      cout << "    Cage" << icage << " Trig: " << m_trigCage[iarm][icage] << endl;
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        cout << "        Sector" << isector << " Trig: " << m_trigSector[iarm][icage][isector] << endl;
        cout << "           Station Nhits - ";
        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
        {
          cout << " " << m_hitarray[iarm][icage][isector][istation];          
        } // istation
        cout << endl;
      } // isector
    } // icage
  }// iarm

  cout << "****************************************************" << endl;

}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::PrintTrigDecision()
{
  cout << "****************************************************" << endl;
  cout << PHWHERE << ": Trigger Decisions" << endl;
  cout << "** Trig: " << m_trig << endl;
  cout << "** Arm : ";
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
    cout << " " << iarm << ": " << m_trigArm[iarm] << endl;
  cout << "** Cage : ";
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << " Arm" << iarm << ":" << endl;
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
      cout << "    " << icage << ": " << m_trigCage[iarm][icage] << endl;
  }
  cout << "** FEM : ";
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << " Arm" << iarm << ":" << endl;
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      cout << "    Cage" << icage << ":" << endl;
      for (int ifem = 0; ifem < FVTXOO::MAX_SECTOR / 2; ifem++)
        cout << "        " << ifem << ": " << m_trigFEM[iarm][icage][ifem] << endl;
    }
  }
  cout << "** Sector : ";
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << " Arm" << iarm << ":" << endl;
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      cout << "    Cage" << icage << ":" << endl;
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
        cout << "        " << isector << ": " << m_trigSector[iarm][icage][isector] << endl;
    }
  }
  cout << endl;
  cout << "** Station : ";
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << " Arm" << iarm << ":" << endl;
    for (int icage = 0; icage < FVTXOO::MAX_CAGE; icage++)
    {
      cout << "    Cage" << icage << ":" << endl;
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR; isector++)
      {
        cout << "        Sector" << isector << ":" << endl;
        for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
          cout << "            " << istation << ": " << m_trigStation[iarm][icage][isector][istation] << endl;
      }
    }
  }
  cout << endl;
  cout << "****************************************************" << endl;

}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void FvtxTrigEmulator::PrintTrigConfig()
{
  cout << "****************************************************" << endl;
  cout << PHWHERE << ": FvtxTrigEmulator configuration" << endl;
  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
  {
    cout << "**** ARM = " << iarm << endl;
    cout << "** ADC Threshold                 : >=" << m_cfgAdcThre[iarm] << endl;
    cout << "** Station Nhit Threshold        : >=" << m_cfgStationNhitThre[iarm] << endl;
    cout << "** Sector NStation Threshold     : >=" << m_cfgSectorStationThre[iarm] << endl;
    cout << "** Fem Logic                     :   ";
    if (m_cfgFemLogic[iarm] == 0)
      cout << "OR " << endl;
    else
      cout << "AND " << endl;
    cout << "** Cage NFEM Threshold           : >=" << m_cfgCageThre[iarm] << endl;
    cout << "** Arm Logic (btw Cages)         :   ";
    if (m_cfgArmLogic[iarm] == 0)
      cout << "OR " << endl;
    else
      cout << "AND " << endl;
  }
  cout << "**** Coincidence Mode (btw Arms) :   ";
  if (m_cfgCoincMode == 0)
    cout << "OR " << endl;
  else
    cout << "AND " << endl;
  cout << "****************************************************" << endl;

}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_TrigConfigFileName(const std::string & name, int arm)
{
  if (armValid(arm))
    m_trigConfigFileName[arm] = name;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::ReadTrigConfigFromFile()
{
  return EVENT_OK;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_AdcThreshold(int thre, int arm)
{
  if (armValid(arm))
    m_cfgAdcThre[arm] = thre;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_SectorThreshold(int thre, int arm)
{
  if (armValid(arm))
    m_cfgSectorStationThre[arm] = thre;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_StationThreshold(int thre, int arm)
{
  if (armValid(arm))
    m_cfgStationNhitThre[arm] = thre;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_CageThreshold(int thre, int arm)
{
  if (armValid(arm))
    m_cfgCageThre[arm] = thre;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_FemLogic(TString log, int arm)
{
  if (armValid(arm))
  {
    if (log.Contains("OR"))
      m_cfgFemLogic[arm] = 0;
    if (log.Contains("AND"))
      m_cfgFemLogic[arm] = 1;
  }
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_ArmLogic(TString log, int arm)
{
  if (armValid(arm))
  {
    if (log.Contains("OR"))
      m_cfgArmLogic[arm] = 0;
    if (log.Contains("AND"))
      m_cfgArmLogic[arm] = 1;
  }
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_Mode(TString log, int arm)
{
  if (armValid(arm))
  {
    if (log.Contains("MULTITRACK"))
      m_cfgMode[arm] = 0;
    if (log.Contains("MULTIHIT"))
      m_cfgMode[arm] = 1;
  }
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void FvtxTrigEmulator::set_CoincidenceMode(TString log)
{
  if (log.Contains("OR"))
    m_cfgCoincMode = 0;
  if (log.Contains("AND"))
    m_cfgCoincMode = 1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::get_AdcThreshold(int arm)
{
  if (armValid(arm))
    return m_cfgAdcThre[arm];
  else
    return -1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::get_SectorThreshold(int arm)
{
  if (armValid(arm))
    return m_cfgSectorStationThre[arm];
  else
    return -1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::get_StationThreshold(int arm)
{
  if (armValid(arm))
    return m_cfgStationNhitThre[arm];
  else
    return -1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int FvtxTrigEmulator::get_CageThreshold(int arm)
{
  if (armValid(arm))
    return m_cfgCageThre[arm];
  else
    return -1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
TString FvtxTrigEmulator::get_FemLogic(int arm)
{
  if (armValid(arm))
  {
    if (m_cfgFemLogic[arm] == 0)
      return "OR";
    else if (m_cfgFemLogic[arm] == 1)
      return "AND";
    else
      return "";
  }
  else
    return "";
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
TString FvtxTrigEmulator::get_ArmLogic(int arm)
{
  if (armValid(arm))
  {
    if (m_cfgArmLogic[arm] == 0)
      return "OR";
    else if (m_cfgArmLogic[arm] == 1)
      return "AND";
    else
      return "";
  }
  else
    return "";
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
TString FvtxTrigEmulator::get_Mode(int arm)
{
  if (armValid(arm))
  {
    if (m_cfgMode[arm] == 0)
      return "MULTITRACK";
    else if (m_cfgMode[arm] == 1)
      return "MULTIHIT";
    else
      return "";
  }
  else
    return "";
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
TString FvtxTrigEmulator::get_CoincidenceMode()
{
  if (m_cfgCoincMode == 0)
    return "OR";
  else
    return "AND";
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
bool FvtxTrigEmulator::armValid(int arm)
{
  if (arm == 0 || arm == 1)
    return true;
  else
  {
    cout << PHWHERE << ": Input arm index not valid."
         << " Must be 0 || 1. Input=" << arm
         << endl;
    return false;
  }
}
//------------------------------------------------------------------------------
