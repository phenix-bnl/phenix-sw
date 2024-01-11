// Class: cglDetectorGeo (implementation)
// Created by: Jeffery T. Mitchell
// Description: Class that generates all detector geometries
//              that are used by global tracking.
//
// Completely reworked by Julia Velkovska julia@bnl.gov 04/18/02
// This class started out as a "generator" for central arm geometries.
// For alignment purposes in Run1,I had added methods to do geometry
// transformations at the stage of global tracks.
// Now, I remove these methods since the geomerty transformations are
// implemeted at the individual DGOs and we don't need this functionality
// here. In fact it is even dangerous, since the geometry we use should be
// identical in recostruction and global tracking. At the time of this
// revision, the MC geometry is also implemeted in the individual DGOs.
// From now on this class will be just a "collector" for central arm
// geometries. It will not generate anything by itself.
// All variables that were associated with the MC geometry and the
// STAF par geoPar tables are also removed from this object and the
// corresponding setting in preco/cglfuncs.C

#include <cglDetectorGeo.hh>
#include <padDetectorGeo.hh>
#include <PHsvxDetectorGeo.hh>
#include <TofGeometryObject.hh>
#include <AccGeometry.h>
#include <TofwGeometry.h>
#include <hbdDetectorGeo.hh>

#include <recoConsts.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <iostream>

using namespace std;
using namespace PHGeometry;

typedef PHIODataNode<TObject> ObjectNode_t;

// Default Constructor for cglDetectorGeo
cglDetectorGeo::cglDetectorGeo()
{
  Verbose = 0;
  memset(pc1Active,0,sizeof(pc1Active));
  memset(pc2Active,0,sizeof(pc2Active));
  memset(pc3Active,0,sizeof(pc3Active));
  memset(tecActive,0,sizeof(tecActive));
  memset(tofActive,0,sizeof(tofActive));
  memset(pbscActive,0,sizeof(pbscActive));
  memset(pbglActive,0,sizeof(pbglActive));
  memset(tofwActive,0,sizeof(tofwActive));
  memset(dchActive,0,sizeof(dchActive));
  memset(crkActive,0,sizeof(crkActive));
  memset(svxActive,0,sizeof(svxActive));
  SvxActive = 0;

  accActive = 0;
  memset(hbdActive,0,sizeof(hbdActive));
  tofGeoPanels = 0;
  tofwGeoPanels = 0;
  SvxUseAsciiFile = false;
}


short cglDetectorGeo::get_dchGeo(short iarm, PHCylinderSection *outCyl)
{
  cerr << PHWHERE << " IMPLEMENT ME !" << endl;
  return 0;
}

short cglDetectorGeo::get_pc1Geo(const short arm, PHPanel outPlane[]) const
{
  short i, nSector;

  nSector = 0;
  for (i = 0; i < padDetectorGeo::PC1MAXSECTPERARM; i++)
    {
      if (pc1Active[arm][i] == 1)
        {
          outPlane[nSector] = pc1Sectors[arm][i];
          nSector++;
        }
    }

  return nSector;
}

short cglDetectorGeo::get_pc2Geo(const short arm, PHPanel outPlane[]) const
{
  short i, nSector;

  nSector = 0;
  for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      if (pc2Active[arm][i] == 1)
        {
          outPlane[nSector] = pc2Sectors[arm][i];
          nSector++;
        }
    }

  return nSector;
}

short cglDetectorGeo::get_pc3Geo(const short arm, PHPanel outPlane[]) const
{

  short i, nSector;

  nSector = 0;
  for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      if (pc3Active[arm][i] == 1)
        {
          outPlane[nSector] = pc3Sectors[arm][i];
          nSector++;
        }
    }

  return nSector;
}



short
cglDetectorGeo::get_crkGeo(short arm, PHCylinderSection outCyl[])
{
  short nArm;

  outCyl[arm] = crkArm[arm];
  nArm = 1;

  return nArm;
}

short cglDetectorGeo::get_svxGeo(const short arm, const short layer, PHPanel outPlane[]) const
{
   short i, nSector;
   nSector = 0;
   for (i = 0; i < SVXLADDERNUMBER; i++)
     {
       if (svxActive[arm][layer][i] == 1)
         {
           outPlane[nSector] = svxLadders[arm][layer][i];
           nSector++;
         }
     }
 
   return nSector;
}


short cglDetectorGeo::get_tecGeo(short arm, PHPanel outPlane[])
{
  short nPanel, numberOfPanels = 0;
  if (tecGeoPanels ) // real TEC geometry
    {
      if (arm == 0) //in arm 0
        {
          nPanel = 0;
          for (int i = 0; i < TECMAXSIDE; i++)
            {
              for (int j = 0; j < TECMAXSECT; j++)
                {
                  if ( ((tecPanels[i][j]).getPoint(0)).getX() != 0)
                    {
                      outPlane[nPanel] = tecPanels[i][j];
                      nPanel++;
                    }
                }
            }
          numberOfPanels = nPanel;
        }
      else
        {
          //PHMessage("cglDetectorGeo::get_tecGeo", PHError, " No real geometry available but arm is active\n ");
          numberOfPanels = 0;
        }
    }
  else
    {

      PHMessage("cglDetectorGeo::get_tecGeo", PHError, " Real geometry was not fetched \n ");
      numberOfPanels = 0;
    }

  return numberOfPanels;
}

short
cglDetectorGeo::get_tecGeoOut(short arm, PHPanel outPlane[])
{
  short nPanel, numberOfPanels = 0;

  if (tecGeoPanelsOut && arm == 0) // real TEC geometry for arm 0
    {
      nPanel = 0;
      for (int i = 0; i < TECMAXSIDE; i++)
        {
          for (int j = 0; j < TECMAXSECT; j++)
            {
              if ( ((tecPanelsOut[i][j]).getPoint(0)).getX() != 0)
                {
                  outPlane[nPanel] = tecPanelsOut[i][j];
                  nPanel++;
                }
            }
        }
      numberOfPanels = nPanel;
    }
  else
    {
      numberOfPanels = 0;
    }

  return numberOfPanels;
}


short
cglDetectorGeo::get_tofGeo(const short arm, PHPanel outPlane[]) const
{
  short i, nSector, nPanel, numberOfPanels = 0;
  if (tofGeoPanels)
    {
      if (arm == 0)
        {
          nPanel = 0;
          for (i = 0; i < TOFPANELSNUMBER; i++)
            {
              outPlane[nPanel] = tofPanels[i];
              nPanel++;
            }
          numberOfPanels = nPanel;
        }
      else
        {
          // check if arm!=0 is set active and inform that it doesn't exist
          //
          nSector = 0;
          for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
            {
              if (tofActive[arm][i] == 1)
                nSector++;
            }
          if (nSector)
            {
              PHMessage("cglDetectorGeo::get_tofGeo", PHError, " No real geometry available but arm is active\n ");
              return 0;
            }
        }
    }
  else
    {
      PHMessage("cglDetectorGeo::get_tofGeo", PHError, " No real geometry called for TOF \n ");
      return 0;
    }

  return numberOfPanels;
}

short
cglDetectorGeo::get_tofwGeo(const short arm, PHPanel outPlane[]) const
{
  short nPanel, numberOfPanels = 0;

  if (tofwGeoPanels && arm == 1) // real TOFW geometry for arm 1
    {
      nPanel = 0;
      for (int i = 0; i < TOFWPANELSNUMBER; i++)
	{
	  outPlane[nPanel] = tofwPanels[i];
	  nPanel++;
	}
      numberOfPanels = nPanel;
    }
  else
    {
      numberOfPanels = 0;
    }

  return numberOfPanels;
}

short
cglDetectorGeo::get_pbscGeo(const short arm, PHPanel outPlane[]) const
{
  short i, nSector;

  nSector = 0;
  for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      if (pbscActive[arm][i] == 1)
        {
          outPlane[nSector] = emcSectors[arm][i];
          nSector++;
        }
    }

  return nSector;
}


short
cglDetectorGeo::get_pbglGeo(const short arm, PHPanel outPlane[]) const
{
  short i, nSector;

  nSector = 0;
  for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      if (pbglActive[arm][i] == 1)
        {
          outPlane[nSector] = emcSectors[arm][i];
          nSector++;
        }
    }

  return nSector;
}

short
cglDetectorGeo::get_accGeo(const short arm, PHPanel outPlane[]) const
{
  short i, nPanel, numberOfPanels = 0;
  if (accActive)
    {
      if (arm == 1)
        {
          nPanel = 0;
          for (i = 0; i < ACCPANELSNUMBER; i++)
            {
              outPlane[nPanel] = accPanels[i];
              nPanel++;
            }
          numberOfPanels = nPanel;
        }
      else
        {
          numberOfPanels = 0;
        }
    }
  else
    {
      numberOfPanels = 0;
    }
  return numberOfPanels;
}

short
cglDetectorGeo::get_hbdGeo(const short arm, PHPanel outPlane[]) const
{
  short i, nPanel;

  nPanel = 0;
  for (i = 0; i < HBDPANELSPERARM; i++)
    {
      if (hbdActive[arm][i] == 1)
        {
          outPlane[nPanel] = hbdPanels[arm][i];
          nPanel++;
        }
    }

  return nPanel;
}

void
cglDetectorGeo::fetch_pc1Geo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_pc1Geo", PHError, "invalid arm\n");
      return ;
    }


  padDetectorGeo *padDetGeo = findNode::getClass<padDetectorGeo>(topNode,"mPadDetGeo");

  // go fetch the panels from padDetectorGeo

  PHPanel* pc1Plane = new PHPanel [padDetectorGeo::PC1MAXSECTPERARM];
  short nPc1Geo = padDetGeo->get_pc1Geo(arm, pc1Plane);

  for (int i = 0; i < nPc1Geo; i++)
    {
      pc1Sectors[arm][i] = pc1Plane[i];
    }

  delete [] pc1Plane;
}

void
cglDetectorGeo::fetch_pc2Geo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_pc2Geo", PHError, "invalid arm\n");
      return ;
    }

  padDetectorGeo *padDetGeo = findNode::getClass<padDetectorGeo>(topNode,"mPadDetGeo");

  // go fetch the panels from padDetectorGeo

  PHPanel* pc2Plane = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM * CHAMBERSPERSECT];
  short nPc2Geo = padDetGeo->get_pc2Geo(arm, pc2Plane);

  for (int i = 0; i < nPc2Geo; i++)
    {
      pc2Sectors[arm][i] = pc2Plane[i];
    }

  delete [] pc2Plane;
}

void
cglDetectorGeo::fetch_pc3Geo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_pc3Geo", PHError, "invalid arm\n");
      return ;
    }


  padDetectorGeo *padDetGeo = findNode::getClass<padDetectorGeo>(topNode,"mPadDetGeo");

  // go fetch the panels from padDetectorGeo

  PHPanel* pc3Plane = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM * CHAMBERSPERSECT];
  short nPc3Geo = padDetGeo->get_pc3Geo(arm, pc3Plane);

  for (int i = 0; i < nPc3Geo; i++)
    {
      pc3Sectors[arm][i] = pc3Plane[i];
    }

  delete [] pc3Plane;
}


void
cglDetectorGeo::fetch_svxGeo(short arm)
{
  PHsvxDetectorGeo* PHsvxDetGeo = new PHsvxDetectorGeo();
  if(SvxUseAsciiFile) {
    PHsvxDetGeo->FetchFromFile();
  }
  else {
    PHsvxDetGeo->Fetch();
  }

  for(short ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++)
    {
      PHPanel* svxPlane = new PHPanel [SVXLADDERNUMBER];
      short nSvxGeo = PHsvxDetGeo->get_svxGeo(arm, ilayer, svxPlane);

      for (int i = 0; i < nSvxGeo; i++)
        {
          svxLadders[arm][ilayer][i] = svxPlane[i];
        }

      delete [] svxPlane;
    }

  delete PHsvxDetGeo;

  return;
}

void
cglDetectorGeo::fetch_tecGeo(PHCompositeNode *topNode, short arm)
{
  //  These will be overwritten if actual panels are found...
  tecGeoPanels    = 0;
  tecGeoPanelsOut = 0;  
  
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_tecGeo", PHError, "invalid arm\n");
      return ;
    }
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *parNode;

  parNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
    {
      PHMessage("cglDetectorGeo::fetch_tecGeo", PHError, "PAR node not in list");
    }
  PHNodeIterator parNodeIter(parNode);

  ObjectNode_t *TGONode;
  TGONode = static_cast<ObjectNode_t*>(parNodeIter.findFirst("PHIODataNode", "TecGeometry"));

  if (!TGONode)
    {
      PHMessage("cglDetectorGeo::fetch_tecGeo", PHError, "TEC detector geometries not found.\n");
      return ;
    }

  TecGeometryObject* TGO = static_cast<TecGeometryObject*>(TGONode->getData());

  // Fetch the panels from TecDetectorGeo
  // there are 6 Tec panels in each side of each eactor,
  // but we will fetch only the first one.
  // Tec tracks entrance point (dTecTrack->get_xyzin(j,i)) corresponds to
  // the intersection of the Tec track with this panel.
  short nTecGeo = 0;
  for (int i = 0; i < TECMAXSIDE; i++)
    {
      for (int j = 0; j < TECMAXSECT; j++)
        {
          if (TGO->isActiveSectorSide(j*TECMAXSIDE + i))
            {   // use only active sector sides
              // This method fetches TecPanel already corrected for corresponding PHFrame
              tecPanels[i][j] = TGO->getTecPanel(j * TECMAXSIDE * TECMAXPLANE + i);
              nTecGeo++;
            }
        }
    }

  // JGL 12/8/00 - Fetch the outer TEC panels as well!
  // SL Starting 03/15/02 outer tec plane is plane 3 instead
  //    of plane 5 (counting from 0)
  // JGL 6/1/04  - back to plane 5 for outer plane

  short nTecGeoOut = 0;
  for (int i = 0; i < TECMAXSIDE; i++)
    {
      for (int j = 0; j < TECMAXSECT; j++)
        {
          if (TGO->isActiveSectorSide(j*TECMAXSIDE + i))
            {
              tecPanelsOut[i][j] = TGO->getTecPanel(j * TECMAXSIDE * TECMAXPLANE + i + 10);
              nTecGeoOut++;
            }
        }
    }

  // set the tecGeoPanels flag and exit
  tecGeoPanels = nTecGeo;
  tecGeoPanelsOut = nTecGeoOut;
}

void
cglDetectorGeo::fetch_tofGeo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_tofGeo", PHError, "invalid arm\n");
      return ;
    }

  TofGeometryObject* TofDetGeo = findNode::getClass<TofGeometryObject>(topNode,"TofGeometry");

  // go fetch the panels from TofDetectorGeo
  // unfortunately the tofPanels have "public" and "private" part
  // the actual reconstruction is at a distance 3.2905cm behind the panel face
  // I'll need to transform the fetched panel for intersections to be correct
  PHFrame XYZ;
  PHPoint slatOffset(3.2905, 0, 0);
  PHVector Xaxis(1, 0, 0);
  PHVector Yaxis(0, 1, 0);
  PHFrame slatFrame(slatOffset, Xaxis, Yaxis);
  PHPanel* tofPlane = new PHPanel [TOFPANELSNUMBER];
  short nTofGeo = 0;
  for (int i = 0; i < TOFPANELSNUMBER; i++)
    {
      tofPanels[i] = transformPanel(XYZ, TofDetGeo->getPanelGeo(i), slatFrame);
      nTofGeo++;
    }

  delete [] tofPlane;
  // set the tofGeoPanels flag and exit
  tofGeoPanels = 1;
}

void
cglDetectorGeo::fetch_tofwGeo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm != 1)
    {
      PHMessage("cglDetectorGeo::fetch_tofwGeo", PHError, "invalid arm\n");
      return ;
    }
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *parNode;

  parNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
    {
      PHMessage("cglDetectorGeo::fetch_tofwGeo", PHError, "PAR node not in list");
    }
  PHNodeIterator parNodeIter(parNode);

  PHDataNode<TofwGeometry> *TofwDetGeoNode;
  TofwDetGeoNode = static_cast<PHDataNode<TofwGeometry> *>(parNodeIter.findFirst("PHDataNode", "TofwGeometry"));

  if (!TofwDetGeoNode)
    {
      PHMessage("cglDetectorGeo::fetch_tofwGeo", PHError, "tofw detector geometries not found.\n");
      return ;
    }

  TofwDetGeo = static_cast<TofwGeometry*>(TofwDetGeoNode->getData());

  // go fetch the panels from TofwDetectorGeo
  PHPanel* tofwPlane = new PHPanel [TOFWPANELSNUMBER];
  short nTofwGeo = 0;  
  
  // getBoxGeo gives the geometry of four large panels. These are the
  // acceptance regions for the TOF.W./TOFW detector subsystem (Run7)--shengli

  for (int i = 0; i < TOFWPANELSNUMBER; i++)
    {
      tofwPanels[i] = TofwDetGeo->getBoxGeo(i);
      nTofwGeo++;
    }

  delete [] tofwPlane;
  // set the tofwGeoPanels flag and exit
  tofwGeoPanels = 1;
}


void
cglDetectorGeo::fetch_accGeo(PHCompositeNode *topNode)
{
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *parNode;

  parNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
    {
      PHMessage("cglDetectorGeo::fetch_accGeo", PHError, "PAR node not in list");
      return;
    }

  PHNodeIterator parNodeIter(parNode);

  PHDataNode<AccGeometry> *AccDetGeoNode;
  AccDetGeoNode = static_cast<PHDataNode<AccGeometry> *>(parNodeIter.findFirst("PHDataNode", "AccGeometry"));

  if (!AccDetGeoNode)
    {
      PHMessage("cglDetectorGeo::fetch_accGeo", PHError, "acc detector geometries not found.\n");
      return ;
    }

  AccDetGeo = static_cast<AccGeometry*>(AccDetGeoNode->getData());

  PHPanel* accPlane = new PHPanel [ACCPANELSNUMBER];
  short nAccGeo = 0;
  for (int i = 0; i < ACCPANELSNUMBER; i++)
    {
      accPanels[i] = AccDetGeo->getPanelGeo(i);
      nAccGeo++;
    }

  delete [] accPlane;
}

void
cglDetectorGeo::fetch_hbdGeo(PHCompositeNode *topNode, short arm)
{
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *parNode;

  parNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
    {
      PHMessage("cglDetectorGeo::fetch_hbdGeo", PHError, "PAR node not in list");
      return;
    }

  PHNodeIterator parNodeIter(parNode);

  PHDataNode<hbdDetectorGeo> *HbdDetGeoNode;
  HbdDetGeoNode = static_cast<PHDataNode<hbdDetectorGeo> *>(parNodeIter.findFirst("PHDataNode", "hbdDetectorGeo"));

  if (!HbdDetGeoNode)
    {
      PHMessage("cglDetectorGeo::fetch_hbdGeo", PHError, "hbd detector geometries not found.\n");
      return ;
    }

  HbdDetGeo = static_cast<hbdDetectorGeo*>(HbdDetGeoNode->getData());

  PHPanel* hbdPlane = new PHPanel [HBDPANELSNUMBER];
  short nHbdGeo = 0;
 
  for (int i = 0; i < HBDPANELSPERARM; i++)
    {
      if(arm==0)hbdPanels[arm][i] = HbdDetGeo->getPanelGeo(i);
      if(arm==1)hbdPanels[arm][i] = HbdDetGeo->getPanelGeo(i+HBDPANELSPERARM);
      nHbdGeo++;
    }

  delete [] hbdPlane;
}

///// new stuff for emc
void
cglDetectorGeo::fetch_emcGeo(PHCompositeNode *topNode, short arm)
{
  // Make sure the input is OK
  if (arm < 0 || arm > 1)
    {
      PHMessage("cglDetectorGeo::fetch_emcGeo", PHError, "invalid arm\n");
      return ;
    }

  recoConsts *rc = recoConsts::instance();
  PHString compname, ioname;
  if ( !rc->FlagExist("EMCSIMULATIONV2") ){
    compname = "PAR";
    ioname = "mEmcGeometry";
  } else {
    compname = "EMC";
    ioname = "mEmcGeometryModule";
  }

  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", compname));
  if (!parNode)
    {
      PHMessage("cglDetectorGeo::fetch_emcGeo", PHError, "PAR node not in list");
      return;
    }
  PHNodeIterator parNodeIter(parNode);

  ObjectNode_t *emcDetGeoNode;
  emcDetGeoNode = static_cast<ObjectNode_t*>(parNodeIter.findFirst("PHIODataNode", ioname));

  if (!emcDetGeoNode)
    {
      PHMessage("cglDetectorGeo::fetch_emcGeo", PHError, "emc detector geometries not found.\n");
      return ;
    }

  EmcDetGeo = static_cast<mEmcGeometryModule*>(emcDetGeoNode->getData());

  // go fetch the panels from mEmcGeometryModule

  for (int sect = 0; sect < padDetectorGeo::CGLMAXSECTPERARM; sect++ )
    {
      // by using the same name this construct is obsolete:
      //       if (arm == 0 && sect < 2)  // East pbgl
      //         emcSectors[arm][sect] = EmcDetGeo->GetPanel(arm, sect);
      //       else // pbsc
      //         emcSectors[arm][sect] = EmcDetGeo ->GetPanel(arm, sect);
      emcSectors[arm][sect] = EmcDetGeo ->GetPanel(arm, sect);
    }
}


void
cglDetectorGeo::fetchAllGeo(PHCompositeNode *topNode)
{
  for ( int arm = 0;arm < 2;arm++)
    {

      fetch_pc1Geo(topNode, arm);
      fetch_pc2Geo(topNode, arm);
      fetch_pc3Geo(topNode, arm);
      
      if(SvxActive) {fetch_svxGeo(arm);}
      int HbdActive=0;
      for(int i=0; i<HBDPANELSPERARM; i++){
	 if(hbdActive[arm][i]==1)HbdActive=1;
      }
      if(HbdActive)fetch_hbdGeo(topNode, arm); //do only if hbd is in

      if (arm == 0)
        {
          fetch_tofGeo(topNode, arm);
          fetch_tecGeo(topNode, arm);
        }
      else  // check if geometry for TEC and TOF is wanted in arm 1
        {
	  fetch_accGeo(topNode);
	  fetch_tofwGeo(topNode, arm);
	  
          int nSectorTOF = 0;
          int nSectorTEC = 0;
          for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
            {
              if (tecActive[arm][i] == 1)
                {
                  nSectorTEC++;
                }
              if (tofActive[arm][i] == 1)
                {
                  nSectorTOF++;
                }
            }
          if (nSectorTEC > 0)
            {
              fetch_tecGeo(topNode, arm);
            }
          if (nSectorTOF > 0)
            {
              fetch_tofGeo(topNode, arm);
            }
        }
      fetch_emcGeo(topNode, arm);
    }
  
}

void
cglDetectorGeo::set_pc1Active(const short arm, const short pc1in[])
{
  for (int i = 0; i < padDetectorGeo::PC1MAXSECTPERARM; i++)
    {
      pc1Active[arm][i] = pc1in[i];
    }
}

void
cglDetectorGeo::get_pc1Active(const short arm, short pc1out[]) const
{
  for (int i = 0; i < padDetectorGeo::PC1MAXSECTPERARM; i++)
    {
      pc1out[i] = pc1Active[arm][i];
    }
}

void
cglDetectorGeo::set_pc2Active(const short arm, const short pc2in[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      pc2Active[arm][i] = pc2in[i];
    }
}

void
cglDetectorGeo::get_pc2Active(const short arm, short pc2out[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      pc2out[i] = pc2Active[arm][i];
    }
}


void
cglDetectorGeo::set_pc3Active(const short arm, const short pc3in[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      pc3Active[arm][i] = pc3in[i];
    }
}

void
cglDetectorGeo::get_pc3Active(const short arm, short pc3out[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
    {
      pc3out[i] = pc3Active[arm][i];
    }
}


void
cglDetectorGeo::set_tecActive(const short arm, short const tecin[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tecActive[arm][i] = tecin[i];
    }
}

void
cglDetectorGeo::get_tecActive(const short arm, short tecout[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tecout[i] = tecActive[arm][i];
    }
}


void
cglDetectorGeo::set_tofActive(const short arm, const short tofin[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tofActive[arm][i] = tofin[i];
    }
}

void
cglDetectorGeo::get_tofActive(const short arm, short tofout[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tofout[i] = tofActive[arm][i];
    }
}

void
cglDetectorGeo::set_tofwActive(const short arm, const short tofwin[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tofwActive[arm][i] = tofwin[i];
    }
}

void
cglDetectorGeo::get_tofwActive(const short arm, short tofwout[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      tofwout[i] = tofwActive[arm][i];
    }
}

void
cglDetectorGeo::set_svxActive(const short arm, const short layer, const short svxin[])
{
  for (int i = 0; i < SVXLADDERNUMBER; i++)
    {
      svxActive[arm][layer][i] = svxin[i];
    }
}
void
cglDetectorGeo::set_SvxActive(const short val)
{
      SvxActive = val;
}


void
cglDetectorGeo::get_svxActive(const short arm, const short layer, short svxout[]) const
{
  for (int i = 0; i < SVXLADDERNUMBER; i++)
    {
      svxout[i] = svxActive[arm][layer][i];
    }
}
short int
cglDetectorGeo::get_SvxActive() const
{
  return SvxActive;
}

void
cglDetectorGeo::set_hbdActive(const short arm, const short hbdin[])
{
  for (int i = 0; i < HBDPANELSPERARM; i++)
    {
      hbdActive[arm][i] = hbdin[i];
    }
}

void
cglDetectorGeo::get_hbdActive(const short arm, short hbdout[]) const
{
  for (int i = 0; i < HBDPANELSPERARM; i++)
    {
      hbdout[i] = hbdActive[arm][i];
    }
}

void
cglDetectorGeo::set_pbscActive(const short arm, const short pbscin[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      pbscActive[arm][i] = pbscin[i];
    }
}

void
cglDetectorGeo::get_pbscActive(const short arm, short pbscout[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      pbscout[i] = pbscActive[arm][i];
    }
}

void
cglDetectorGeo::set_pbglActive(const short arm, const short pbglin[])
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      pbglActive[arm][i] = pbglin[i];
    }
}

void
cglDetectorGeo::get_pbglActive(const short arm, short pbglout[]) const
{
  for (int i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
    {
      pbglout[i] = pbglActive[arm][i];
    }
}

void
cglDetectorGeo::PrintGeo(const short detid, const short arm) const
{
  int i;

  // Print overall information
  cout << "cglDetectorGeo:\n";
  cout << "  Verbose = " << Verbose << "\n";

  // Print DCH information
  if (detid == 0)
    {
      cout << "  cglDetectorGeo for DCH:\n";
      cout << "   Radius = " << dchRadius << "\n";
      cout << "   Generated cylinder for arm " << arm << ":\n";
      dchArm[arm].print();
    }

  // Print PC1 information
  if (detid == 1)
    {
      cout << "  cglDetectorGeo for PC1:\n";
      cout << "   Radius = " << pc1Radius << "\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::PC1MAXSECTPERARM; i++)
        {
          cout << pc1Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[arm][i] == 1)
            {
              pc1Sectors[arm][i].print();
            }
        }
    }

  // Print PC2 information
  if (detid == 2)
    {
      cout << "  cglDetectorGeo for PC2:\n";
      cout << "   Radius = " << pc2Radius << "\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
        {
          cout << pc2Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
        {
          if (pc2Active[arm][i] == 1)
            {
              pc2Sectors[arm][i].print();
            }
        }
    }

  // Print PC3 information
  if (detid == 3)
    {
      cout << "  cglDetectorGeo for PC3:\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
        {
          cout << pc3Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT; i++)
        {
          if (pc3Active[arm][i] == 1)
            {
              pc3Sectors[arm][i].print();
            }
        }
    }

  // Print CRK information
  if (detid == 4)
    {
      cout << "  cglDetectorGeo for CRK:\n";
      cout << "   Radius = " << crkRadius << endl;
      cout << "   Generated cylinder for arm " << arm << ":\n";
      crkArm[arm].print();
    }

  // Print TEC information
  if (detid == 5)
    {
      cout << "  cglDetectorGeo for TEC:\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          cout << tecActive[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          if (tecActive[arm][i] == 1)
            {
              //             tecSectors[arm][i].print();
            }
        }
    }

  // Print TOF information
  if (detid == 6)
    {
      cout << " cglDetectorGeo for TOF fetched from TofGeometryObject" << endl;
      for (i = 0; i < TOFPANELSNUMBER; i++)
        {
          tofPanels[i].print();
        }
    }

  // Print PbSc information
  if (detid == 7)
    {
      cout << "  cglDetectorGeo for PbSc:\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          cout << pbscActive[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          if (pbscActive[arm][i] == 1)
            {
              emcSectors[arm][i].print();
            }
        }
    }

  // Print PbGl information
  if (detid == 8)
    {
      cout << "  cglDetectorGeo for PbGl:\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          cout << pbglActive[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < padDetectorGeo::CGLMAXSECTPERARM; i++)
        {
          if (pbglActive[arm][i] == 1)
            {
              emcSectors[arm][i].print();
            }
        }
    }
  //Print ACC information
  if(detid == 12)
    {
      cout << " cglDetectorGeo for ACC fetched from Acc Ascii" << endl;
      for(i = 0; i < ACCPANELSNUMBER; i++)
	{
	  accPanels[i].print();
	}
    }

  // Print TOFW information
  if (detid == 13)
    {
      cout << " cglDetectorGeo for TOFW fetched from TofwGeometry" << endl;
      for (i = 0; i < TOFWPANELSNUMBER; i++)
        {
          tofwPanels[i].print();
        }
    }

// Print SVX information
  if (detid >=14 && detid <=17)
    {
      int ilayer = detid - 14;
      cout << "  cglDetectorGeo for SVX:\n";
      cout << "   Active sectors for layer " << ilayer << ": ";
      for (i = 0; i < SVXLADDERNUMBER; i++)
        {
          cout << svxActive[arm][ilayer][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for layer " << ilayer << ":\n";
      for (i = 0; i < SVXLADDERNUMBER; i++)
        {
          if (svxActive[arm][ilayer][i] == 1)
            {
              svxLadders[arm][ilayer][i].print();
            }
        }
    }
 
}
