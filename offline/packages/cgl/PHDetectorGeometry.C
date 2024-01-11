#include "PHDetectorGeometry.h"
#include "DetectorGeometry.h"
#include "cglDetectorGeo.hh"
#include "SavePHPanel.h"
#include <PHPanel.h>

#include <getClass.h>

#include <iostream>
#include <vector>

using namespace std;

PHDetectorGeometry *PHDetectorGeometry::__instance = NULL;

static const char *DetName[] =
    {
      "PC1",
      "PC2",
      "PC3",
      "TECIN",
      "TECOUT",
      "TOF",
      "PBGL",
      "PBSC",
      "ACC",
      "TOFW",
      "SVXPIXEL",  
      "SVXSTRIP1",  
      "SVXSTRIP2",  
      "SVXSTRIP3",  
      "HBD",
      "NONE"
     };

PHDetectorGeometry *PHDetectorGeometry::instance()
{
  if (! __instance)
    {
      __instance = new PHDetectorGeometry();
    }
  return __instance;
}

PHDetectorGeometry::PHDetectorGeometry()
{
  initialized = 0;
}

int PHDetectorGeometry::copycglDetectorGeo(PHCompositeNode *topNode)
{
  cglDetectorGeo *cglgeo = findNode::getClass<cglDetectorGeo>(topNode,"cglDetectorGeo");
  DetectorGeometry *detgeom = findNode::getClass<DetectorGeometry>(topNode,"DetectorGeometry");
  if (!detgeom)
    {
      cerr << PHWHERE
	   << ": ERROR - DetectorGeometry Node not found." << endl;
      return -1;
    }

  for (int iarm = 0; iarm <= 1; iarm++)
    {
      PHPanel *Panel = NULL;
      int nPanel = 0;
      Panel = new PHPanel [padDetectorGeo::PC1MAXSECTPERARM];
      nPanel = cglgeo->get_pc1Geo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[PC1]);
        }
      delete [] Panel;

      Panel = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM * 2];
      nPanel = cglgeo->get_pc2Geo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[PC2]);
        }
      delete [] Panel;

      Panel = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM * 2];
      nPanel = cglgeo->get_pc3Geo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[PC3]);
        }
      delete [] Panel;

      Panel = new PHPanel [TECMAXSECT * TECMAXSIDE];
      nPanel = cglgeo->get_tecGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[TECIN]);
        }
      delete [] Panel;

      Panel = new PHPanel [TECMAXSECT * TECMAXSIDE];
      nPanel = cglgeo->get_tecGeoOut(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[TECOUT]);
        }
      delete [] Panel;

      Panel = new PHPanel [TOFPANELSNUMBER];
      nPanel = cglgeo->get_tofGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[TOF]);
        }
      delete [] Panel;

      Panel = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM];
      nPanel = cglgeo->get_pbscGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[PBSC]);
        }
      delete [] Panel;

      Panel = new PHPanel [padDetectorGeo::CGLMAXSECTPERARM];
      nPanel = cglgeo->get_pbglGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[PBGL]);
        }
      delete [] Panel;

      Panel = new PHPanel [ACCPANELSNUMBER];
      nPanel = cglgeo->get_accGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[ACC]);
        }
      delete [] Panel;

      Panel = new PHPanel [TOFWPANELSNUMBER];
      nPanel = cglgeo->get_tofwGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[TOFW]);
        }
      delete [] Panel;

      Panel = new PHPanel [SVXLADDERNUMBER];
      nPanel = cglgeo->get_svxGeo(iarm, 0, Panel);
      for (int j = 0; j < nPanel; j++) { detgeom->AddPanel(&Panel[j], iarm, j, DetName[SVXPIXEL]); }
      delete [] Panel;
      Panel = new PHPanel [SVXLADDERNUMBER];
      nPanel = cglgeo->get_svxGeo(iarm, 1, Panel);
      for (int j = 0; j < nPanel; j++) { detgeom->AddPanel(&Panel[j], iarm, j, DetName[SVXSTRIP1]); }
      delete [] Panel;
      Panel = new PHPanel [SVXLADDERNUMBER];
      nPanel = cglgeo->get_svxGeo(iarm, 2, Panel);
      for (int j = 0; j < nPanel; j++) { detgeom->AddPanel(&Panel[j], iarm, j, DetName[SVXSTRIP2]); }
      delete [] Panel;
      Panel = new PHPanel [SVXLADDERNUMBER];
      nPanel = cglgeo->get_svxGeo(iarm, 3, Panel);
      for (int j = 0; j < nPanel; j++) { detgeom->AddPanel(&Panel[j], iarm, j, DetName[SVXSTRIP3]); }
      delete [] Panel;

      Panel = new PHPanel [HBDPANELSNUMBER];
      nPanel = cglgeo->get_hbdGeo(iarm, Panel);
      for (int j = 0; j < nPanel; j++)
        {
          detgeom->AddPanel(&Panel[j], iarm, j, DetName[HBD]);
        }
      delete [] Panel;


    }
  detgeom->identify();
  int iret = readDetectorGeometry(topNode);

  return iret;
}

int PHDetectorGeometry::readDetectorGeometry(PHCompositeNode *topNode)
{
  if (initialized)
    {
      return 0;
    }

  DetectorGeometry *detgeom = NULL;
  PHTypedNodeIterator<DetectorGeometry> sviter(topNode);
  PHIODataNode<DetectorGeometry> *SvNode = sviter.find("DetectorGeometry");
  if (SvNode)
    {
      detgeom = SvNode->getData();
    }
  else
    {
      cerr << PHWHERE
	   << ": ERROR - DetectorGeometry Node not found." << endl;
      return -1;
    }
  TClonesArray *TC = detgeom->GetPHPanelTCArray();
  for (int i = 0; i <= TC->GetLast(); i++)
    {
      SavePHPanel *svpanel = static_cast<SavePHPanel *> (TC->UncheckedAt(i));
      const char *tmp = svpanel->GetName();
      int iarm = svpanel->GetArm();
      PHPoint p0(svpanel->GetPoint(0, 0), svpanel->GetPoint(0, 1), svpanel->GetPoint(0, 2));
      PHPoint p1(svpanel->GetPoint(1, 0), svpanel->GetPoint(1, 1), svpanel->GetPoint(1, 2));
      PHPoint p2(svpanel->GetPoint(2, 0), svpanel->GetPoint(2, 1), svpanel->GetPoint(2, 2));
      PHPanel newpanel(p0, p1, p2);
      if (iarm != (int) EAST && iarm != (int) WEST)
        {
          cerr << PHWHERE << "Invalid arm for " << tmp << ": " << iarm << endl;
        }
      else
        {
          for (int idet = 0; idet < LAST; idet++)
            {
              if (!strcmp(tmp, DetName[idet]))
                {
                  panel[iarm][idet].push_back(newpanel);
		  break;
                }
            }
        }
    }
  initialized = 1;
  return 0;
}

void PHDetectorGeometry::Print() const
{
  const char *ArmName[] =
    {
      "EAST",
      "WEST"
    };

  vector <PHPanel>::const_iterator iter;
  for (int idet = 0; idet < LAST; idet++)
    {
      for (int iarm = 0; iarm < 2; iarm++)
        {
          cout << DetName[idet] << " " << ArmName[iarm] << ": " << endl;
          for (iter = panel[iarm][idet].begin(); iter != panel[iarm][idet].end(); iter++)
            {
              (*iter).print();
            }
        }
    }
  return ;
}

