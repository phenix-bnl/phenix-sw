#include "DchSagCorrector.h"
#include "DchTrack.h"
#include "DchHitLineTable.hh"
#include "DchSnglTrackv1.h" // not really a versioned object
#include "PHDchGeometryObject.h"

#include "PHGeometry.h"

#include "Fun4AllServer.h"
#include "TH2.h"
#include "getClass.h"

#include <iostream>

using namespace std;
using namespace PHGeometry;

bool SagInitDone = (1 == 0);

DchSagCorrector::DchSagCorrector(const char *name) : SubsysReco(name)
{
  DAO = NULL;
  DGO = NULL;
  return ;
}


int DchSagCorrector::InitRun(PHCompositeNode *topNode)
{
  PHTimeStamp Tsearch;

  const char* addressNameDB  = "calibdch_v3_add";
  const char* geometryNameDB = "calibdch_v3_geo";
  PdbBankID bankID;
  bankID.setInternalValue(1);

  DAO = new PHDchAddressObject();
  DAO->initialize(); // reading
  DAO->fetch(Tsearch, addressNameDB, bankID);
  DAO->commit();
  DGO = new PHDchGeometryObject(DAO);
  DGO->fetch(Tsearch, geometryNameDB, bankID);
  DGO->commit();
  DGO->rotateAndTranslate();
  return 0;
}

int DchSagCorrector::process_event(PHCompositeNode *topNode)
{
  dchtracks = findNode::getClass<DchTrack>(topNode, "DchTrack");
  dchhits = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");

  if (!dchtracks)
    {
      cout << PHWHERE << "SagCorrector:: DchTrack not in Node Tree" << endl;
      return -1;
    }
  if (!dchhits)
    {
      cout << PHWHERE << "SagCorrector:: DchHitLineTable not in Node Tree" << endl;
      return -1;
    }

  unsigned int nTracks = dchtracks->get_DchNTrack();
  for (unsigned int i = 0; i < nTracks; i++)
    {
      DchSnglTrackv1 * track = dchtracks->get_Track(i);
      float zed = track->get_zed();
      PHPoint base = track->get_point();
      PHVector dir = track->get_direction();
      PHLine trackline(base, dir);
      if (track->get_quality() != 63)
        {
          continue;
        }
      if (fabs(zed) > 100)
        {
          continue;
        }
      for (int h = 2; h < 10; h++)
        {
          // X1
          CalcResidual(h , track, trackline, zed);
        }

      for (int h = 22; h < 30; h++)
        {
          // X2
          CalcResidual(h , track, trackline, zed);
        }
    }


  return 0;
}


int DchSagCorrector::CalcResidual(int plane , DchSnglTrackv1 *track, PHLine trackline, float zed)
{
  short ihit = track->get_hits(plane);
  if (ihit > -1)
    {
      DchHitLineOut* hit = dchhits->getHit(ihit);
      int plane = hit->getPlane();
      int even = (plane % 2 == 0);
      int side = hit->getSide();
      int arm = hit->getArm();
      int cell = hit->getCell();
      float hitx = hit->getX();
      float hity = hit->getY();
      float hitz = hit->getZ();

      PHPoint hitpoint(hitx, hity, hitz);
      PHVector DriftDirection = DGO->getLocalDriftDirection(arm, plane, cell, hit->getZ());
      PHLine hitline(hitpoint, DriftDirection);
      PHPoint closeC = closestApproachLineLine(trackline, hitline);

      DriftDirection.setZ(0);
      DriftDirection.normalize();
      PHVector orthoV(DriftDirection.getY(), -DriftDirection.getX(), 0);
      float dx = 2. * (closeC.getX() - hitpoint.getX());
      float dy = 2. * (closeC.getY() - hitpoint.getY());
      PHVector closeV(dx, dy, 0);
      float parallel = dot(closeV, DriftDirection);
      float ortho = dot(closeV, orthoV);

      if (verbosity > 0)
        {
          FillHistos(arm, side, even, dx, dy, parallel, ortho, hitz);
        }
    }
  return 0;
}



int DchSagCorrector::FillHistos(int arm, int side, int even, float dx, float dy, float para, float ortho, float zed)
{
  //  cout << arm << " " << side << " " << even << " " << dx << " " << dy << " " << zed << endl;
  char hname[80];
  Fun4AllServer * server = Fun4AllServer::instance();
  if (!SagInitDone)
    {
      SagInitDone = (1 == 1);
      sprintf(hname, "SAG_X_ENO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_ENE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_ESO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_ESE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_WNO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_WNE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_WSO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_X_WSE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));

      sprintf(hname, "SAG_Y_ENO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_ENE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_ESO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_ESE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_WNO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_WNE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_WSO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_Y_WSE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));


      sprintf(hname, "SAG_P_ENO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_ENE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_ESO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_ESE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_WNO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_WNE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_WSO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_P_WSE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));

      sprintf(hname, "SAG_O_ENO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_ENE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_ESO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_ESE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_WNO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_WNE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_WSO");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));
      sprintf(hname, "SAG_O_WSE");
      server->registerHisto(hname, new TH2F(hname, hname, 50, -90, 90, 1000, -0.1, 0.1));

    }
  TH1* h = 0;
  if (arm == 0)
    {
      if (side == 1)
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_X_ENO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_ENO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
          else
            {
              sprintf(hname, "SAG_X_ENE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_ENE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
        }
      else
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_X_ESO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_ESO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
          else
            {
              sprintf(hname, "SAG_X_ESE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_ESE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
        }
    }
  else
    {
      if (side == 1)
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_X_WNO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_WNO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
          else
            {
              sprintf(hname, "SAG_X_WNE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_WNE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
        }
      else
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_X_WSO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_WSO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
          else
            {
              sprintf(hname, "SAG_X_WSE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dx);
              sprintf(hname, "SAG_Y_WSE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, dy);
            }
        }
    }

  if (arm == 0)
    {
      if (side == 1)
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_P_ENO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -para);
              sprintf(hname, "SAG_O_ENO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -ortho);
            }
          else
            {
              sprintf(hname, "SAG_P_ENE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, para);
              sprintf(hname, "SAG_O_ENE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, ortho);
            }
        }
      else
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_P_ESO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -para);
              sprintf(hname, "SAG_O_ESO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -ortho);
            }
          else
            {
              sprintf(hname, "SAG_P_ESE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, para);
              sprintf(hname, "SAG_O_ESE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, ortho);
            }
        }
    }
  else
    {
      if (side == 1)
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_P_WNO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -para);
              sprintf(hname, "SAG_O_WNO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -ortho);
            }
          else
            {
              sprintf(hname, "SAG_P_WNE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, para);
              sprintf(hname, "SAG_O_WNE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, ortho);
            }
        }
      else
        {
          if (even == 0)
            {
              sprintf(hname, "SAG_P_WSO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -para);
              sprintf(hname, "SAG_O_WSO");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, -ortho);
            }
          else
            {
              sprintf(hname, "SAG_P_WSE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, para);
              sprintf(hname, "SAG_O_WSE");
              h = dynamic_cast<TH1*>(server->getHisto(hname));
              h->Fill(zed, ortho);
            }
        }
    }
  return 0;
}
