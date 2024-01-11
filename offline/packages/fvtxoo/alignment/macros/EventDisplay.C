///////////////////////////////////////////////////////////////////////////
// Macro to display an event from an FVTX ntuple created by FvtxEval.cxx //
// Author - Aaron Key (2012)                                             //
//
// Updated - Jin Huang : to use alignment root files and track fits
///////////////////////////////////////////////////////////////////////////

#include <TEveManager.h>
#include <TEveStraightLineSet.h>
#include <TEvePointSet.h>
#include <TGLViewer.h>
#include <TGLClip.h>
#include <TEveVSDStructs.h>
#include <TEveVector.h>
#include <TEveTrack.h>
#include <TEveTrackPropagator.h>
//#include <TEveGeoTopNode.h>
#include <cassert>

double M_PI = 3.14159265358979323846; /* pi */
double M_PI_2 = M_PI/2;

using namespace std;

// Arguments:
// ----------------------------
// ntuple_filename: ntuple created by FvtxEval.cxx
// evt: Event number to display
// rcut: Places a cut on the maximum r value the track can have at the PHENIX vertex
//       Helps to eliminate the tracks that don't point to the vertex
// transparency: Transparency value used for the FVTX sensors/HDIs (95 = 5% visible)
void
EventDisplay(unsigned int evt = 0, TString ntuple_filename =
    "FvtxGlobalAlign_eval.root", double rcut = 9999, unsigned int transparency =
    92)
{

  cout << "Load shared libs" << endl;
  gSystem->Load("libfvtxoo_alignment");
  gSystem->Load("libfvtxgeom");

  // Initialize EVE
  cout << "Initialize EVE" << endl;
  TEveManager::Create();
  TFile::SetCacheFileDir(".");

///////////////////////////////////////
// Setup for drawing geometry        //
///////////////////////////////////////

  cout << "Load geometry" << endl;
  FvtxGeom::create_arms();
//  gGeoManager = gEve->GetGeometry("fvtxgeom.root");
  gGeoManager->DefaultColors();

  // Grab the SICG_1 TGeoVolume and TGeoNode from the geometry file
  TGeoVolume* sicg_vol =
      gGeoManager->GetTopVolume()->FindNode("SIEN_1")->GetVolume()->FindNode(
          "SICG_1")->GetVolume();
  TGeoNode* node1 =
      gGeoManager->GetTopVolume()->FindNode("SIEN_1")->GetVolume()->GetNode(
          "SICG_1");

  // Create a TEveGeoTopNode from the TGeoNode that Eve understands
  TEveGeoTopNode *eve_node_n = new TEveGeoTopNode(gGeoManager, node1);
  // Only draw down 6 levels from SICG.  This will draw everything up to the strips.  Strips are very slow to draw.
  eve_node_n->SetVisLevel(6);

  // Grab all of the nodes contained in the SICG_1 volume.
  TObjArray* sicg_nodes = sicg_vol->GetNodes();

  // String that is compared against to decide to draw a FVTX volume or not
  TString tester = "I";

/////////////////////////////////////////////////////////
//  Draw the FVTX and hide the VTX/support structures  //
/////////////////////////////////////////////////////////

  for (unsigned int j = 0; j < node1->GetVolume()->GetNdaughters(); j++)
    {
      // For all volumes in SICG_1
      TString node_name = sicg_nodes->At(j)->GetName(); // arm
      // Check if the node is NOT the north or south FVTX node by name comparison
      if (node_name != "SCMN_1" && node_name != "SCMS_1")
        {
          TGeoNode * geo_node = (TGeoNode *) (sicg_nodes->At(j));
          assert(geo_node);
          // Make the node invisisble
          geo_node->SetVisibility(kFALSE);
          // Make all subvolumes invisible
          geo_node->VisibleDaughters(kFALSE);
        }
      // If we are in the FVTX node then only draw the volumes named SI* as
      else
        {
          // To get at the actual FVTX disks we have to go down two levels from SICG
          TGeoNode* tmp_node = (TGeoNode*) sicg_nodes->At(j);
          TObjArray* sub_nodes = (TObjArray*) tmp_node->GetVolume()->GetNodes();
          for (unsigned int k = 0; k < tmp_node->GetVolume()->GetNdaughters();
              k++)
            { // cage
              // Down one level
              TGeoNode* tmp_node2 = (TGeoNode*) sub_nodes->At(k);
              TObjArray* sub_sub_nodes =
                  (TObjArray*) tmp_node2->GetVolume()->GetNodes();
              for (unsigned int l = 0;
                  l < tmp_node2->GetVolume()->GetNdaughters(); l++)
                { // station
                  // Down two levels
                  TGeoNode* tmp_node3 = (TGeoNode*) sub_sub_nodes->At(l);
                  // Get the name of these volumes
                  TString* pre_name = new TString(tmp_node3->GetName());
                  // Check if it is not named SI*
                  if ((*pre_name)[1] != tester)
                    { // not a station
                      // Make the cages and subvolumes invisible
                      tmp_node3->SetVisibility(kFALSE);
                      tmp_node3->GetVolume()->SetTransparency(transparency);
//                      tmp_node3->VisibleDaughters(kFALSE);
                    }
                  // If we are in the SI* named volumes (disks) then make everything transparent so
                  // that clusters and tracks are more visible
                  else
                    { // a FVTX station
                      // Make the volume itself transparent.  Unfortunately this doesn't propagate down to subvolumes...
                      tmp_node3->GetVolume()->SetTransparency(transparency);
                      tmp_node3->SetVisibility(kTRUE);
                      TObjArray* sub3_nodes =
                          tmp_node3->GetVolume()->GetNodes();
                      for (unsigned int m = 0;
                          m < tmp_node3->GetVolume()->GetNdaughters(); m++)
                        { // sectors , e.g. SIPB_1
                          // Make volumes down one level transparent
                          TGeoNode* tmp_node4 = (TGeoNode*) sub3_nodes->At(m);

                          TString name(tmp_node4->GetName());

                          if (name.Contains("SUP"))
                            {

                              tmp_node4->SetVisibility(kTRUE);
                              tmp_node4->VisibleDaughters(kFALSE);

                              continue;
                            }

                          tmp_node4->SetVisibility(kFALSE);
                          tmp_node4->VisibleDaughters(kFALSE);
                          tmp_node4->GetVolume()->SetTransparency(transparency);
                          TObjArray* sub4_nodes =
                              tmp_node4->GetVolume()->GetNodes();
                          for (unsigned int n = 0;
                              n < tmp_node4->GetVolume()->GetNdaughters(); n++)
                            { // sub wedge structure, HDI, sensor, chips, etc.
                              // Make volumes down two levels transparent
                              TGeoNode* tmp_node5 = (TGeoNode*) sub4_nodes->At(
                                  n);
                              tmp_node5->GetVolume()->SetTransparency(
                                  transparency);

                              if (TString(tmp_node5->GetName())
                                  != TString("SISB_1")
                                  || TString(tmp_node5->GetName())
                                      != TString("SISS_1"))
                                {
                                  tmp_node5->SetVisibility(kFALSE);
                                  tmp_node5->VisibleDaughters(kFALSE);
                                }
                              else
                                {
                                  tmp_node5->SetVisibility(kTRUE);
                                  tmp_node5->VisibleDaughters(kTRUE);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

  // Add the element to the Geometry section in the TEveViewer (AddElement adds it to the Event section)
  gEve->AddGlobalElement(eve_node_n);

/////////////////////////////////////////
//  Open the ntuple file               //
/////////////////////////////////////////

  TFile* fvtx_eval = TFile::Open(ntuple_filename);
  assert(fvtx_eval);

  TTree * alignment = (TTree *) fvtx_eval->GetObjectChecked("alignment",
      "TTree");
  assert(alignment);

//  alignment->Draw(">>TheEntry", Form("Entry$ == %d", evt));
//  TEventList * TheEntry = (TEventList *) fvtx_eval->GetObjectChecked("TheEntry",
//      "TEventList");
//  assert(TheEntry);
  TEventList * TheEntry = new TEventList();
  TheEntry->Enter(evt);

  alignment->SetEventList(TheEntry);

  Long64_t nentries = alignment->Draw("z0reco_latcon:arm:bbcz", "", "goff");
  assert(nentries == 1);

  const double z0reco = alignment->GetV1()[0];
  const int arm = alignment->GetV2()[0];
  const double bbcz = alignment->GetV3()[0];

  Long64_t nentries = alignment->Draw("phi_avg:phi_acpt_cent:phi_acpt_width",
      "", "goff");
  assert(nentries == 1);

  const double phi_avg = alignment->GetV1()[0];
  const double phi_acpt_cent = alignment->GetV2()[0];
  const double phi_acpt_width = alignment->GetV3()[0];

  Long64_t nentries = alignment->Draw("vtxx:vtxy:vtxz:disp_pt_vtx_latcon", "", "goff");
  assert(nentries == 1);

  const double vtxxp = alignment->GetV1()[0];
  const double vtxyp = alignment->GetV2()[0];
  const double vtxzp = alignment->GetV3()[0];
  const double disp_pt_vtx_latcon = alignment->GetV4()[0];

  const TEveVectorD vertex_vtx(vtxxp, vtxyp, vtxzp);

/////////////////////////////////////////
//  Draw Tracks                      //
/////////////////////////////////////////

//  FvtxGeom::get_node_arm(1 - arm)->SetVisibility(kFALSE);
//  FvtxGeom::get_node_arm(1 - arm)->VisibleDaughters(kFALSE);

//
  // Create a list to contain all of the cluster sets
  TEveElementList* coord_list = new TEveElementList("Clusters");
  // Add the clusters to event display
  gEve->AddElement(coord_list);

  // Create a list to contain all of the cluster sets
//  TEveElementList* fit_list = new TEveElementList("Fit Projection");
//  gEve->AddElement(fit_list);

  // Create a list to hold all tracks
  TEveElementList* track_list = new TEveElementList("Reconstructed Tracks");
  // Add the main list to the event display
  gEve->AddElement(track_list);

  cout << "  MakeStripHits(alignment, arm, coord_list);" << endl;
  MakeStripHits(alignment, arm, coord_list, phi_acpt_cent, phi_acpt_width , vtxxp, vtxyp);

  Long64_t nentries = alignment->Draw("x_fit:y_fit:z_fit:flag", "", "goff");
  assert(nentries);

  cout << "  DisplayFit(Kalman Fit);" << endl;
  DisplayFit(track_list, track_list, nentries, alignment, z0reco, "Kalman Fit",
      kWhite);

  Long64_t nentries = alignment->Draw("x_fit_vzcon:y_fit_vzcon:z_fit:flag", "",
      "goff");
  assert(nentries);

  cout << "  DisplayFit(VzCon Fit);" << endl;
  DisplayFit(track_list, track_list, nentries, alignment, z0reco, "VzCon Fit",
      kYellow);

  /////////////////////////////////////////
  //  AUX features             //
  /////////////////////////////////////////

  TEveElementList* vertex_list = new TEveElementList("Vertex");
  // Add the quadrant lists to the main list
  gEve->AddElement(vertex_list);

  TEvePointSet * BBC = new TEvePointSet("BBC");

  // Set the point style - 20 means resizable spheres
  BBC->SetMarkerStyle(29);
  // Make them white
  BBC->SetMarkerColor(kBlue);
  // Make them smaller
  BBC->SetMarkerSize(2.5);
  // Add the station sets to the quadrant lists
  vertex_list->AddElement(BBC);

  BBC->SetNextPoint(0, 0, bbcz);

  if (abs(vtxzp) < 50)
    {

      TEvePointSet * VTX = new TEvePointSet("VTX");

      // Set the point style - 20 means resizable spheres
      VTX->SetMarkerStyle(20);
      // Make them white
      VTX->SetMarkerColor(kRed);
      // Make them smaller
      VTX->SetMarkerSize(3);
      // Add the station sets to the quadrant lists
      vertex_list->AddElement(VTX);

      VTX->SetNextPoint(vtxxp, vtxyp, vtxzp);


      TEveVector pb(vtxxp, //Float_t x1, //
          vtxyp, //          Float_t y1, //
          -10 //          Float_t z1,
          );

      TEveVector pe(vtxxp, //Float_t x1, //
          vtxyp, //          Float_t y1, //
          10 //          Float_t z1,
          );

      // Peak strip

      TEveStraightLineSet * clusters = new TEveStraightLineSet("Beam");
      assert(clusters);
      clusters->SetLineColor(kRed);
      clusters->SetLineWidth(1);
//
      clusters->AddLine(pb, pe);

      // Add the station sets to the quadrant lists
      vertex_list->AddElement(clusters);

    }

  if (abs(vtxzp) < 50)
    {

      TEvePointSet * VTX = new TEvePointSet("FVTX Projection");

      // Set the point style - 20 means resizable spheres
      VTX->SetMarkerStyle(20);
      // Make them white
      VTX->SetMarkerColor(kYellow);
      // Make them smaller
      VTX->SetMarkerSize(3);
      // Add the station sets to the quadrant lists
      vertex_list->AddElement(VTX);

      VTX->SetNextPoint(vtxxp + cos(phi_acpt_cent) * disp_pt_vtx_latcon
          , vtxyp + sin(phi_acpt_cent) * disp_pt_vtx_latcon
          , vtxzp);

    }
/////////////////////////////////////////
//  Set up the viewer                  //
/////////////////////////////////////////

  // EClipType not exported to CINT (see TGLUtil.h):
  // 0 - no clip, 1 - clip plane, 2 - clip box
  TGLViewer *v = gEve->GetDefaultGLViewer();
  // Draw the axes
  v->SetGuideState(TGLUtil::kAxesOrigin, kTRUE, kFALSE, 0);
  // Set the clipping
  v->GetClipSet()->SetClipType((EClipType) 0);
  // Apply changes
  v->RefreshPadEditor(v);

  // Redraw the viewer with the changes
  gEve->FullRedraw3D(kTRUE);

  // Set initial viewing angle
  v->CurrentCamera().RotateRad(0, M_PI / 2);
  v->CurrentCamera().Zoom(1, 0, 0);

  // Draw with that angle
  v->DoDraw();

  v->SavePicture(ntuple_filename + Form("_EventDisplay_%d.png", evt));
}

void
DisplayFit(TEveElementList * track_list, TEveElementList* coord_list,
    const Long64_t nentries, TTree * alignment, const double z0reco,
    TString name, Color_t c)
{
  assert(alignment);

  // Create a cluster list for each quadrant
  TEveElementList* ne_coord_list = new TEveElementList(name + "Clusters");
  // Add the quadrant lists to the main list
  coord_list->AddElement(ne_coord_list);

  TEveVectorD station0;
  double minz = 1000;
  TEveVectorD station3;
  double maxz = -1000;

  for (Long64_t i = 0; i < nentries; i++)
    {
      const double * flags = alignment->GetV4();
      const double flag = flags[i];

      if (flag != 1)
        {
          cout << "no hit at " << i << ", flag = " << flag << endl;
          ;
          continue;
        }
      // Create the actual TEvePointSets
      TString name_station = name + Form("Coord %d", i);

      TEvePointSet * coords = new TEvePointSet(name_station.Data());

      // Set the point style - 20 means resizable spheres
      coords->SetMarkerStyle(20);
      // Make them white
      coords->SetMarkerColor(c);
      // Make them smaller
      coords->SetMarkerSize(0.5);
      // Add the station sets to the quadrant lists
      ne_coord_list->AddElement(coords);

      const double * xs = alignment->GetV1();
      const double * ys = alignment->GetV2();
      const double * zs = alignment->GetV3();

      const double x = xs[i];
      const double y = ys[i];
      const double z = zs[i];

      cout << "coords->SetNextPoint(" << x << "," << y << "," << z << ");"
          << endl;
      coords->SetNextPoint(x, y, z);

      if (minz > abs(z))
        {
          station0 = TEveVectorD(x, y, z);
          minz = abs(z);
        }
      if (maxz < abs(z))
        {
          station3 = TEveVectorD(x, y, z);
          maxz = abs(z);
        }
    }

  MakeTrackFromFit(track_list, station0, station3, z0reco, name, c);

}

void
MakeTrackFromFit(TEveElementList * track_list, const TEveVectorD station0,
    const TEveVectorD station3, const double z0reco, TString name, Color_t c)
{

  assert(track_list);

  // Create a list of tracks for each arm
  TEveTrackList *list = new TEveTrackList(name + "Track Fit");
  // Add them to the main list
  track_list->AddElement(list);

  const TEveVectorD vertex = station0
      + (station3 - station0)
          * ((z0reco - station0.fZ) / (station3.fZ - station0.fZ));

  const TEveVectorD extrapolation = station0
      + (station3 - station0)
          * ((station3.fZ * 1.5 - station0.fZ) / (station3.fZ - station0.fZ));

  // Create a B=0 propagator to propagate the tracks
  TEveTrackPropagator* prop = new TEveTrackPropagator();

  // Create a reconstructed track object
  TEveRecTrackD* trk_reco = new TEveRecTrackD();
  // Set the "vertex"
  trk_reco->fV = vertex;
  // Set the initial momentum vector
  trk_reco->fP = station3 - station0;

  // Create a TEveTrack from the reconstructed track and the B=0 propagator
  TEveTrack* trk = new TEveTrack(trk_reco, prop);
  // Make it cyan
  trk->SetLineColor(c);

  // Add a reference pathmark that forces the track to pass through a station 3 point
  // TEvePathMarkD::kReference is crucial.  Other types for decays, etc.
  trk->AddPathMark(TEvePathMarkD(TEvePathMarkD::kReference, extrapolation));

  // Use the reference points
  trk->SetRnrPoints(kTRUE);
  // Apply the propagator to the track
  trk->MakeTrack();

  list->AddElement(trk);
}

void
MakeStripHits(TTree * alignment, const int arm, TEveElementList* coord_list,
    const double phi_acpt_cent, const double phi_acpt_width, const double vtxxp, const double vtxyp)
{
  assert(alignment);
  assert(coord_list);

  Long64_t nentries = alignment->Draw("flag:w_det:sector_det:column_det", "",
      "goff");
  assert(nentries == 8);

  const double * flags = alignment->GetV1();
  const double * w_dets = alignment->GetV2();
  const double * sector_dets = alignment->GetV3();
  const double * column_dets = alignment->GetV4();

  for (Long64_t i = 0; i < nentries; i++)
    {
      cout << "Checking " << i << " " << endl;

      const int flag = flags[i];
      const double w_det = w_dets[i];
      const int sector = (int) (sector_dets[i]) % FVTXGEOM::NumberOfSectors;
      const int cage = (int) (sector_dets[i]) / FVTXGEOM::NumberOfSectors;
      const int column_det = column_dets[i];
      const int station = i / 2;

      if (flag == 1)
        {

          FvtxColumn * col =
              FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(
                  sector)->get_column(column_det);
          assert(col);

          TGeoNode * node = FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(
              station)->get_sector(sector)->get_phy_node()->GetNode();
          cout << "Node Name = " << node->GetName() << "  at "
              << col->get_phy_node()->GetName() << endl;
          node->SetVisibility(kTRUE);
          node->SetVisDaughters(kTRUE);

          TObjArray* sub4_nodes = node->GetVolume()->GetNodes();
          assert(sub4_nodes);

          for (unsigned int n = 0; n < node->GetVolume()->GetNdaughters(); n++)
            { // sub wedge structure, HDI, sensor, chips, etc.
              // Make volumes down two levels transparent
              TGeoNode* tmp_node5 = (TGeoNode*) sub4_nodes->At(n);
              assert(tmp_node5);

              TString name = tmp_node5->GetName();
              cout << "Process " << name << " - " << name.CompareTo("SISB_1")
                  << " - " << name.CompareTo("SISS_1") << endl;
//
              if ((name.CompareTo("SISB_1") == 0)
                  || (name.CompareTo("SISS_1") == 0))
                {
                  tmp_node5->SetVisibility(kTRUE);
                  tmp_node5->VisibleDaughters(kTRUE);
                }
              else
                {
                  tmp_node5->SetVisibility(kFALSE);
                  tmp_node5->VisibleDaughters(kFALSE);
                }
            }

          double angle = col->get_strip(0)->get_angle();
          angle = (angle < -M_PI_2) ? angle + M_PI : angle;
          angle = (angle > M_PI_2) ? angle - M_PI : angle;

          const double x_det = w_det * cos(angle + M_PI_2);
          const double y_det = w_det * sin(angle + M_PI_2);

          cout << "Found Strip hit " << i << " (" << x_det << "," << y_det
              << ");" << endl;

          FvtxStrip* central_strip = col->find_strip(
              PHPoint(x_det, y_det, col->get_z()));
          assert(central_strip);
          central_strip->print();

          TEveVector pb(central_strip->get_position_begin().getX(), //Float_t x1, //
              central_strip->get_position_begin().getY(), //          Float_t y1, //
              central_strip->get_position_begin().getZ() //          Float_t z1,
              );

          TEveVector pe(central_strip->get_position_end().getX(), //Float_t x1, //
              central_strip->get_position_end().getY(), //          Float_t y1, //
              central_strip->get_position_end().getZ() //          Float_t z1,
              );

          // Peak strip

          TEveStraightLineSet * clusters = new TEveStraightLineSet(
              Form("StripHit %d", i));
          assert(clusters);
          clusters->SetLineColor(kGreen);
          clusters->SetLineWidth(3);
//
          clusters->AddLine(pb, pe);

          // Add the station sets to the quadrant lists
          coord_list->AddElement(clusters);

          // phi aceptance

          for (int sign = 1; sign >= -1; sign -= 2)
            {

              const double phi = phi_acpt_cent + phi_acpt_width / 2 * sign;
              const double radius = abs(w_det * 1.2);

              TEveStraightLineSet * clusters = new TEveStraightLineSet(
                  Form("PhiAccept %d", i));
              assert(clusters);
              clusters->SetLineColor(phi_acpt_width > 0 ? kGreen : kRed);
              clusters->SetLineWidth(1);
              clusters->SetLineStyle(3);
              //
              clusters->AddLine(TEveVector(vtxxp, vtxyp, col->get_z()),
                  TEveVector(radius * cos(phi) + vtxxp, radius * sin(phi) + vtxyp,
                      col->get_z()));

              // Add the station sets to the quadrant lists
              coord_list->AddElement(clusters);
            }

        }

    }

}
