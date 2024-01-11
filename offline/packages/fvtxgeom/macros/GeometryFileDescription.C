/*
 * DrawResidu.C
 *
 *  Created on: Aug 22, 2012
 *      Author: Jin Huang <jhuang@bnl.gov>
 *
 *  Give FVTX Geometry file a name, which prints out during replay
 */

#include <cassert>
#include <iostream>

using namespace std;

void
GeometryFileDescription(TString geom_file = "Fvtxgeom_new.root")
{
  cout <<"Opening "<<geom_file <<" for editing"<<endl;

  TFile *_file0 = TFile::Open(geom_file, "update");
  assert(_file0);

  TGeoManager * _fGeom = (TGeoManager *) _file0->GetObjectChecked("PHENIX",
      "TGeoManager");
  assert(_fGeom);

  std::cout
      << "FvtxGeom::load_root_geometry: FVTX Geometry Loaded with description"
      << std::endl;
  std::cout << _fGeom->GetTitle() << std::endl;

  string new_name;

  std::cout
      << "Enter the new name:"
      << std::endl;
  getline(cin, new_name);

  _fGeom->SetTitle(new_name.c_str());

  _fGeom->Write("PHENIX",TObject::kWriteDelete);

  _file0 -> ls();

  _file0->Close();

}
