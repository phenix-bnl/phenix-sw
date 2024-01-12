#include <TFile.h>
//INCLUDECHECKER: Removed this line: #include <TString.h>
#include <string>
#include <iostream>
#include <fstream>
#include <MUTOO.h>
#include <MWGpico.h>

using namespace std;

//================================== LoadNano ====================================//
void MWGpico::LoadNano( const char* choice, const char* where)
{
  // some dump
  MUTOO::PRINT( cout, "MWGpico::LoadNano" );
  cout << "choice: " << choice << endl;
  cout << "where: " << where << endl;

  // global stuff
  _choice = choice;
  bool badrun=false;
  if (badrun) cout << "WARNING : bad runs are included !" << endl;
  TString dir;

  //=== file or simu_file
  if( _choice == "file" || _choice == "simu_file") {
    ifstream from( _nano_file.c_str() );
    cout <<"Reading file list from " << _nano_file <<endl;
    if (!from) {cout<<"WARNING: file is missing"<<endl; return;}
    
    char filename[1024];
    while (!from.eof()) {
      from.getline(filename,1024);
      if( !(strlen( filename )) ) continue;
      if( strncmp( filename, "//", 2 ) == 0 ) continue;
      if( TFile* test = TFile::Open(filename)) 
      { 
	      _nano_list.push_back(filename);
	      test->Close();
	    } else cout << "Corrupted file" << filename << endl;
    }
  }

  //=== CuCu05
  if (_choice=="CuCu05N") { // CuCu05 data
    _type = "CuCu";
    ifstream from("NanoListCuCu05N.txt");
    cout<<endl<<"Reading file list from NanoListCuCu05N.txt"<<endl;
    while (!from.eof()) {
      TString filename; filename.ReadLine(from);
      if (!filename.IsNull()) {
	if (TFile* test = TFile::Open(filename)) {
	  _nano_list.push_back(filename);
	  test->Close();
	} else cout << "Corrupted file" << filename << endl;
      }
    }
  }
  if (_choice=="CuCu05S") { // CuCu05 data
    _type = "CuCu";
    ifstream from("NanoListCuCu05S.txt");
    cout<<endl<<"Reading file list from NanoListCuCu05S.txt"<<endl;
    while (!from.eof()) {
      TString filename; filename.ReadLine(from);
      if (!filename.IsNull()) {
	if (TFile* test = TFile::Open(filename)) {
	  _nano_list.push_back(filename);
	  test->Close();
	} else cout << "Corrupted file" << filename << endl;
      }
    }
  }

  //=== AuAu04
  if (_choice=="AuAu04") { // AuAu04 data
    _type = "AuAu";
    ifstream from("NanoListAuAu04.txt");
    cout<<endl<<"Reading file list from NanoListAuAu04.txt"<<endl;
    while (!from.eof()) {
      TString filename; filename.ReadLine(from);
      if (!filename.IsNull()) {
	if (TFile* test = TFile::Open(filename)) {
	  _nano_list.push_back(filename);
	  test->Close();
	} else cout << "Corrupted file" << filename << endl;
      }
    }
  }

  //=== pp04
  if (_choice=="pp04") { // pp04 data
    _type = "pp";
    ifstream from("NanoListpp04.txt");
    cout<<endl<<"Reading file list from NanoListpp04.txt"<<endl;
    while (!from.eof()) {
      TString filename; filename.ReadLine(from);
      if (!filename.IsNull()) {
	if (TFile* test = TFile::Open(filename)) {
	  _nano_list.push_back(filename);
	  test->Close();
	} else cout << "Corrupted file" << filename << endl;
      }
    }
  }

  //=== dAu03N2D or dAu03N1D1S or dAu03S2D or dAu03S1D1S
  // if (_choice == "dAu03N2D" || _choice == "dAu03NDS" || 
  //     _choice == "dAu03S2D" || _choice == "dAu03SDS") { 
  //   _type = "dAu";
  //   if (strcmp(where,"CCF") == 0) dir = "/sps/phenix/Run3dAu/";
  //   if (strcmp(where,"RCF") == 0) {
  //     if (_choice == "dAu03N2D") dir = "/direct/phenix+data25/nanoDSTs/Run3dAu_N2D/";
  //     if (_choice == "dAu03NDS") dir = "/direct/phenix+data25/nanoDSTs/Run3dAu_N1D1S/";
  //     if (_choice == "dAu03S2D") dir = "/direct/phenix+data25/nanoDSTs/Run3dAu_S2D/";
  //     if (_choice == "dAu03SDS") dir = "/direct/phenix+data25/nanoDSTs/Run3dAu_S1D1S/";
  //   }
  //   ifstream from("NanoListdAu03.txt");
  //   cout<<endl<<"Reading file list from NanoListdAu03.txt"<<endl;
  //   if (!from) {cout<<"WARNING: input file NanoListdAu03.txt is missing"<<endl; return;}
  //   while (!from.eof()) {
  //     TString filename; filename.ReadLine(from);
  //     if (!filename.IsNull()) {
	// if (!filename.Contains("empty") &&
	//     !filename.Contains("missing")) {
	//   if ((_choice=="dAu03N2D" && filename.Contains("N2D")) ||
	//       (_choice=="dAu03NDS" && filename.Contains("N1D1S")) ||
	//       (_choice=="dAu03S2D" && filename.Contains("S2D")) ||
	//       (_choice=="dAu03SDS" && filename.Contains("S1D1S"))) {
	//     if (!badrun && !filename.Contains("bad")) {
	//       filename.Remove(filename.Index("root")+4);
	//       if (TFile* test = TFile::Open(dir+filename)) {
	// 	_nano_list.push_back(dir+filename);
	// 	test->Close();
	//       }
	//       else cout << "Corrupted file" << dir+filename << endl;
	//     }
	//     if (badrun && filename.Contains("bad")) {
	//       filename.Remove(filename.Index("root")+4);
	//       if (TFile* test = TFile::Open(dir+filename)) {
	// 	_nano_list.push_back(dir+filename);
	// 	test->Close();
	//       } else cout << "Corrupted file" << dir+filename << endl;
	//     }
	//   }
	// }
  //     }
  //   }
  // }

  // //=== pp03S or pp03N
  // if (_choice == "pp03S" || _choice == "pp03N") { 
  //   _type = "pp";
  //   if (strcmp(where,"CCF") == 0) dir = "/sps/phenix/Run3pp/";
  //   if (strcmp(where,"RCF") == 0) {
  //     if (_choice == "pp03S") dir = "/direct/phenix+data25/nanoDSTs/Run3pp_N1D1S/";
  //     if (_choice == "pp03N") dir = "/direct/phenix+data25/nanoDSTs/Run3pp_S1D1S/";
  //   }
  //   ifstream from("NanoListpp03.txt");
  //   cout<<endl<<"Reading file list from NanoListpp03.txt"<<endl;
  //   if (!from) {cout<<"WARNING: input file NanoListpp03.txt is missing"<<endl; return;}
  //   while (!from.eof()) {
  //     TString filename; filename.ReadLine(from);
  //     if (!filename.IsNull()) {
	// if (!filename.Contains("empty") &&
	//     !filename.Contains("missing")) {
	//   if ((_choice=="pp03S" && filename.Contains("S1D1S")) ||
	//       (_choice=="pp03N" && filename.Contains("N1D1S"))) {
	//     if (!badrun && !filename.Contains("bad")) {
	//       filename.Remove(filename.Index("root")+4);
	//       if (TFile* test = TFile::Open(dir+filename)) {
	// 	_nano_list.push_back(dir+filename);
	// 	test->Close();
	//       } else cout << "Corrupted file" << dir+filename << endl;
	//     }
	//     if (badrun && filename.Contains("bad")) {
	//       filename.Remove(filename.Index("root")+4);
	//       if (TFile* test = TFile::Open(dir+filename)) {
	// 	_nano_list.push_back(dir+filename);
	// 	test->Close();
	//       } else cout << "Corrupted file" << dir+filename << endl;
	//     }
	//   }
	// }
  //     }
  //   }
  // }

  //=== AuAu02
  if (_choice=="AuAu02") { // AuAu02 data
    _type = "AuAu";
    ifstream from("NanoListAuAu02.txt");
    cout<<endl<<"Reading file list from NanoListAuAu02.txt"<<endl;
    while (!from.eof()) {
      TString filename; filename.ReadLine(from);
      if (!filename.IsNull()) {
	if (TFile* test = TFile::Open(filename)) {
	  _nano_list.push_back(filename);
	  test->Close();
	} else cout << "Corrupted file" << filename << endl;
      }
    }
  }


  cout<<endl<<endl<<"------ nanoDSTs path = "<<dir<<" -----"<<endl;
  MUTOO::PRINT( cout, "**" );
}
