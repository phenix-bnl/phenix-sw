////////////////////////////////////////////////////////////////////////////////
//
// Update the PHCentralTrack phi0 & theta0 rotation values in the DB.
//
////////////////////////////////////////////////////////////////////////////////
//
// Darren McGlinchey
// 12/31/2014
//
////////////////////////////////////////////////////////////////////////////////
//
// Usage:
//   For now, update the "dphi", "dthe" arrays and the run range.
//   Don't forget to update "desc" with an approriate description
//
////////////////////////////////////////////////////////////////////////////////

#include <iostream>

using namespace std;

void updateDBPHCntRotations()
{

  gSystem->Load("libsvx.so");

  //===============================================================//
  // SET COMMIT VALUES
  //===============================================================//

  // Arrays holding the values to be committed
  // 0:East 1:West
  float dphi[2] = {0.0042, -0.0066}; //dphi values to be committed
  float dthe[2] = {0.0074, -0.0036}; //dtheta values to be committed

  // Run range
  int beginRun = 405839;
  int endRun = 414988;
  int testRun = 407951;

  // Commit description
  string desc = "(D. McGlinchey) - Values for Run 14 AuAu 200";


  //===============================================================//
  // TEST THE VALUES BEFORE TRYING TO COMMIT
  // (HANDLED IN SvxCentralTrackReco AS WELL)
  //===============================================================//


  // test run range
  if (beginRun < 0 || endRun < 0 || endRun < beginRun)
  {
    cout << "ERROR!!! Invalid run range! " << endl;
    cout << "         beginRun = " << beginRun << endl;
    cout << "         endRun   = " << endRun << endl;

    return;
  }

  // test that the "testRun" is within the run range
  if ( testRun < beginRun || testRun > endRun)
  {
    cout << "ERROR!! testRun should be within the run range!" << endl;
    cout << "        run range = " << beginRun << " - " << endRun << endl;
    cout << "        test run  = " << testRun << endl;

    return;
  }

  //test the description
  if (desc.length() == 0)
  {
    cout << "ERROR!! Please input non-zero description" << endl;
    cout << "        desc = " << desc << endl;
    return;
  }

  if (desc.length() > 256)
  {
    cout << "ERROR!! desc must be < 256 characters! Please truncate!" << endl;
    cout << "        desc.length() = " << desc.length() << endl;
    cout << "        desc = " << desc << endl;
  }

  cout << endl;
  cout << "--> Desired values to be committed" << endl;
  cout << " 0(East)   : " << dphi[0] << " " << dthe[0] << endl;
  cout << " 1(West)   : " << dphi[1] << " " << dthe[1] << endl;
  cout << " run range : " << beginRun << " - " << endRun << endl;
  cout << " desc      : " << desc << endl;

  //===============================================================//
  // COMMIT THE VALUES
  //===============================================================//
  cout << endl;
  cout << "--> Committing values" << endl;

  /// SvxCentralTrackReco should be called after PHCentralTrack is reconstructed.
  SvxCentralTrackReco *svxcentraltrack = new SvxCentralTrackReco();
  svxcentraltrack->Verbosity(10);
  svxcentraltrack->setPHCentralTrackDphiDtheta(0, dphi[0], dthe[0]);
  svxcentraltrack->setPHCentralTrackDphiDtheta(1, dphi[1], dthe[1]);

  cout << endl;
  cout << "  Values as set in SvxCentralTrackReco:" << endl;
  cout << "    0(East): "
       << svxcentraltrack->getPHCentralTrackDphi(0) << " "
       << svxcentraltrack->getPHCentralTrackDtheta(0)
       << endl;
  cout << "    1(Wast): "
       << svxcentraltrack->getPHCentralTrackDphi(1) << " "
       << svxcentraltrack->getPHCentralTrackDtheta(1)
       << endl;

  //Try updating!
  cout << endl;
  svxcentraltrack->updateDBPHCentralTrackDphiDtheta(
    beginRun, endRun, desc.c_str());

  //===============================================================//
  // TRY FETCHING THE VALUES JUST COMMITTED
  //===============================================================//

  //make a new instance of SvxCentralTrackReco and try to fetch the values
  SvxCentralTrackReco *svxcentraltrack2 = new SvxCentralTrackReco();
  svxcentraltrack2->Verbosity(10);
  svxcentraltrack2->fetchDBPHCentralTrackDphiDtheta(testRun);

  cout << endl;
  cout << "--> Values from DB" << endl;
  cout << " 0(East): "
       << svxcentraltrack2->getPHCentralTrackDphi(0) << " "
       << svxcentraltrack2->getPHCentralTrackDtheta(0)
       << endl;
  cout << " 1(Wast): "
       << svxcentraltrack2->getPHCentralTrackDphi(1) << " "
       << svxcentraltrack2->getPHCentralTrackDtheta(1)
       << endl;


}