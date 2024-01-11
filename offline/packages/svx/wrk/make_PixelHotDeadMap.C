void make_PixelHotDeadMap(int runstart, int runend, const char *infilelist)
///
///
/// Before running this macro, output direcotries should be created.
/// In the default setup, you should create the following directoies where
/// this macro is:
///   - ratechip_data_temp
///   - raterun_data_temp
///   - pixelmap_temp
/// You can change the name of those directories by editting ratechip.C,
/// raterun.C, raterunplot.C, and ratepixel.C
///
///
/// - runstart, runend :
///    Only runs between runstart and runend (includes runstart and runend)
///    are analyzed.
///
/// - infilelist :
///    Files of SvxDstQA_run12_vtx_online-0000?????-0000.root for the runs
///    which you want to analyze should be listed in infilelist.
///
/// Output files are made at the following derectories
///   - raterun_date_temp
///   - pixelmap_temp
///
{
  ///
  /// convert files which are made by online production
  /// to what is needed to make hot/dead map
  ///
  ifstream ifs;
  cout << infilelist << endl;
  ifs.open(infilelist);
  if ( ifs.fail() ) {
    cout << "FAIL TO READ INPUT FILE" << endl;
    ifs.close();
    return;
  }
  string file;
  TString cmd;
  while ( getline(ifs, file) ) {
    cout << file.c_str() << endl;
    cmd = ".x ratechip.C(\"";
    cmd += file;
    cmd += "\")";
    cout << cmd << endl;
    gROOT->ProcessLine(cmd);
  }
  ifs.close();

  ///
  /// combine files for all runs to make hot/dead map
  ///
  cmd = ".x raterun.C(";
  cmd += runstart;
  cmd += ",";
  cmd += runend;
  cmd += ")";
  gROOT->ProcessLine(cmd);
  
  ///
  /// update ratecut file
  ///
  gROOT->ProcessLine(".L raterunplot.C");
  TString infile = "raterun_data_temp/raterun_";
  infile += runstart;
  infile += "_";
  infile += runend;
  infile += ".root";
  TString ratecutfile = "raterun_data_temp/ratecut-combine_";
  ratecutfile += runstart;
  ratecutfile += "_";
  ratecutfile += runend;
  ratecutfile += ".txt";
  fitchipall(runstart, runend, infile, ratecutfile);

  ///
  /// run again to make chip map with updated ratecut file
  ///
  cmd = ".x raterun.C(";
  cmd += runstart;
  cmd += ",";
  cmd += runend;
  cmd += ",\"";
  cmd += ratecutfile;
  cmd += "\")";
  gROOT->ProcessLine(cmd);

  ///
  /// make pixel map
  ///
  cmd = ".x ratepixel.C(\"";
  cmd += infile;
  cmd += "\",\"";
  cmd += ratecutfile;
  cmd += "\")";
  gROOT->ProcessLine(cmd);
}
