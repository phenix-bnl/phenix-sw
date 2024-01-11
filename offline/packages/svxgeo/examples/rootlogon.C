/*
   rootlogon.C
   This script is automatically invoked whenever ROOT is started.
   Add session-level configurations as needed.
*/

void rootlogon()
{
  Printf("This is %s/rootlogon.C.", gSystem->Getenv("PWD"));
  Printf("Starting ROOT version %s", gROOT->GetVersion());

  // Include paths
  // =============
  // There are 2 separate paths: one for ACLiC, and one for CINT or CLING.
  // 1. ACLiC include path
  const char *inc = gSystem->DirName(gSystem->WorkingDirectory());
  gSystem->AddIncludePath(Form("-I%s ", inc));

  // 2. Interpreter include path
#ifdef __CINT__
  //  Printf("__CINT__ is defined");
  gROOT->ProcessLine(Form(".include %s", inc));
#endif
#ifdef __CLING__
  //  Printf("__CLING__ is defined");
  gROOT->ProcessLine(Form(".I %s", inc));
#endif

  // Put build files (.d, .so, dicts) in $TMPDIR
  // ===========================================
  const char *tmpdir  = gSystem->Getenv("TMPDIR");
  if (tmpdir)
    gSystem->SetBuildDir(tmpdir);

  // Build libraries
  // ===============
  gROOT->LoadMacro("../SvxTGeo.C+");
  gROOT->LoadMacro("../SvxGeoTrack.C+");
  gROOT->LoadMacro("../SvxProj.C+");
}
