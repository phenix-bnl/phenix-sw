void fcal(const Int_t maxEvents=1,
	  const char *prdfIFile="prdf.prdf",
	  const char *dstOFile="dst.root"){

  gSystem->Load("libfcl.so");
  gSystem->Load("libpreco.so");


  recoConsts *rc = recoConsts::instance(); // instantiate flags

  rc->set_SysFlag("MVD",0);  // 0 means the system is OFF, 1 means system is ON
  rc->set_SysFlag("ZDC",1);
  rc->set_SysFlag("BBC",1);
  rc->set_SysFlag("VTX",0);
  rc->set_SysFlag("DCH",0);
  rc->set_SysFlag("PAD",0);  
  rc->set_SysFlag("CRK",0);
  rc->set_SysFlag("TEC",0); 
  rc->set_SysFlag("TOF",0);
  rc->set_SysFlag("EMC",0);
  rc->set_SysFlag("CGL",0); 
  rc->set_SysFlag("MOM",0); // MOM is not in use
  rc->set_SysFlag("MUTR",0); 
  rc->set_SysFlag("MUID",0);
  rc->set_SysFlag("TRIG",0);
  rc->set_SysFlag("NTC",0);  // NTC is not yet in Off-Line code
  rc->set_SysFlag("LVL2",0); // LVL2 is not yet in Off-Line code
  rc->set_SysFlag("SPIN",0); // SPIN is not yet in Off-Line code
  rc->set_SysFlag("TZR",0); // 
  rc->set_SysFlag("ERT",0); // 

  rc->set_SysFlag("FCL",1);

  pfileopen(prdfIFile);     // open the PRDF input file
  poutfileopen(dstOFile);   // open the DST output file
  pidentify();              // identify the next event run from the data stream
  prun(maxEvents);          // run over requested events (-1 is all events)
  pend();                   // clean up
  pexit();                  // exit

}
