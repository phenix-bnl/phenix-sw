void mrpc_calib_checkDB()
{
  // __________________________________________________________
  //
  //  This macro does the DB check for MRPC calib.
  //  
  //  Run 146651: the first run for MRPC (TOF.W) time-in.
  //  (2005-01-06 04:51:34). 
  //  The end run is not specified, i.e. put the "FarFuture"
  //  EndTime by default. -- T.Chujo (1/31/2005)
  // __________________________________________________________

  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("/phenix/data20/chujo/MRPC/offline/install/lib/libmrpc.so");
  
  // === MRPC Calib ===
  MrpcCalib* calib = new MrpcCalib();
  calib->set_debug(1);

  // === Check DB (Run 146651) ===
  int run = 146651;

  calib->fetch(run);
  /*
  calib->fetchTvcConv(run);
  calib->fetchTvcPede(run);
  calib->fetchQvcConv(run);
  calib->fetchQvcPede(run);
  calib->fetchGlobalT0(run);
  calib->fetchSlatT0(run);
  calib->fetchSlewParA(run);
  calib->fetchSlewParB(run);
  calib->fetchYoffset(run);
  calib->fetchVelocity(run);
  */

  // === Print out ===
  calib->print_tvcconv();
  calib->print_tvcpede();
  calib->print_qvcconv();
  calib->print_qvcpede();
  calib->print(7);

}
