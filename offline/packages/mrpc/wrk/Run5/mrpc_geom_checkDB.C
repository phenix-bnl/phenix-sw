void mrpc_geom_checkDB()
{
  // __________________________________________________________
  //
  //  This macro does the DB check for MRPC geom.
  //  
  //  Run 146651: the first run for MRPC (TOF.W) time-in.
  //  (2005-01-06 04:51:34). 
  //  The end run is not specified, i.e. put the "FarFuture"
  //  EndTime by default. -- T.Chujo (1/31/2005)
  // __________________________________________________________

  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("/phenix/data20/chujo/MRPC/offline/install/lib/libmrpc.so");
  
  // === MRPC Geom ===
  MrpcGeometry* geom = new MrpcGeometry();
  geom->set_debug(1);

  // === Check DB (Run 146651) ===
  int run = 146651;
  geom->fetch(run);

  // === Print out ===
  geom->print();

}
