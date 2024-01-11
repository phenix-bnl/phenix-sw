void mrpc_geom_update()
{
  // __________________________________________________________
  //
  //  This macro does the DB update for MRPC geom.
  //  
  //  Run 146651: the first run for MRPC (TOF.W) time-in.
  //  (2005-01-06 04:51:34). 
  //  The end run is not specified, i.e. put the "FarFuture"
  //  EndTime by default. -- T.Chujo (1/31/2005)
  // __________________________________________________________

  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libmrpc.so");

  // MRPC Geometry
  MrpcGeometry* geometry = new MrpcGeometry();
  geometry->set_debug(1);

  //Update
  geometry->fetch("./geompar/mrpc_panelgeo_Run5_05.0427.txt");
  geometry->print(10);
  geometry->update(146651);
  geometry->write("./geompar/out/mrpc_panelgeo_out_05.0427.txt");
}
