#include<iostream>
#include<PHGeometry.h>
#include<TMutGeo.h>
#include<MutStrip.h>
int main()
{    
  std::cout << "station 1" << std::endl;
  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,0,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,0,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,0,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,0,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,0,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,0,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,1,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,1,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,1,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,1,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,1,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,1,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,2,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,2,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,2,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,0,0,0,2,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,0,0,0,2,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,0,0,0,2,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << "station 2" << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,0,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,0,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,0,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,0,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,0,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,0,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,1,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,1,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,1,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,1,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,1,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,1,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,2,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,2,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,2,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,1,0,0,2,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,1,0,0,2,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,1,0,0,2,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << "station 3" << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,2,0,0,0,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,2,0,0,0,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,2,0,0,0,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,2,0,0,0,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,2,0,0,0,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,2,0,0,0,1,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,2,0,0,1,0) << " "
            << TMutGeo::get_angle_cathode_anode(0,2,0,0,1,0)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,2,0,0,1,0,0)->getGlobalPositionBegin().getZ() << std::endl;

  std::cout << TMutGeo::get_angle_cathode_anode(0,2,0,0,1,1) << " "
            << TMutGeo::get_angle_cathode_anode(0,2,0,0,1,1)*180/3.14 << "   z: "
	    << TMutGeo::get_strip_geom(0,2,0,0,1,1,0)->getGlobalPositionBegin().getZ() << std::endl;
  return 0;
}









