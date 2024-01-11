#include<TMuiKeyGen.h>
#include<MUIOO.h>

const unsigned long TMuiKeyGen::PLANE_BITS=4;
const unsigned long TMuiKeyGen::PANEL_BITS=4;
const unsigned long TMuiKeyGen::ORIENTATION_BITS=2;
const unsigned long TMuiKeyGen::TWOPACK_BITS=8;
const unsigned long TMuiKeyGen::INDEX_BITS=12;	
	
const unsigned long TMuiKeyGen::ARM_MASK = 0x00000003;
const unsigned long TMuiKeyGen::PLANE_MASK = 0x0000000F;
const unsigned long TMuiKeyGen::PANEL_MASK = 0x0000000F;
const unsigned long TMuiKeyGen::ORIENTATION_MASK = 0x00000003;
const unsigned long TMuiKeyGen::TWOPACK_MASK = 0x000000FF;
const unsigned long TMuiKeyGen::INDEX_MASK = 0x00000FFF; 

const unsigned long TMuiKeyGen::INDEX_SHIFT=0;
const unsigned long TMuiKeyGen::TWOPACK_SHIFT = TMuiKeyGen::INDEX_BITS + TMuiKeyGen::INDEX_SHIFT;
const unsigned long TMuiKeyGen::ORIENTATION_SHIFT = TMuiKeyGen::TWOPACK_BITS + TMuiKeyGen::TWOPACK_SHIFT;
const unsigned long TMuiKeyGen::PANEL_SHIFT = TMuiKeyGen::ORIENTATION_BITS + TMuiKeyGen::ORIENTATION_SHIFT;
const unsigned long TMuiKeyGen::PLANE_SHIFT = TMuiKeyGen::PANEL_SHIFT + TMuiKeyGen::PANEL_BITS;
const unsigned long TMuiKeyGen::ARM_SHIFT = TMuiKeyGen::PLANE_SHIFT + TMuiKeyGen::PLANE_BITS;


//______________________________________________________________
/* Keys for TMuiHitO */
PHKey::object_key_type TMuiKeyGen::get_key(
  const UShort_t& i_arm,
  const UShort_t& i_plane,
  const UShort_t& i_panel,
  const UShort_t& i_orientation,
  const UShort_t& i_twopack,
  const UShort_t& i_index)
{

  // shift and mask					 
	return	
		((ARM_MASK & i_arm) << ARM_SHIFT) |
		((PLANE_MASK & i_plane) << PLANE_SHIFT) |
		((PANEL_MASK & i_panel) << PANEL_SHIFT) |
		((ORIENTATION_MASK & i_orientation) << ORIENTATION_SHIFT) |
		((TWOPACK_MASK & i_twopack) << TWOPACK_SHIFT) |
		((INDEX_MASK & i_index) << INDEX_SHIFT);

}

//______________________________________________________________
/* Keys for TMuiClusterO */
PHKey::object_key_type TMuiKeyGen::get_key(
  const UShort_t& i_arm,
  const UShort_t& i_plane,
  const UShort_t& i_panel,
  const UShort_t& i_orientation,
  const UShort_t& i_index)
{

  // shift and mask					 
	return	
		((ARM_MASK & i_arm) << ARM_SHIFT) |
		((PLANE_MASK & i_plane) << PLANE_SHIFT) |
		((PANEL_MASK & i_panel) << PANEL_SHIFT) |
		((ORIENTATION_MASK & i_orientation) << ORIENTATION_SHIFT) |
		((INDEX_MASK & i_index) << INDEX_SHIFT);
  
}

//______________________________________________________________
TMuiKeyGen::key_range TMuiKeyGen::get_key_range(
  const UShort_t& i_arm, 
  const UShort_t& i_plane, 
  const UShort_t& i_panel, 
  const UShort_t& i_orientation,
  const UShort_t& i_twopack )
{
						
	return std::make_pair(
    get_key(i_arm, i_plane, i_panel, i_orientation, i_twopack, 0),
    get_key(i_arm, i_plane, i_panel, i_orientation, i_twopack+1, 0)-1);
  
}

//______________________________________________________________
TMuiKeyGen::key_range TMuiKeyGen::get_key_range(
  const UShort_t& i_arm, 
  const UShort_t& i_plane, 
  const UShort_t& i_panel, 
  const UShort_t& i_orientation)
{
						
	return std::make_pair(
    get_key(i_arm, i_plane, i_panel, i_orientation, 0),
    get_key(i_arm, i_plane, i_panel, i_orientation+1, 0)-1); 
  
}


//______________________________________________________________
PHKey::object_key_type TMuiKeyGen::get_key(
  const UShort_t& i_arm, 
  const UShort_t& i_plane, 
  const UShort_t& i_panel,
  const UShort_t& index)
{
	// shift and mask					 
	return	((ARM_MASK & i_arm) << ARM_SHIFT) |
			((PLANE_MASK & i_plane) << PLANE_SHIFT) |
      ((PANEL_MASK & i_panel) << PANEL_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}

//______________________________________________________________
TMuiKeyGen::key_range TMuiKeyGen::get_key_range(
  const UShort_t& i_arm, 
  const UShort_t& i_plane, 
  const UShort_t& i_panel  )
{
						
	return std::make_pair(
    get_key(i_arm, i_plane, i_panel, 0),
    get_key(i_arm, i_plane, i_panel + 1, 0)-1); 
}

//______________________________________________________________
PHKey::object_key_type TMuiKeyGen::get_key(
  const UShort_t& i_arm, 
  const UShort_t& i_plane, 
  const UShort_t& index)
{
	// shift and mask					 
	return	((ARM_MASK & i_arm) << ARM_SHIFT) |
			((PLANE_MASK & i_plane) << PLANE_SHIFT) |
			((INDEX_MASK & index) << INDEX_SHIFT);
}


//______________________________________________________________
TMuiKeyGen::key_range TMuiKeyGen::get_key_range(
	const UShort_t& i_arm, 
	const UShort_t& i_plane)
{
					
	return std::make_pair(
    get_key(i_arm, i_plane, 0),
    get_key(i_arm, i_plane+1, 0)-1); 
  
}

//______________________________________________________________
/* Keys for TMuiRoadO */
PHKey::object_key_type TMuiKeyGen::get_key(const UShort_t& i_arm, const UShort_t& i_index)
{
	// shift and mask					 
	return ((ARM_MASK & i_arm) << ARM_SHIFT) | ((INDEX_MASK & i_index) << INDEX_SHIFT);
}

//______________________________________________________________
TMuiKeyGen::key_range TMuiKeyGen::get_key_range(const UShort_t& i_arm)
{ return std::make_pair(get_key(i_arm, 0), get_key(i_arm+1,0)-1); }

//______________________________________________________________
int TMuiKeyGen::get_index(PHKey::object_key_type tmpKey)
{ return (INDEX_MASK&tmpKey); }
