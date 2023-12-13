open Vector3;;
open Globals;;
open Shapes;;

type physics_body = {
  velocity: ref vec3;
  mass: float;
  e: float;
  shp: shape_info;
  is_fixed: bool;
};;

let move phyx_obj ms_since_update =
  if (phyx_obj.is_fixed) then phyx_obj
  else
    phyx_obj.velocity := add !phyx_obj.velocity (scale gravity ms_since_update) in
    phyx_obj.shp.position := add !phyx_obj.velocity phyx_obj.shp.position