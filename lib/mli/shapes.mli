
val get_sphere_normal: vec3 -> vec3 -> vec3

type sphere_info = {
  center: vec3;
  radius: float;
}

type plane_info = {
  normal: vec3;
  point: vec3;
}

type shape_info =
  | Sphere of sphere_info
  | Plane of plane_info;;

type shape_attr = {
  colour: int;
  k_d: float;
  k_s: float;
  phong_alpha: float;
  reflectivity: float;
  shp_info: shape_info;
}

val sphere1: shape_attr

val sphere2: shape_attr

val sphere3: shape_attr

val plane1: shape_attr

val shapes: shape_attr array

