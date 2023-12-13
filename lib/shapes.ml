open Vector3;;
open Colour;;

type ray =
  | Ray of vec3 * vec3;;

let get_sphere_normal center pos = normalise (subtract pos center);;

type sphere_info = {
  center: vec3 ref;
  radius: float;
};;

type plane_info = {
  normal: vec3;
  point: vec3 ref;
};;

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
};;

let sphere1: shape_attr = {
  colour = (rgb 255 0 0);
  k_d = 5.;
  k_s = 1.4;
  phong_alpha = 10.;
  reflectivity = 0.3;
  shp_info=Sphere({
    center = ref (Vector3(-2., -2., 20.));
    radius=1.4;
  });
};;

let sphere2: shape_attr = {
  colour = (rgb 0 255 0);
  k_d = 5.;
  k_s = 1.2;
  phong_alpha = 10.;
  reflectivity = 0.3;
  shp_info=Sphere({
    center = ref (Vector3(0., 3., 18.5));
    radius = 2.;
  });
};;

let plane1: shape_attr = {
  colour = (rgb 0 0 255);
  k_d = 3.;
  k_s = 1.2;
  phong_alpha = 10.;
  reflectivity = 0.;
  shp_info=Plane({
    point = ref (Vector3(5., 0., 2.));
    normal = Vector3(1., 0., 0.);
  });
};;

let shapes = [sphere1; sphere2; plane1];;
