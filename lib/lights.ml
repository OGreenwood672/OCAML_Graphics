open Vector3;;
open Colour;;
open Globals;;

type point_light = {
  position: vec3;
  colour: int;
  intensity: float;
};;

let pl1 = {
  position = Vector3(0., 2., 3.);
  colour = (rgb 255 255 255);
  intensity = 100.;
};;

let pl2 = {
  position = Vector3(0., -2., 8.);
  colour = (rgb 255 255 255);
  intensity = 100.;
};;

let pl3 = {
  position = Vector3(0.8, -2., 19.);
  colour = (rgb 255 255 255);
  intensity = 10.;
};;

let pls = [pl1; pl2; pl3];;

let get_illumination_of_pointlight c_diff intensity distance =
  magnitude (scale c_diff (intensity /. (pi *. 4. *. distance *. distance)));;