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
  intensity = 1.;
};;

let pl2 = {
  position = Vector3(0., -2., 4.);
  colour = (rgb 255 255 255);
  intensity = 1.;
};;

let pls = [pl1; pl2];;

let get_illumination_of_pointlight c_diff intensity distance =
  magnitude (scale c_diff (intensity /. (pi *. 4. *. distance *. distance)));;