open Vector3;;

let rgb r g b = (r lsl 16) + (g lsl 8) + b;;

let vec3_of_colour c =
  Vector3((float_of_int ((c lsr 16) land 0xFF)), (float_of_int ((c lsr 8) land 0xFF)), (float_of_int (c land 0xFF)))

let colour_of_vec3 v =
  match v with
  | Vector3(r, g, b) -> rgb (int_of_float r) (int_of_float g) (int_of_float b);;

let map_colour_from_255_to_1 v =
  div v (Vector3(255., 255., 255.))

let map_colour_from_1_to_255 v =
  mult v (Vector3(255., 255., 255.))