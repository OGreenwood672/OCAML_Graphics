open Graphics

let px_width = 600
let px_height = 600;;

open_graph (" " ^ (string_of_int px_width) ^ "x" ^ (string_of_int px_height));;

let pi = 4.0 *. atan 1.0;;

(*Vectors*)
type vec3 =
  | Vector3 of float * float * float;;

let dot v1 v2 =
  match v1, v2 with
  | Vector3(a, b, c), Vector3(x, y, z) ->
    a *. x +. b *. y +. c *. z;;

let add v1 v2 =
  match v1, v2 with
  | Vector3(a, b, c), Vector3(x, y, z) ->
    Vector3(a +. x, b +. y, c +. z);;

let subtract v1 v2 =
  match v1, v2 with
  | Vector3(a, b, c), Vector3(x, y, z) ->
    Vector3(a -. x, b -. y, c -. z);;

let scale v1 scaler =
  match v1 with
  | Vector3(a, b, c) ->
    Vector3(a *. scaler, b *. scaler, c *. scaler);;

let mult v1 v2 =
  match v1, v2 with
  | Vector3(a, b, c), Vector3(x, y, z) ->
    Vector3(a *. x, b *. y, c *. z);;

let div v1 v2 =
  match v1, v2 with
  | Vector3(a, b, c), Vector3(x, y, z) ->
    Vector3(a /. x, b /. y, c /. z);;
    
let reflect_v1_in_v2 v1 v2 =
  subtract (scale v2 (2. *. (dot v1 v2))) v1;;

let magnitude v =
  match v with
  | Vector3(x, y, z) -> sqrt (x *. x +. y *. y +. z *. z);;

let normalise v =
  let mag = magnitude v in
  match v with
  | Vector3(x, y, z) ->
    Vector3(x /. mag, y /. mag, z /. mag);;

let power v exp =
  match v with
  | Vector3(x, y, z) -> Vector3(x ** exp, y ** exp, z ** exp);;

let inverse v =
  match v with
  | Vector3(x, y, z) -> Vector3(1. /. x, 1. /. y, 1. /. z);;

let rotate_about v origin (Vector3(x_rad, y_rad, z_rad)) =
  match subtract v origin with
  | Vector3(x, y, z) ->
    add origin (Vector3(
      x *. (cos y_rad) *. (cos z_rad) -. y *. (cos y_rad) *. (sin z_rad) +. z *. (sin y_rad),
      x *. (cos x_rad) *. (sin z_rad) +. y *. (cos x_rad) *. (cos z_rad) -. z *. (sin x_rad) *. (cos z_rad),
      x *. (sin x_rad) *. (sin y_rad) +. y *. (sin x_rad) *. (cos y_rad) +. z *. (cos x_rad) *. (cos y_rad)
    ));;

(*Colour*)
let vec3_of_colour c =
  Vector3((float_of_int ((c lsr 16) land 0xFF)), (float_of_int ((c lsr 8) land 0xFF)), (float_of_int (c land 0xFF)))

let colour_of_vec3 v =
  match v with
  | Vector3(r, g, b) -> rgb (int_of_float r) (int_of_float g) (int_of_float b);;

let map_colour_from_255_to_1 v =
  div v (Vector3(255., 255., 255.))

let map_colour_from_1_to_255 v =
  mult v (Vector3(255., 255., 255.))

(*CAMERA*)
let camera_pos = ref (Vector3(0., 0., 0.));;
let aspect_ratio = (float_of_int px_width) /. (float_of_int px_height)

let rotation = ref (Vector3(0., 0., 0.))

let fov = pi /. 4.0
let wld_width = 2.0 *. tan (fov /. 2.0)
let wld_height = wld_width /. aspect_ratio

let wld_x_step = wld_width /. (float_of_int px_width)
let wld_y_step = wld_height /. (float_of_int px_height);;

(*Shapes*)
let get_sphere_normal center pos = normalise (subtract pos center);;

type sphere_info = {
  center: vec3;
  radius: float;
};;

type plane_info = {
  normal: vec3;
  point: vec3;
}

type shape_info =
  | Sphere of sphere_info
  | Plane of plane_info

type shape_attr = {
  colour: int;
  shp_info: shape_info;
  k_d: float;
  k_s: float;
  phong_alpha: float;
}

let sphere1: shape_attr = {
  colour = (rgb 255 0 0);
  k_d = 5.;
  k_s = 1.4;
  phong_alpha = 10.;
  shp_info=Sphere({
    center = Vector3(0., 1., 4.);
    radius=0.5;
  });
};;

let sphere2: shape_attr = {
  colour = (rgb 0 255 0);
  k_d = 5.;
  k_s = 1.2;
  phong_alpha = 10.;
  shp_info=Sphere({
    center = Vector3(0., -1., 4.);
    radius=0.5;
  });
};;

let plane1: shape_attr = {
  colour = (rgb 0 0 255);
  k_d = 3.;
  k_s = 1.2;
  phong_alpha = 10.;
  shp_info=Plane({
    point = Vector3(1., 0., 2.);
    normal=Vector3(1., 0., 0.);
  });
};;

let shapes = [sphere1; sphere2; plane1];

(*Lights*)
type point_light = {
  position: vec3;
  colour: int;
  intensity: float;
};;

let pl1 = {
  position = Vector3(0., 0., 3.);
  colour = (rgb 255 255 255);
  intensity = 1.;
};;

let pl2 = {
  position = Vector3(-1., 0., 3.8);
  colour = (rgb 255 255 255);
  intensity = 1.;
};;

let pls = [pl1; pl2];;

let get_illumination_of_pointlight c_diff intensity distance =
  magnitude (scale c_diff (intensity /. (pi *. 4. *. distance *. distance)));;

(*Ray Tracing*)
type ray =
  | Ray of vec3 * vec3;;

let get_ray_origin = function
  | Ray(o, _) -> o;;

type hit = {
  shapeRec: shape_attr;
  distance: float;
  intersection: vec3;
  normal: vec3;
} 

let make_pixel_ray x y =
  let x_pos = (wld_x_step -. wld_width) /. 2. +. x *. wld_x_step in
  let y_pos = (wld_y_step -. wld_height) /. 2. +. y *. wld_y_step in
  Ray(!camera_pos, (rotate_about (Vector3(x_pos, y_pos, 1.)) !camera_pos !rotation));;


let null_hit = {
  distance=infinity;
  intersection=Vector3(0.,0.,0.);
  normal=Vector3(0.,0.,0.);
  shapeRec={
    colour=0;
    k_d=0.;
    k_s=0.;
    phong_alpha=0.;
    shp_info=Sphere({
      center=Vector3(0.,0.,0.);
      radius=0.
    })
  };
};;

let sphere_intersection sphere ray =
  match sphere.shp_info with
  | Plane (_) -> failwith "Wrong Shape"
  | Sphere(shp_info) ->
    match ray with
    | Ray(o, d) ->
      let a = dot d d in
      let b = dot (scale d 2.) (subtract o shp_info.center) in
      let c = (dot (subtract o shp_info.center) (subtract o shp_info.center)) -. (shp_info.radius *. shp_info.radius) in
      let discriminent = b *. b -. 4. *. a *. c in
      if (discriminent < 0.) then null_hit
      else
        let s1 = (-1. *. b +. (sqrt discriminent)) /. (2. *. a) in
        let s2 = (-1. *. b -. (sqrt discriminent)) /. (2. *. a) in
        if (s1 >= 0. && s1 <= s2) then {shapeRec=sphere; distance=s1; intersection=(add o (scale d s1)); normal=(get_sphere_normal shp_info.center (add o (scale d s1)))}
        else if (s2 >= 0.) then {shapeRec=sphere; distance=s2; intersection=(add o (scale d s2)); normal=(get_sphere_normal shp_info.center (add o (scale d s2)))}
        else null_hit;;

let plane_intersection plane (Ray(origin, direction)) =
  match plane.shp_info with
  | Sphere (_) -> failwith "Wrong Shape"
  | Plane (shp_info) ->
    let s = (dot (subtract shp_info.point origin) shp_info.normal) /. (dot direction shp_info.normal) in
    if (s < 0.) then null_hit
    else {shapeRec=plane; distance=s; intersection=add origin (scale direction s); normal=shp_info.normal}

let illuminate (shp: shape_attr) intersection normal origin =

  let c_diff = map_colour_from_255_to_1 (vec3_of_colour shp.colour) in

  let ambient = (scale c_diff 0.05) in (*Ambient*)
  
  let rec loop_pls pls =
    match pls with
    | [] -> Vector3(0., 0., 0.)
    | h :: t ->
      let distance = (magnitude (subtract h.position intersection)) in
      let illumination = get_illumination_of_pointlight c_diff h.intensity distance in

      let l = normalise (subtract h.position intersection) in
      let r = normalise (reflect_v1_in_v2 l normal) in
      let v = normalise (subtract origin intersection) in

      let c_spec = map_colour_from_255_to_1 (vec3_of_colour h.colour) in

      let diffuse = (scale (scale (scale c_diff shp.k_d) illumination) (max 0. (dot normal l))) in
      let specular = (scale (scale (scale c_spec shp.k_s) illumination) ((max 0. (dot r v)) ** shp.phong_alpha)) in
      add (add specular diffuse) (loop_pls t)
  in
  colour_of_vec3 (map_colour_from_1_to_255 (add (loop_pls pls) ambient));;

let find_closest_hit ray =
  let compare_and_return s1 s2 = if s1.distance < s2.distance then s1 else s2 in
  let rec loop_objects objects =
    match objects with
    | [] -> null_hit
    | h :: t ->
      match h.shp_info with
      | Sphere (_) -> compare_and_return (sphere_intersection h ray) (loop_objects t)
      | Plane (_) -> compare_and_return (plane_intersection h ray) (loop_objects t)
  in
  let hit = loop_objects shapes in
  if (hit.distance = infinity) then null_hit
  else hit;;

let trace ray =
  let hit = find_closest_hit ray in
  illuminate hit.shapeRec hit.intersection hit.normal (get_ray_origin ray);;

let tonemap linearRGB =
  let powRGB = power (map_colour_from_255_to_1 (vec3_of_colour linearRGB)) 1.3 in
  let t = (0.5 /. 2.) ** 1.3 in
  let displayRGB = mult powRGB (inverse (add powRGB (Vector3(t, t, t)))) in
  colour_of_vec3 (map_colour_from_1_to_255 (power displayRGB (1. /. 2.2)));;
  
let render_pixel i j =
  let linearRGB = trace (make_pixel_ray i j) in
  tonemap linearRGB;;

(*FUNCTION*)
let draw_frame = fun () ->
  let img = Array.make_matrix px_width px_height (rgb 0 0 0) in
  let i = ref 0 in
  while (!i < px_height) do
    let j = ref 0 in
    while (!j < px_width) do
      img.(!i).(!j) <- render_pixel (float_of_int !i) (float_of_int !j);
      j := !j + 1;
    done;
    i := !i + 1;
  done;
  make_image img;;

remember_mode true;
draw_image (draw_frame ()) 0 0;
remember_mode false;

let movement = 0.5 in
let rotation_step = 0.05 in
try
  while true do
    let st = wait_next_event [Key_pressed] in
    synchronize ();
    if st.keypressed then
      let key = st.key in
      if key = 'q' then raise Exit
      else if key = 'a' then camera_pos := add !camera_pos (Vector3(0., -1. *. movement, 0.))
      else if key = 'd' then camera_pos := add !camera_pos (Vector3(0., movement, 0.))
      else if key = 'w' then camera_pos := add !camera_pos (Vector3(-1. *. movement, 0., 0.))
      else if key = 's' then camera_pos := add !camera_pos (Vector3(movement, 0., 0.))
      else if key = 'z' then camera_pos := add !camera_pos (Vector3(0., 0., -1. *. movement))
      else if key = 'x' then camera_pos := add !camera_pos (Vector3(0., 0., movement))
      else if key = 'c' then rotation := add !rotation (Vector3(0., -1. *. rotation_step, 0.))
      else if key = 'v' then rotation := add !rotation (Vector3(0., rotation_step, 0.));
      
      remember_mode true;
      draw_image (draw_frame ()) 0 0;
      remember_mode false;
  done;
with Exit -> ()
