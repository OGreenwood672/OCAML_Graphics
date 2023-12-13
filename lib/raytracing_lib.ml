open Vector3;;
open Shapes;;
open Lights;;
open Colour;;
open Globals;;

type ray =
  | Ray of vec3 * vec3;;

let get_ray_origin = function
  | Ray(o, _) -> o;;

let get_ray_direction = function
  | Ray(_, d) -> d;;

type hit = {
  shapeRec: shape_attr;
  distance: float;
  intersection: vec3;
  normal: vec3;
};;

let null_hit = {
  distance=infinity;
  intersection=Vector3(0.,0.,0.);
  normal=Vector3(0.,0.,0.);
  shapeRec={
    colour=0;
    k_d=0.;
    k_s=0.;
    phong_alpha=0.;
    reflectivity=0.;
    shp_info=Sphere({
      center=ref (Vector3(0.,0.,0.));
      radius=0.
    })
  };
};;

let make_pixel_ray x y =
  let x_pos = (wld_x_step -. wld_width) /. 2. +. x *. wld_x_step in
  let y_pos = (wld_y_step -. wld_height) /. 2. +. y *. wld_y_step in
  Ray(!camera_pos, (rotate_about (Vector3(x_pos, y_pos, 1.)) !camera_pos !rotation));;

let sphere_intersection sphere ray =
  match sphere.shp_info with
  | Plane (_) -> failwith "Wrong Shape"
  | Sphere(shp_info) ->
    match ray with
    | Ray(o, d) ->
      let a = dot d d in
      let b = dot (scale d 2.) (subtract o !(shp_info.center)) in
      let c = (dot (subtract o !(shp_info.center)) (subtract o !(shp_info.center))) -. (shp_info.radius *. shp_info.radius) in
      let discriminent = b *. b -. 4. *. a *. c in
      if (discriminent < 0.) then null_hit
      else
        let s1 = (-1. *. b +. (sqrt discriminent)) /. (2. *. a) in
        let s2 = (-1. *. b -. (sqrt discriminent)) /. (2. *. a) in
        if (s1 >= 0. && s1 <= s2) then {shapeRec=sphere; distance=s1; intersection=(add o (scale d s1)); normal=(get_sphere_normal !(shp_info.center) (add o (scale d s1)))}
        else if (s2 >= 0.) then {shapeRec=sphere; distance=s2; intersection=(add o (scale d s2)); normal=(get_sphere_normal !(shp_info.center) (add o (scale d s2)))}
        else null_hit;;

let plane_intersection plane (Ray(origin, direction)) =
  match plane.shp_info with
  | Sphere (_) -> failwith "Wrong Shape"
  | Plane (shp_info) ->
    let s = (dot (subtract !(shp_info.point) origin) shp_info.normal) /. (dot direction shp_info.normal) in
    if (s < 0.) then null_hit
    else {shapeRec=plane; distance=s; intersection=add origin (scale direction s); normal=shp_info.normal}

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
  loop_objects shapes


let illuminate (shp: shape_attr) intersection normal origin =

  let c_diff = map_colour_from_255_to_1 (vec3_of_colour shp.colour) in

  let ambient = (scale c_diff ambience) in (*Ambient*)
  
  let rec loop_pls pls =
    match pls with
    | [] -> Vector3(0., 0., 0.)
    | h :: t ->
      let distance_to_light = (magnitude (subtract h.position intersection)) in
      let illumination = get_illumination_of_pointlight c_diff h.intensity distance_to_light in

      let l = normalise (subtract h.position intersection) in
      let r = normalise (reflect_v1_in_v2 l normal) in
      let v = normalise (subtract origin intersection) in

      let c_spec = map_colour_from_255_to_1 (vec3_of_colour h.colour) in
    
      if ((find_closest_hit (Ray(subtract intersection (scale normal epsilon), l))).distance > distance_to_light) then
        let diffuse = (scale (scale (scale c_diff shp.k_d) illumination) (max 0. (dot normal l))) in
        let specular = (scale (scale (scale c_spec shp.k_s) illumination) ((max 0. (dot r v)) ** shp.phong_alpha)) in
        add (add specular diffuse) (loop_pls t)
      else (loop_pls t);
  in
  (map_colour_from_1_to_255 (add (loop_pls pls) ambient));;

let rec trace ray bounces_left =
  let hit = find_closest_hit ray in
  let direct_illumination = illuminate hit.shapeRec hit.intersection hit.normal (get_ray_origin ray) in
  if (bounces_left = 0 || hit.shapeRec.reflectivity = 0.) then direct_illumination
  else (
    let r = normalise (reflect_v1_in_v2 (scale (get_ray_direction ray) (-1.)) hit.normal) in
    let reflected_ray = Ray(add hit.intersection (scale hit.normal epsilon), r) in
    let reflected_illumination = trace reflected_ray (bounces_left - 1) in
    add (scale direct_illumination (1.0 -. hit.shapeRec.reflectivity)) (scale reflected_illumination hit.shapeRec.reflectivity)
  );;

let tonemap linearRGB =
  let powRGB = power (map_colour_from_255_to_1 (vec3_of_colour linearRGB)) 1.3 in
  let t = (0.5 /. 2.) ** 1.3 in
  let displayRGB = mult powRGB (inverse (add powRGB (Vector3(t, t, t)))) in
  colour_of_vec3 (map_colour_from_1_to_255 (power displayRGB (1. /. 2.2)));;

let render_pixel i j =
  let linearRGB = colour_of_vec3 (trace (make_pixel_ray i j) bounces) in
  tonemap linearRGB;;