open Vector3;;
open Globals;;
open Shapes;;

type physics_body = {
  velocity: vec3 ref;
  mass: float;
  e: float;
  shp: shape_info;
  is_fixed: bool;
};;

let make_rigid_body shp mass e is_fixed =
  {
    velocity = ref (Vector3(0., 0.05, 0.));
    mass = mass;
    e = e;
    shp = shp;
    is_fixed = is_fixed;
  };;

let collide velocity normal e =
  let normal_component = scale normal (dot velocity normal) in
  let constant_component = subtract velocity normal_component in
  add (scale normal_component (-. e)) constant_component

let distance_to_plane point plane_info =
  dot (subtract point !(plane_info.point)) plane_info.normal

let sphere_plane_collision sphere_phyx plane_phyx sphere_info plane_info =
  let distance = distance_to_plane !(sphere_info.center) (plane_info) in
  let overlap = sphere_info.radius -. abs_float distance in
  if overlap > 0.0 then (
    let direction = if distance > 0.0 then 1.0 else -1.0 in
    sphere_info.center := (
      (add 
        !(sphere_info.center)
        (scale plane_info.normal (overlap *. direction))
      )
    );
    sphere_phyx.velocity := (collide
      !(sphere_phyx.velocity)
      plane_info.normal
      ((plane_phyx.e +. sphere_phyx.e) /. 2.)  
    )
  );;

let rec move ms_since_update = function
  | [] -> ()
  | phyx_obj :: t ->
    if (phyx_obj.is_fixed) then ()
    else (
      phyx_obj.velocity := add !(phyx_obj.velocity) (scale gravity ms_since_update);
      match phyx_obj.shp with
      | Plane(plane_info) ->
        plane_info.point := (add
          !(plane_info.point)
          (scale !(phyx_obj.velocity) ms_since_update)
        );
        move ms_since_update t
      | Sphere(sphere_info) ->
        sphere_info.center := (add 
          !(sphere_info.center)
          (scale !(phyx_obj.velocity) ms_since_update)
        );
        move ms_since_update t
    );;

let rec do_collisions = function
  | [] -> ()
  | h :: t ->
    let rec do_collisions_subloop = function
      | [] -> do_collisions t
      | h2 :: t2 ->
        match h.shp, h2.shp with
        | Sphere(sphere_info), Plane(plane_info) ->
          sphere_plane_collision h h2 sphere_info plane_info;
          do_collisions_subloop t2;
        | Plane(plane_info), Sphere(sphere_info) ->
          sphere_plane_collision h h2 sphere_info plane_info;
          do_collisions_subloop t2;
        | Sphere(_), Sphere(_) -> do_collisions_subloop t2;
        | Plane(_), Plane(_) -> do_collisions_subloop t2;
    in
    do_collisions_subloop t

let phyx_objs = [
  make_rigid_body sphere1.shp_info 1. 0.5 false;
  make_rigid_body sphere2.shp_info 1. 0.5 false;
  make_rigid_body plane1.shp_info 1. 1. true;
]