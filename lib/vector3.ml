

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
