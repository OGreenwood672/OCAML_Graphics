open Vector3;;

let px_width = 600
let px_height = 600;;

let movement = 0.5;;
let rotation_step = 0.05;;

let pi = 4.0 *. atan 1.0;;
let epsilon = 0.0001;;

let camera_pos = ref (Vector3(0., 0., 0.));;
let aspect_ratio = (float_of_int px_width) /. (float_of_int px_height)

let rotation = ref (Vector3(0., 0., 0.))

let fov = pi /. 4.0
let wld_width = 2.0 *. tan (fov /. 2.0)
let wld_height = wld_width /. aspect_ratio

let wld_x_step = wld_width /. (float_of_int px_width)
let wld_y_step = wld_height /. (float_of_int px_height);;

let ambience = 0.01;;
let bounces = 1;;