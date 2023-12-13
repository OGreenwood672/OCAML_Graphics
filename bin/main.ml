open Graphics;;
open Vector3;;
open Colour;;
open Raytracing_lib;;
open Globals;;
open Physics;;

open_graph (" " ^ (string_of_int px_width) ^ "x" ^ (string_of_int px_height));;


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
      else if key = 'v' then rotation := add !rotation (Vector3(0., rotation_step, 0.))
      else if key = 'p' then move 2. phyx_objs; do_collisions phyx_objs;

      remember_mode true;
      draw_image (draw_frame ()) 0 0;
      remember_mode false;
  done;
with Exit -> ()
