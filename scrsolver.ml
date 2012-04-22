module Scr_Solver = struct
    open Scr_Board
    let min_length = 2
    let create_dict =
        Hashtbl.create 0 (*TODO should this be initialized to a different size? *)

    let read_file_to_dict chan dict =
        let rec loop rlst =
            let word, eof =
                try
                    (input_line chan), false
                with
                | End_of_file -> "", true
            in 
            if not eof
            then
                let key = 
                    try 
                        String.sub word 0 min_length
                    with
                        Invalid_argument "String.sub" -> word
                in
                    Hashtbl.add dict key word;
                    loop (word :: rlst);
            else
                List.rev rlst
        in
        loop []
    ;;

    let load_dictionary dict = 
        let in_channel = 
            open_in "/usr/share/dict/words"
        in
        read_file_to_dict in_channel dict

    let get_words dict key =
        Hashtbl.find_all dict key

    let is_valid_pos (x,y) =
        if (x > pred row) || (x < 0) || (y > pred col) || (y < 0)
        then false
        else true

    let is_valid_word dict word =  
        try
            List.mem word (Hashtbl.find_all dict (String.sub word 0 min_length))
        with
            Invalid_argument "String.sub" -> false

   (*FIXME the below is currently broken for actual words *)
    let is_valid_subword dict subword =
      if String.length subword >= 2
      then
        if is_valid_word dict subword
        then
          (* do something differently here *)
          List.exists (fun a -> (String.sub a 0 (String.length subword)) = subword) (Hashtbl.find_all dict (String.sub subword 0 min_length))
        else
          List.exists (fun a -> (String.sub a 0 (String.length subword)) = subword) (Hashtbl.find_all dict (String.sub subword 0 min_length))
      else
        false

    let rec recurse board (x,y) =
        let rec loop word (a, b) =
            if is_valid_pos (a, b)
            then 
                let new_word = 
                    word ^ String.make 1 (get_char board (a, b));
                in
                    loop new_word (a + 1, b + 1)
            else
                loop word (a - 1, b - 1)

        in
            loop "" (x, y)

end
