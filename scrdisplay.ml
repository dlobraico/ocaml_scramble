module Scr_Display = struct
    open Scr_Board
    type board = Scr_Board.board
    type move = Scr_Board.move

    let ask s = 
        print_endline s;
        print_string "yes/no? ";
        read_line() = "yes"

    let print_game mat = 
        for l = row - 1 downto 0 do
            for c = 0 to col - 1 do
                match mat.(l).(c) with
                Letter x -> print_string (String.make 1 x ^ " ")
              | Empty    -> print_string ". "
            done; 
            print_newline ()
            done; 
            print_newline () 
end
