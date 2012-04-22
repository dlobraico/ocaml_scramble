include Random

module Scr_Board = struct
    Random.self_init

    type cell = Letter of char | Empty
    type board = cell array array
    type move = (int * int)
    let col = 4 and row = 4

    let letters = [|'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'|]
    let random_letter arr = 
        let idx = Random.int (Array.length arr) in
        Letter (Array.get arr idx)

    let fill_letters mat = 
        for r = row - 1 downto 0 do 
            for c = 0 to col - 1 do
                mat.(r).(c) <- random_letter letters
            done
            done

    let fill_board_from_arr mat arr = 
        let num_elems = (row * col) in
        for i = 0 to num_elems do
            for r = row - 1 downto 0 do
                for c = 0 to col - 1 do
                    mat.(r).(c) <- Letter (Array.get arr i)
                done
            done
        done

    let get_char board (x,y) =
        match board.(x).(y) with
          Letter c -> c
        | Empty -> 
                raise (Invalid_argument "get_char")


    let empty_board () = Array.make_matrix row col Empty 
    let init_board board = fill_letters board
end
