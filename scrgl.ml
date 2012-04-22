module Scr_GL = struct
    open GL
    open Glu
    open Glut

    let board = Array.make_matrix 4 4 0 ;;   (* amount of color for each square *)


    (*  The nine squares are drawn.  In selection mode, each 
     *  square is given two names:  one for the row and the 
     *  other for the column on the grid.  The color of each 
     *  square is determined by its position on the grid, and 
     *  the value in the board array.
     *)
    let drawSquares ~mode =
        for i = 0 to pred 4 do
            if mode = GL_SELECT then
                glLoadName i;

                for j = 0 to pred 4 do
                    if mode = GL_SELECT then
                        glPushName j;

                        glColor3 ((float i) /. 4.0) ((float j) /. 4.0)
                        ((float board.(i).(j)) /. 4.0);

                        glRecti i j (i+1) (j+1);

                        if mode = GL_SELECT then
                            glPopName ();
                done;
                done;
    ;;


    (*  processHits prints out the contents of the 
     *  selection array.
     *)
    let processHits ~hits ~buffer =
        let ii, jj = ref 0, ref 0 in
        let c = ref 0 in
        Printf.printf "hits = %d\n" hits;
        for i = 0 to pred hits do	  (*  for each hit  *)
            let names = select_buffer_get buffer !c in
            Printf.printf " number of names for this hit = %d\n" names; incr c;
            incr c;  (* z1 *)
            incr c;  (* z2 *)
            Printf.printf "   names are ";
            for j = 0 to pred names do    (*  for each name *)
                Printf.printf "%d " (select_buffer_get buffer !c);
                if j = 0 then  (*  set row and column  *)
                    ii := (select_buffer_get buffer !c)
                else if j = 1 then
                    jj := (select_buffer_get buffer !c);
                    incr c;
        done;
        print_newline();
        board.(!ii).(!jj) <- (board.(!ii).(!jj) + 1) mod 4;
            done;
    ;;


    (*  pickSquares sets up selection mode, name stack, 
     *  and projection matrix for picking.  Then the 
     *  objects are drawn.
     *)
    let buffer_size = 512 ;;

    let pickSquares ~button ~state ~x ~y =

        if button = GLUT_LEFT_BUTTON && state = GLUT_DOWN
        then begin
            let select_buffer = new_select_buffer buffer_size in
            at_exit(fun () -> free_select_buffer select_buffer);

            let viewport = glGetInteger4 Get.GL_VIEWPORT in
            let _,_,_, viewport_h = viewport in

            glSelectBuffer buffer_size select_buffer;
            ignore(glRenderMode GL_SELECT);

            glInitNames();
            glPushName 0;

            glMatrixMode GL_PROJECTION;
            glPushMatrix();
            glLoadIdentity();
            (* create 5x5 pixel picking region near cursor location *)
            gluPickMatrix (float x) (float(viewport_h - y)) 5.0 5.0 viewport;
            gluOrtho2D 0.0 4.0 0.0 4.0;
            drawSquares GL_SELECT;

            glMatrixMode GL_PROJECTION;
            glPopMatrix();
            glFlush();

            let hits = glRenderMode GL_RENDER in
            processHits ~hits ~buffer:select_buffer;
            glutPostRedisplay();
        end
    ;;


    let display() =
        glClear [GL_COLOR_BUFFER_BIT];
        drawSquares GL_RENDER;
        glFlush();
    ;;


    let reshape ~width ~height =
        glViewport 0 0 width height;
        glMatrixMode GL_PROJECTION;
        glLoadIdentity();
        gluOrtho2D 0.0 4.0 0.0 4.0;
        glMatrixMode GL_MODELVIEW;
        glLoadIdentity();
    ;;


    let keyboard ~key ~x ~y =
        match key with
        | '\027' -> exit 0
        | _ -> ()
    ;;


    let () =
        ignore(glutInit Sys.argv);
        glutInitDisplayMode [GLUT_SINGLE; GLUT_RGB];
        glutInitWindowSize 100 100;
        glutInitWindowPosition 100 100;
        ignore(glutCreateWindow Sys.argv.(0));
        (* Clear color value for every square on the board *)
        glClearColor 0.0 0.0 0.0 0.0;
        glutReshapeFunc ~reshape;
        glutDisplayFunc ~display; 
        glutMouseFunc ~mouse:pickSquares;
        glutKeyboardFunc ~keyboard;
        glutMainLoop();
    ;;
end
