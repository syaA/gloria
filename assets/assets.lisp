(
 (texture char01-sheet "vx_chara01_a.tga")
 (sprite char1 char01-sheet 32 48 3 4 :init-x 96 :init-y 0 :base-x 15 :base-y 44)
 (sprite-pattern-animation char1-s (char1-1 char1-2 char1-1 char1-0) 10 t)
 (sprite-pattern-animation char1-w (char1-4 char1-5 char1-4 char1-3) 10 t)
 (sprite-pattern-animation char1-e (char1-7 char1-8 char1-7 char1-6) 10 t)
 (sprite-pattern-animation char1-n (char1-10 char1-11 char1-10 char1-9) 10 t)
 (animation-set char1-set char1-n char1-s char1-e char1-w)
 (texture weapon-sheet "weapon.tga")
 (sprite sword weapon-sheet 32 32 1 1 :base-x 15 :base-y 47 :rot-center-x 15 :rot-center-y 47)
 (key-frame-animation weapon-swing-rot-n (0 -55 10 55) nil)
 (key-frame-animation weapon-swing-trans-n (0 #(2 -16 0)) t)
 (rt-animation weapon-swing-n weapon-swing-rot-n weapon-swing-trans-n)
 (key-frame-animation weapon-swing-rot-s (0 -125 10 -235) nil)
 (key-frame-animation weapon-swing-trans-s (0 #(2 -16 0)) t)
 (rt-animation weapon-swing-s weapon-swing-rot-s weapon-swing-trans-s)
 (key-frame-animation weapon-swing-rot-e (0 35 10 145) nil)
 (key-frame-animation weapon-swing-trans-e (0 #(2 -12 0)) t)
 (rt-animation weapon-swing-e weapon-swing-rot-e weapon-swing-trans-e)
 (key-frame-animation weapon-swing-rot-w (0 -35 10 -145) nil)
 (key-frame-animation weapon-swing-trans-w (0 #(2 -12 0)) t)
 (rt-animation weapon-swing-w weapon-swing-rot-w weapon-swing-trans-w)
 (animation-set weapon-swing-set weapon-swing-n weapon-swing-s weapon-swing-e weapon-swing-w)
 )

