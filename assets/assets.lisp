(
 (texture char01-sheet "vx_chara01_a.tga")
 (sprite char1 char01-sheet 32 48 3 4 96 0)
 (sprite-pattern-animation char1-s (char1-1 char1-2 char1-1 char1-0) 10 t)
 (sprite-pattern-animation char1-w (char1-4 char1-5 char1-4 char1-3) 10 t)
 (sprite-pattern-animation char1-e (char1-7 char1-8 char1-7 char1-6) 10 t)
 (sprite-pattern-animation char1-n (char1-10 char1-11 char1-10 char1-9) 10 t)
 (sprite-animation-set char1-set char1-n char1-s char1-e char1-w)
 (texture weapon-sheet "weapon.tga")
 (sprite sword weapon-sheet 32 32 1 1)
 )

